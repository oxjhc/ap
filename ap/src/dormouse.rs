extern crate hyper;
extern crate untrusted;
extern crate ring;
extern crate get_if_addrs;

use std::{thread, time};
use std::net::UdpSocket;
use std::sync::Mutex;
use std::mem::transmute;
use std::io::Read;

use self::hyper::server::{Handler, Request, Response};
use self::hyper::client::Client;
use self::hyper::status::StatusCode;
use self::hyper::uri::RequestUri;
use self::hyper::header;

use self::untrusted::Input;

use self::ring::{agreement, rand, pbkdf2, aead};
use self::ring::rand::SecureRandom;
use self::ring::aead::SealingKey;

use openssl::hash::MessageDigest as MD;

use protobuf;
use protobuf::Message;

use self::get_if_addrs::{get_if_addrs as ifaces, IfAddr};

use messages::{SignedProofReq,
               ProofResp, SignedProofResp,
               VaultMsg, SignedVaultMsg,
               LocnProof, SignedLocnProof};
use vault::make_vault;
use crypto::PubKey;
use config::Config;

static mut SEQID: i64 = 0;

macro_rules! p2b {
  ($x:expr) => {
    $x.write_to_bytes().unwrap().as_slice()
  }
}

macro_rules! printhex {
  ($x:expr) => {
    for byte in $x {
      print!("{:02X}", byte);
    }
    println!();
  }
}

pub struct Dormouse {
  prflock: Mutex<Option<LocnProof>>,
  vidlock: Mutex<Option<Vec<u8>>>,
  pub cfg: Config
}

impl Dormouse {
  pub fn new() -> Dormouse {
    let prflock = Mutex::new(None);
    let vidlock = Mutex::new(None);
    let cfg = Config::new().unwrap();

    Dormouse::spawn_pinger(cfg.ping_port);

    //printhex!(cfg.key.pub_to_der().unwrap());
    Dormouse{prflock, vidlock, cfg}
  }

  fn ping(sock: &UdpSocket, addr: String) {
    // convert SEQID to a byte array
    let data : [u8; 8] = unsafe { transmute(SEQID.to_be()) };
    match sock.send_to(&data, &addr) {
      Ok(n) => if n == 8 {
        println!("pinged {} with seqid {}", addr,
                 unsafe { transmute::<[u8; 8], i64>(data) }); },
      Err(err) => println!("failed to send ping: {}", err)
    }
  }

  fn spawn_pinger(port: i64) {
    // this... should be modifiable at runtime.
    // buuuuuuuut I'm lazy
    let pinger = UdpSocket::bind(format!("0.0.0.0:{}", port)).unwrap();
    pinger.set_broadcast(true).unwrap();

    thread::spawn(move || {
      let rand = rand::SystemRandom::new();
      loop {
        thread::sleep(time::Duration::from_millis(300));
        for iface in ifaces().unwrap() {
          if iface.name.starts_with("p2p") {
            match iface.addr {
              IfAddr::V4(addr) => match addr.broadcast {
                Some(brip) => {
                  unsafe { rand.fill(transmute::<&mut i64, &mut [u8;8]>(&mut SEQID)).unwrap(); };
                  Dormouse::ping(&pinger, format!("{}:{}", brip, port));
                },
                _ => unsafe { SEQID = 0; }
              },
              _ => unsafe { SEQID = 0; }
            }
          }
        }
      }
    });
  }

  pub fn ping_server(&self) {
    let client = Client::new();
    let resp = client.get(format!("{}{}", self.cfg.server_url, "/ping").as_str())
      .send().unwrap();
    if resp.status == hyper::Ok {
      println!("server successfully pinged");
      // done.
    } else {
      println!("the server is down :(.");
    }
  }

  pub fn server_test(&self) {
    let uid = vec![0x00];
    let unonce = vec![0x11];
    let nonce = vec![0x22];
    let time = time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap().as_secs();

    let mut prf = LocnProof::new();

    prf.set_uid(uid.clone());
    prf.set_unonce(unonce.clone());
    prf.set_apid(self.cfg.key.pub_to_der().unwrap());
    prf.set_apnonce(nonce.clone());

    let mut vault_key = self.gen_proof(uid.as_slice(), unonce.as_slice(),
                                       nonce.as_slice(), time).unwrap();

    let rand = rand::SystemRandom::new();
    let ekey = agreement::EphemeralPrivateKey::generate(&agreement::ECDH_P256, &rand).unwrap();
    let mut pubept = vec![0u8; ekey.public_key_len()];
    ekey.compute_public_key(&mut pubept).unwrap();
    let pubekey = PubKey::from_point(pubept.as_slice()).unwrap();
    let vid = PubKey::from_file(&self.cfg._t_vid_file).unwrap().to_point_bytes().unwrap();
    let vid = Input::from(vid.as_slice());

    let key = agreement::agree_ephemeral(
      ekey, &agreement::ECDH_P256, vid, ring::error::Unspecified,
      |_key_material| {
        let mut key = vec![0; 32]; //digest::SHA256.output_len
        pbkdf2::derive(&pbkdf2::HMAC_SHA256, 30, prf.get_apnonce(),
                       _key_material, key.as_mut_slice());
        //printhex!(&key)
        SealingKey::new(&aead::AES_256_GCM, key.as_slice())
      }).unwrap();

    vault_key.extend(vec![0; 16]);
    vault_key = match aead::seal_in_place(&key, &[0; 12], &[], vault_key.as_mut_slice(), 16) {
      Ok(len) => vault_key[..len].to_vec(),
      Err(e) => panic!("Error sealing vault key: {}", e)
    };

    prf.set_vault_key(vault_key);
    prf.set_ekey(pubekey.pub_to_der().unwrap());
    prf.set_time(time);

    let sig = self.cfg.key.sign(MD::sha256(), p2b!(prf)).unwrap();
    let mut sgn_prf = SignedLocnProof::new();
    sgn_prf.set_locnproof(prf.clone());
		sgn_prf.set_sig(sig);

    let client = Client::new();
    println!("sending proof to server");
    //printhex!(p2b!(sgn_prf));
    let mut resp =
      match client.post(format!("{}{}", self.cfg.server_url, "/proof").as_str())
      .header(header::ContentType("application/x-protobuf".parse().unwrap()))
      .body(p2b!(sgn_prf))
      .send() {
        Ok(res) => res,
        Err(err) => {
          panic!("Error sending proof to server: {}", err);
        }
      };

    if resp.status == hyper::Ok {
      println!("vault successfully sent, response:");
      println!("{:?}", resp);
      // done.
    } else {
      println!("error sending vault: {}", resp.status);
      let mut res = String::new();
      resp.read_to_string(&mut res).unwrap();
      println!("resp is:\n{:?}", res);
    }
  }

  fn gen_proof(&self, uid: &[u8], unonce: &[u8], nonce: &[u8], time: u64) -> Result<Vec<u8>, String> {
    println!("starting gen_proof");
    let locn_tag = vec![ 243, 122, 33, 214 ];
    let key_sz = 10;
    let (vault, vault_key) = make_vault(locn_tag, key_sz, 100);

    let mut msg = VaultMsg::new();
    msg.set_vault(vault);
    msg.set_uid(uid.to_vec());
    msg.set_unonce(unonce.to_vec());
    msg.set_apid(self.cfg.key.pub_to_der().unwrap());
    msg.set_apnonce(nonce.to_vec());
    msg.set_time(time);

    let sig = self.cfg.key.sign(MD::sha256(), p2b!(msg)).unwrap();
    let mut sgn_msg = SignedVaultMsg::new();
    sgn_msg.set_vault_msg(msg);
    sgn_msg.set_sig(sig);

    let client = Client::new();
    println!("sending vault to server");
    //printhex!(p2b!(sgn_msg));
    let resp =
      match client.post(format!("{}{}", self.cfg.server_url, "/vault").as_str())
      .header(header::ContentType("application/x-protobuf".parse().unwrap()))
      .body(p2b!(sgn_msg))
      .send() {
        Ok(res) => res,
        Err(err) => {
          println!("Error sending vault to server: {}", err);
          return Err("Couldn't send vault to server".to_string());
        }
      };

    if resp.status == hyper::Ok {
      println!("vault successfully sent");
      // done.
    } else {
      println!("error sending vault: {}", resp.status);
    }

    Ok(unsafe {transmute(vault_key)})
  }
}

impl Handler for Dormouse {
  fn handle(&self, mut req: Request, mut res: Response) {
    macro_rules! reply {
      ($c:expr, $x:expr) => {
        *res.status_mut() = $c;
        match res.send($x) {
          Ok(_) => (),
          Err(e) => println!("Failed to send response: {}", e)
        }
      }
    }

    macro_rules! etry {
      ($x:expr) => {
        match $x {
          Ok(v) => v,
          Err(e) => {
            println!("Internal error: {}", e);
            reply!(StatusCode::InternalServerError, b"Something broke :'(");
            return;
          }
        }
      }
    }
    match req.uri.clone() {
      RequestUri::AbsolutePath(ref path) => match (&req.method, &path[..]) {
        (&hyper::Post, "/proof_req") => {
          println!("received proof req");
          // lock proof lock, ensuring only one proof generation occurs at one time
          let mut prfg = self.prflock.lock().unwrap();
          let mut prf = match *prfg {
            Some(_) => {
              reply!(StatusCode::BadRequest, b"Proof req was already received");
              return;
            },
            None => LocnProof::new()
          };

          let mut vid = self.vidlock.lock().unwrap();

          let mut sgn_prf_req: SignedProofReq = match protobuf::parse_from_reader(&mut req) {
            Ok(res) => res,
            Err(_) => {
              reply!(StatusCode::BadRequest, b"Fuck you don't send me invalid protobufs");
              return;
            }
          };
          let mut prf_req = sgn_prf_req.take_proofreq();

          if prf_req.get_seqid() != unsafe { SEQID } && unsafe { SEQID } != 0 {
            reply!(StatusCode::Unauthorized, b"The sequence id sent is too old");
            return;
          }

          *vid = Some(etry!(etry!(PubKey::from_der(prf_req.get_vid())).to_point_bytes()));

          let uid = match PubKey::from_der(prf_req.get_uid()) {
            Ok(v) => v,
            Err(e) => {
              println!("failed to parse uid from bytes: {:?}", e);
              reply!(StatusCode::BadRequest, b"That's not a real user ID.");
              return;
            }
          };

          if etry!(uid.verify(MD::sha256(), p2b!(prf_req), sgn_prf_req.take_sig().as_slice())) {
            let mut prf_res = ProofResp::new();
            prf_res.set_uid(prf_req.get_uid().to_vec());
            prf_res.set_unonce(prf_req.get_unonce().to_vec());

            let sig = self.cfg.key.sign(MD::sha256(), p2b!(prf_res)).unwrap();
            let mut sgn_prf_res = SignedProofResp::new();
            sgn_prf_res.set_proofresp(prf_res.clone());
            sgn_prf_res.set_sig(sig);

            reply!(StatusCode::Ok, p2b!(sgn_prf_res));

            // generate nonce
            let mut nonce = vec![0; 10];
            let rand = rand::SystemRandom::new();
            match rand.fill(nonce.as_mut_slice()) {
              Ok(_) => (),
              Err(e) => {
                println!("Failed to create nonce: {}", e);
                nonce = vec![0x42; 10]
              }
            }

            prf.set_uid(prf_req.take_uid());
            prf.set_unonce(prf_req.take_unonce());
            prf.set_apid(self.cfg.key.pub_to_der().unwrap());
            prf.set_apnonce(nonce);

            *prfg = Some(prf);
          } else {
            reply!(StatusCode::Forbidden, b"That signature doesn't match your public key!");
          }
        },
        (&hyper::Get, "/proof") => {
          println!("received proof");
          let mut prfg = self.prflock.lock().unwrap();
          let vidg = self.vidlock.lock().unwrap();
          {
            let ref mut prf = match *prfg {
              Some(ref mut v) => v,
              None => {
                reply!(StatusCode::BadRequest, b"Proof req was not received");
                return;
              }
            };

            // get rssi and crap.

            let time = etry!(time::SystemTime::now().duration_since(time::UNIX_EPOCH)).as_secs();
            let mut vault_key = etry!(self.gen_proof(prf.get_uid(), prf.get_unonce(), prf.get_apnonce(), time));

            let rand = rand::SystemRandom::new();
            let ekey = etry!(agreement::EphemeralPrivateKey::generate(&agreement::ECDH_P256, &rand));
            let mut pubept = vec![0u8; ekey.public_key_len()];
            etry!(ekey.compute_public_key(&mut pubept));
            let pubekey = etry!(PubKey::from_point(pubept.as_slice()));
            let ref vid = match *vidg {
              Some(ref v) => v,
              None => {
                println!("vid was None when attempting to encrypt vault key!");
                reply!(StatusCode::InternalServerError, b"Something broke :'(");
                return;
              }
            };
            let vid = Input::from(vid.as_slice());

            let key = etry!(agreement::agree_ephemeral(
              ekey, &agreement::ECDH_P256, vid, ring::error::Unspecified,
              |_key_material| {
                let mut key = vec![0; 32]; //digest::SHA256.output_len
                pbkdf2::derive(&pbkdf2::HMAC_SHA256, 30, prf.get_apnonce(),
                               _key_material, key.as_mut_slice());
                SealingKey::new(&aead::AES_256_GCM, key.as_slice())
              }));

            vault_key.extend(vec![0; 16]);
            vault_key = match aead::seal_in_place(&key, &[0; 12], &[], vault_key.as_mut_slice(), 16) {
              Ok(len) => vault_key[..len].to_vec(),
              Err(e) => {
                println!("Error sealing vault key: {}", e);
                reply!(StatusCode::InternalServerError, b"Something broke :'(");
                return;
              }
            };
            prf.set_vault_key(vault_key);
            prf.set_ekey(etry!(pubekey.pub_to_der()));
            prf.set_time(time);

            let sig = self.cfg.key.sign(MD::sha256(), p2b!(prf)).unwrap();
            let mut sgn_prf = SignedLocnProof::new();
            sgn_prf.set_locnproof(prf.clone());
					  sgn_prf.set_sig(sig);

            reply!(StatusCode::Ok, p2b!(sgn_prf));
          }

          *prfg = None;
        },
        _ => {
          println!("received something unknown: {} on {}", req.method, path);
          reply!(StatusCode::NotFound, b"No such resource! Don't be stupid!");
        }
      },
      _ => {
        println!("received weird url");
        reply!(StatusCode::NotImplemented,
               b"This server doesn't know how to deal with proxies and shit.");
      }
    }
  }
}
