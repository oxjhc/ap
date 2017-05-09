extern crate hyper;
extern crate ring;
extern crate base64;

use std::{thread, time};
use std::sync::Mutex;
use std::io::Read;
use std::process::Command;

use self::hyper::server::{Handler, Request, Response};
use self::hyper::client::Client;
use self::hyper::status::StatusCode;
use self::hyper::uri::RequestUri;
use self::hyper::header;

use protobuf;
use protobuf::Message;

use util::p2p_iface_name;
use messages::{SignedProofReq,
               ProofResp, SignedProofResp,
               VaultMsg, SignedVaultMsg,
               LocnProof, SignedLocnProof,
               SignedToken};
use vault::make_vault;
use crypto::{PubKey, EphKey, gen_nonce};
use config::Config;
use error::Error;
use pinger::Pinger;

pub struct Dormouse {
  prflock: Mutex<Option<LocnProof>>,
  vidlock: Mutex<Option<PubKey>>,
  pinger: Pinger,
  pub cfg: Config
}

impl Dormouse {
  pub fn new() -> Dormouse {
    let prflock = Mutex::new(None);
    let vidlock = Mutex::new(None);
    let cfg = Config::new().unwrap();

    let pinger = Pinger::new(cfg.ping_port).unwrap();

    //printhex!(cfg.key.pub_to_der().unwrap());
    Dormouse{prflock, vidlock, pinger, cfg}
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
    let unonce = gen_nonce();
    let nonce = gen_nonce();
    let time = time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap().as_secs();

    let mut prf = LocnProof::new();

    prf.set_uid(uid.clone());
    prf.set_unonce(unonce.clone());
    prf.set_apid(self.cfg.key.pub_to_der().unwrap());
    prf.set_apnonce(nonce.clone());

    let mut vault_key = self.gen_proof(uid.as_slice(), unonce.as_slice(),
                                       nonce.as_slice(), time).unwrap();

    let ekey = EphKey::new().unwrap();
    let vid = PubKey::from_file(&self.cfg._t_vid_file).unwrap();
    prf.set_ekey(ekey.pub_to_der().unwrap());
    prf.set_time(time);

    vault_key = ekey.seal(vault_key.as_slice(), &vid, prf.get_apnonce()).unwrap();

    prf.set_vault_key(vault_key);

    println!("locn_tag: {:?}", self.cfg.locn_tag);
    let mut to_sign = Vec::with_capacity(20);
    for x in self.cfg.locn_tag.iter() {
      let (h, l) = x.to_byte_pair();
      to_sign.push(h); to_sign.push(l);
    }
    let sig = self.cfg.key.sign(to_sign.as_slice()).unwrap();
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
      let sgn_tok : SignedToken = protobuf::parse_from_reader(&mut resp).unwrap();
      print!("token:\n\t");
      printhex!(sgn_tok.get_token().get_vnonce());
      print!("locn_tag base64:\n\t");
      print64!(to_sign);
      print!("received locn_tag base64:\n\t");
      print64!(sgn_tok.get_token().get_locn_tag());
      // done.
    } else {
      println!("error sending vault: {}", resp.status);
      let mut res = String::new();
      resp.read_to_string(&mut res).unwrap();
      println!("resp is:\n{:?}", res);
    }
  }

  fn gen_proof(&self, uid: &[u8], unonce: &[u8], nonce: &[u8], time: u64) -> Result<Vec<u8>, Error> {
    println!("starting gen_proof");
    // probably add ways to get tag from cssi
    let key_sz = 10;
    let (vault, vault_key) = make_vault(self.cfg.locn_tag.as_slice(), key_sz, 100);

    let mut msg = VaultMsg::new();
    msg.set_vault(vault);
    msg.set_uid(uid.to_vec());
    msg.set_unonce(unonce.to_vec());
    msg.set_apid(self.cfg.key.pub_to_der().unwrap());
    msg.set_apnonce(nonce.to_vec());
    msg.set_time(time);

    let sig = self.cfg.key.sign(p2b!(msg)).unwrap();
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
          return Err(From::from(err));
        }
      };

    if resp.status == hyper::Ok {
      println!("vault successfully sent for user:");
      printhex!(sgn_msg.get_vault_msg().get_uid());
      // done.
    } else {
      println!("error sending vault: {}", resp.status);
    }

    let mut ret = Vec::<u8>::with_capacity(20);
    vault_key.into_iter().map(|x| {
      ret.push((x >> 8) as u8); ret.push(x as u8);
    }).last(); // last() is run to consume the iterator because it's lazy and won't
               // evaluate the funtion otherwise.
    Ok(ret)
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

          let cur_seq = self.pinger.get_seqid();
          if prf_req.get_seqid() != cur_seq && cur_seq != 0 {
            reply!(StatusCode::Unauthorized, b"The sequence id sent is too old");
            return;
          }

          *vid = Some(etry!(PubKey::from_der(prf_req.get_vid())));

          let uid = match PubKey::from_der(prf_req.get_uid()) {
            Ok(v) => v,
            Err(e) => {
              println!("failed to parse uid from bytes: {:?}", e);
              reply!(StatusCode::BadRequest, b"That's not a real user ID.");
              return;
            }
          };

          if etry!(uid.verify(p2b!(prf_req), sgn_prf_req.take_sig().as_slice())) {
            let mut prf_res = ProofResp::new();
            prf_res.set_uid(prf_req.get_uid().to_vec());
            prf_res.set_unonce(prf_req.get_unonce().to_vec());

            let sig = self.cfg.key.sign(p2b!(prf_res)).unwrap();
            let mut sgn_prf_res = SignedProofResp::new();
            sgn_prf_res.set_proofresp(prf_res.clone());
            sgn_prf_res.set_sig(sig);

            reply!(StatusCode::Ok, p2b!(sgn_prf_res));

            // generate nonce
            let nonce = gen_nonce();

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

            let ekey = etry!(EphKey::new());
            let ref vid = match *vidg {
              Some(ref v) => v,
              None => {
                println!("vid was None when attempting to encrypt vault key!");
                reply!(StatusCode::InternalServerError, b"Something broke :'(");
                return;
              }
            };
            prf.set_ekey(etry!(ekey.pub_to_der()));
            prf.set_time(time);

            vault_key = etry!(ekey.seal(vault_key.as_slice(), &vid, prf.get_apnonce()));

            prf.set_vault_key(vault_key);

            let mut to_sign = Vec::with_capacity(20);
            for x in self.cfg.locn_tag.iter() {
              let (h, l) = x.to_byte_pair();
              to_sign.push(h); to_sign.push(l);
            }
            let sig = self.cfg.key.sign(to_sign.as_slice()).unwrap();
            let mut sgn_prf = SignedLocnProof::new();
            sgn_prf.set_locnproof(prf.clone());
					  sgn_prf.set_sig(sig);

            reply!(StatusCode::Ok, p2b!(sgn_prf));
          }

          match p2p_iface_name() {
            Ok(i) => match i {
              Some(i) => {
                match Command::new("wpa_cli")
                  .arg("p2p_group_remove")
                  .arg(i).output() {
                    Ok(o) => {
                      println!("group remove output:\n{}", String::from_utf8(o.stdout).unwrap())
                    },
                    Err(err) => {
                      println!("Failed to remove group: {}", err);
                    }
                  };
                thread::sleep(time::Duration::from_millis(100));
                match Command::new("wpa_cli")
                  .arg("p2p_listen")
                  .arg("-i")
                  .arg("p2p-dev-wlan0")
                  .output() {
                    Ok(o) => {
                      println!("p2p listen output:\n{}", String::from_utf8(o.stdout).unwrap())
                    },
                    Err(err) => {
                      println!("Failed to start listening on p2p: {}", err);
                    }
                  };
              },
              _ => {
                println!("No p2p interface");
              }
            },
            _ => {
              println!("Failed to get interfaces");
            }
          };
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
