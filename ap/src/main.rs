extern crate ap;
extern crate hyper;
extern crate rand;
extern crate openssl;
extern crate protobuf;
extern crate toml;
//extern crate wpactrl;

use std::{thread, time};
use std::net::UdpSocket;
use std::env;
use std::process::exit;
use std::sync::{Mutex};
use std::fs::File;
use std::io::Read;
use std::collections::BTreeMap as Map;

use hyper::server::{Server, Handler, Request, Response};
use hyper::client::Client;
use hyper::status::StatusCode;
use hyper::uri::RequestUri;
use hyper::header;

use openssl::bn::BigNumContext;
use openssl::sign::{Signer, Verifier};
use openssl::ec::{EcGroup, EcPoint, EcKey};
use openssl::dsa::Dsa;
use openssl::pkey::PKey;
use openssl::nid::X9_62_PRIME256V1 as CURVE;
use openssl::hash::MessageDigest;
use openssl::error::ErrorStack;

//use wpactrl::WpaCtrl;

use rand::Rng;
use rand::os::OsRng;

use protobuf::Message;

use toml::Value;

use ap::vault::make_vault;
use ap::messages::{SignedProofReq,
                   ProofResp, SignedProofResp,
                   VaultMsg, SignedVaultMsg,
                   LocnProof, SignedLocnProof};

static SERVER_URL: &'static str = "http://oxjhc.club";
//static seq_ids: [u64; 2] = [ 0, 0 ]
static mut SEQID: i64 = 0;

#[derive(Debug)]
enum Error {
  ConfErr(String),
  IoErr(std::io::Error),
  TomlDeErr(toml::de::Error)
}

impl From<std::io::Error> for Error {
  fn from(err: std::io::Error) -> Self {
    Error::IoErr(err)
  }
}

impl From<toml::de::Error> for Error {
  fn from(err: toml::de::Error) -> Self {
    Error::TomlDeErr(err)
  }
}

fn ping(sock: &UdpSocket) {
  let seq = unsafe { SEQID };
  // convert apid and seq to a byte array
  let data : [u8; 8] = [
    (seq >> 56) as u8, (seq >> 48) as u8, (seq >> 40) as u8, (seq >> 32) as u8,
    (seq >> 24) as u8, (seq >> 16) as u8, (seq >> 8) as u8, seq as u8
  ];
  match sock.send_to(&data, "192.168.49.255:1832") {
    Ok(n) => if n == 8 { /*println!("pinged with seqid {}", seq);*/ },
    Err(err) => println!("{}", err)
  }
}

fn spawn_pinger() {
  // this... should be modifiable at runtime.
  // buuuuuuuut I'm lazy
  let pinger = UdpSocket::bind("0.0.0.0:1832").unwrap();
  pinger.set_broadcast(true).unwrap();

  thread::spawn(move || {
    let mut rand = OsRng::new().unwrap();
    loop {
      thread::sleep(time::Duration::from_millis(100));
      unsafe { SEQID = rand.gen::<i64>(); }
      ping(&pinger);
    }
  });
}


fn key_from_bytes(bytes: &[u8]) -> Result<PKey, ErrorStack> {
  //let group = EcGroup::from_curve_name(CURVE).unwrap();
  //let mut ctx = BigNumContext::new().unwrap();
  //let point = EcPoint::from_bytes(&group, bytes, &mut ctx).unwrap();
  //let key = EcKey::from_public_key(&group, &point)?;
  let key = Dsa::public_key_from_der(bytes)?;
  PKey::from_dsa(key)
}

struct Dormouse {
  key: PKey,
  prflock: Mutex<LocnProof>,
  cfg: Map<String, Value>
}

impl Dormouse {
  fn new(key: PKey) -> Dormouse {
    let prflock = Mutex::new(LocnProof::new());
    let cfg = Dormouse::read_cfg().unwrap();
    Dormouse{key, prflock, cfg}
  }

  fn read_cfg() -> Result<Map<String, Value>, Error> {
    let mut cfg_file = File::open("/etc/dormouse/config.toml")?;
    let mut contents = String::new();
    cfg_file.read_to_string(&mut contents)?;
    match contents.parse::<Value>() {
      Ok(res) => match res.as_table() {
        Some(table) => Ok(table.clone()),
        None => Err(Error::ConfErr("not a table".to_string()))
      },
      Err(err) => Err(Error::TomlDeErr(err))
    }
  }

  fn gen_proof(&self, uid: &[u8], unonce: &[u8], nonce: &[u8], time: u64) {
    println!("starting gen_proof");
    let locn_tag = vec![ 243, 122, 33, 214 ];
    let vault = make_vault(locn_tag, 10, 100);

    let mut msg = VaultMsg::new();
    msg.set_vault(vault);
    msg.set_uid(uid.to_vec());
    msg.set_unonce(unonce.to_vec());
    println!("here");
    msg.set_apid(self.key.public_key_to_der().unwrap());
    msg.set_apnonce(nonce.to_vec());
    msg.set_time(time);

    let mut signer = Signer::new(MessageDigest::sha256(), &self.key).unwrap();
    signer.update(msg.write_to_bytes().unwrap().as_slice()).unwrap();
    let mut sgn_msg = SignedVaultMsg::new();
    sgn_msg.set_vault_msg(msg);
    sgn_msg.set_sig(signer.finish().unwrap());

    // send proof back to client (after connecting via wifi-direct and
    // generating proof

    // I really need a better error handling method
    let client = Client::new();
    println!("sending vault to server");
    let resp =
      match client.post(format!("{}{}", SERVER_URL, "/vault").as_str())
      .header(header::ContentType("application/x-protobuf".parse().unwrap()))
      .body(sgn_msg.write_to_bytes().unwrap().as_slice())
      .send() {
        Ok(res) => res,
        Err(err) => {
          println!("Error sending vault to server: {}", err);
          return;
        }
      };
    if resp.status == hyper::Ok {
      println!("vault successfully sent");
      // done.
    } else {
      println!("error sending vault: {}", resp.status);
    }
  }
}

impl Handler for Dormouse {
  fn handle(&self, mut req: Request, mut res: Response) {
    match req.uri.clone() {
      RequestUri::AbsolutePath(ref path) => match (&req.method, &path[..]) {
        (&hyper::Post, "/proof_req") => {
          println!("received proof req");
          // lock proof lock, ensuring only one proof generation occurs at one time
          let mut prf = self.prflock.lock().unwrap();
          if prf.has_vault_key() {
            *res.status_mut() = StatusCode::BadRequest;
            res.send(b"Proof req was already received").unwrap();
            return;
          }

          let mut sgn_prf_req: SignedProofReq = match protobuf::parse_from_reader(&mut req) {
            Ok(res) => res,
            Err(_) => {
              *res.status_mut() = StatusCode::BadRequest;
              res.send(b"Fuck you don't send me invalid protobufs").unwrap();
              return;
            }
          };
          let mut prf_req = sgn_prf_req.take_proofreq();

          if prf_req.get_seqid() != unsafe { SEQID } {
            *res.status_mut() = StatusCode::Unauthorized;
            res.send(b"The sequence id sent is too old").unwrap();
            return;
          }

          // lol more unwrapping
          let ukey = key_from_bytes(prf_req.get_uid()).unwrap();
          let mut verifier = Verifier::new(MessageDigest::sha256(), &ukey).unwrap();
          verifier.update(prf_req.write_to_bytes().unwrap().as_slice()).unwrap();
          if verifier.finish(sgn_prf_req.take_sig().as_slice()).unwrap() {
            let mut prf_res = ProofResp::new();
            prf_res.set_uid(prf_req.get_uid().to_vec());
            prf_res.set_unonce(prf_req.get_unonce().to_vec());

            let mut signer = Signer::new(MessageDigest::sha256(), &self.key).unwrap();
            signer.update(prf_res.write_to_bytes().unwrap().as_slice()).unwrap();
            let mut sgn_prf_res = SignedProofResp::new();
            sgn_prf_res.set_proofresp(prf_res.clone());
            sgn_prf_res.set_sig(signer.finish().unwrap());

            *res.status_mut() = StatusCode::Ok;
            res.send(sgn_prf_res.write_to_bytes().unwrap().as_slice()).unwrap();

            // generate nonce
            let mut nonce = Vec::<u8>::new();
            nonce.reserve(10);
            let mut rand = OsRng::new().unwrap();
            for _ in 1..10 { nonce.push(rand.gen()); }

            let time = time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap().as_secs();
            self.gen_proof(prf_res.get_uid(), prf_res.get_unonce(), nonce.as_slice(), time);

            prf.set_vault_key(vec![0x00]);
            prf.set_uid(prf_req.take_uid());
            prf.set_unonce(prf_req.take_unonce());
            prf.set_apid(self.key.public_key_to_der().unwrap());
            prf.set_apnonce(nonce);
            prf.set_time(time);
          } else {
            *res.status_mut() = StatusCode::Forbidden;
            res.send(b"That signature doesn't match your public key!").unwrap();
          }
        },
        (&hyper::Get, "/proof") => {
          println!("received proof");
          let prf = self.prflock.lock().unwrap();
          if !prf.has_vault_key() {
            *res.status_mut() = StatusCode::BadRequest;
            res.send(b"Proof req was not received").unwrap();
            return;
          }

          // get rssi and crap.

          let mut signer = Signer::new(MessageDigest::sha256(), &self.key).unwrap();
          signer.update(prf.write_to_bytes().unwrap().as_slice()).unwrap();
          let mut sgn_prf = SignedLocnProof::new();
          sgn_prf.set_locnproof(prf.clone());
          sgn_prf.set_sig(signer.finish().unwrap());

          *res.status_mut() = StatusCode::Ok;
          res.send(sgn_prf.write_to_bytes().unwrap().as_slice()).unwrap();
        },
        _ => {
          println!("received something unknown: {} on {}", req.method, path);
          *res.status_mut() = StatusCode::NotFound;
          res.send(b"No such resource! Don't be stupid!").unwrap();
        }
      },
      _ => {
        println!("received weird url");
        *res.status_mut() = StatusCode::NotImplemented;
        res.send(b"This server doesn't know how to deal with proxies and shit.").unwrap();
      }
    }
  }
}

fn ping_server() {
  let client = Client::new();
  let resp = client.get(format!("{}{}", SERVER_URL, "/ping").as_str())
    .send().unwrap();
  if resp.status == hyper::Ok {
    println!("server successfully pinged");
    // done.
  } else {
    println!("the server is down :(.");
  }
}

fn main() {
  let group = EcGroup::from_curve_name(CURVE).unwrap();
  //let mut ctx = BigNumContext::new().unwrap();
  //let point = EcPoint::from_bytes(&group, &public_key, &mut ctx).unwrap();
  //let key = EcKey::from_public_key(&group, &point).unwrap();
  let key = EcKey::generate(&group).unwrap();
  key.check_key().unwrap();
  let pkey = PKey::from_ec_key(key).unwrap();
  let dormouse = Dormouse::new(pkey);

  spawn_pinger();

  for arg in env::args() {
    if arg == "-test" {
      println!("sending test location proof");
      dormouse.gen_proof(&[0x00], &[0x11], &[0x22], time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap().as_secs());
      exit(0);
    } else if arg == "-ping" {
      println!("pinging server");
      ping_server();
      exit(0);
    }
  }

  println!("starting dormouse");
  Server::http("0.0.0.0:80").unwrap().handle(dormouse).unwrap();
}
