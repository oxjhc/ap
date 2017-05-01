extern crate ap;
extern crate chan;
extern crate hyper;
extern crate rand;
extern crate openssl;
extern crate protobuf;
//extern crate wpactrl;

use std::{thread, time};
use std::net::{UdpSocket, SocketAddr};
use std::env;
use std::process::exit;
use std::sync::{Mutex};

use chan::{Sender, Receiver};

use hyper::server::{Server, Handler, Request, Response};
use hyper::client::Client;
use hyper::status::StatusCode;
use hyper::uri::RequestUri;
use hyper::header;

use openssl::bn::BigNumContext;
use openssl::sign::{Signer, Verifier};
use openssl::ec::{EcGroup, EcPoint, EcKey};
use openssl::pkey::PKey;
use openssl::nid::X9_62_PRIME256V1 as CURVE;
use openssl::hash::MessageDigest;
use openssl::error::ErrorStack;

//use wpactrl::WpaCtrl;

use rand::Rng;
use rand::os::OsRng;

use protobuf::Message;

use ap::vault::make_vault;
use ap::messages::{SignedProofReq,
                   ProofResp, SignedProofResp,
                   VaultMsg, SignedVaultMsg,
                   LocnProof, SignedLocnProof};

static SERVER_URL: &'static str = "http://oxjhc.club";
//static seq_ids: [u64; 2] = [ 0, 0 ]
static mut SEQID: i64 = 0;

fn ping(sock: &UdpSocket) {
  let seq = unsafe { SEQID };
  // convert apid and seq to a byte array
  let data : [u8; 8] = [
    (seq >> 56) as u8, (seq >> 48) as u8, (seq >> 40) as u8, (seq >> 32) as u8,
    (seq >> 24) as u8, (seq >> 16) as u8, (seq >> 8) as u8, seq as u8
  ];
  match sock.send_to(&data, "255.255.255.255:1832") {
    Ok(_) => (),
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
  let group = EcGroup::from_curve_name(CURVE).unwrap();
  let mut ctx = BigNumContext::new().unwrap();
  let point = EcPoint::from_bytes(&group, bytes, &mut ctx).unwrap();
  let key = try!(EcKey::from_public_key(&group, &point));
  PKey::from_ec_key(key)
}

struct Dormouse {
  key: PKey,
  prflock: Mutex<()>,
  tx: Sender<()>,
  rx: Receiver<()>
}

impl Dormouse {
  fn new(key: PKey) -> Dormouse {
    let lock = Mutex::new(());
    let (tx, rx) = chan::sync(0);
    Dormouse{key: key, prflock: lock, tx: tx, rx: rx}
  }

  fn gen_proof(&self, uid: &[u8], unonce: &[u8], nonce: &[u8]) {
    let locn_tag = vec![ 243, 122, 33, 214 ];
    let vault = make_vault(locn_tag, 10, 100);

    let mut msg = VaultMsg::new();
    msg.set_vault(vault);
    msg.set_uid(uid.to_vec());
    msg.set_unonce(unonce.to_vec());
    msg.set_apid(self.key.public_key_to_der().unwrap());
    msg.set_apnonce(nonce.to_vec());
    msg.set_time(time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap().as_secs());

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
    let resp = client.post(format!("{}{}", SERVER_URL, "/vault").as_str())
      .header(header::ContentType("application/x-protobuf".parse().unwrap()))
      .body(sgn_msg.write_to_bytes().unwrap().as_slice())
      .send().unwrap();
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
          // lock proof lock, ensuring only one proof generation occurs at one time
          let _ = self.prflock.lock().unwrap();

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
            prf_res.set_uid(prf_req.take_uid());
            prf_res.set_unonce(prf_req.take_unonce());

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

            self.gen_proof(prf_res.get_uid(), prf_res.get_unonce(), nonce.as_slice());

            self.rx.recv();
          } else {
            *res.status_mut() = StatusCode::Forbidden;
            res.send(b"That signature doesn't match your public key!").unwrap();
          }
        },
        (&hyper::Get, "/proof") => {
          match self.prflock.try_lock() {
            Ok(_) => {
              *res.status_mut() = StatusCode::Unauthorized;
              res.send(b"no request in progress").unwrap();
            },
            Err(_) => {}
          }
          self.tx.send(());
        },
        _ => {
          *res.status_mut() = StatusCode::NotFound;
          res.send(b"No such resource! Don't be stupid!").unwrap();
        }
      },
      _ => {
        *res.status_mut() = StatusCode::NotImplemented;
        res.send(b"Please post to this server.").unwrap();
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
  let key = EcKey::from_curve_name(CURVE).unwrap();
  let pkey = PKey::from_ec_key(key).unwrap();
  let dormouse = Dormouse::new(pkey);

  spawn_pinger();

  for arg in env::args() {
    if arg == "-test" {
      dormouse.gen_proof(&[0x00], &[0x11], &[0x22]);
      exit(0);
    } else if arg == "-ping" {
      ping_server();
      exit(0);
    }
  }

  Server::http("0.0.0.0:1865").unwrap().handle(dormouse).unwrap();
}
