extern crate ap;
extern crate hyper;
extern crate rand;
extern crate ring;
extern crate protobuf;

use std::{thread, time};
use std::net::UdpSocket;
use std::os::unix::net::UnixStream;
use std::env;
use std::process::exit;

use hyper::server::{Server, Request, Response};
use hyper::client::Client;
use hyper::status::StatusCode;
use hyper::uri::RequestUri;
use hyper::header;
use hyper::mime::{Mime, TopLevel, SubLevel};

use rand::Rng;
use rand::os::OsRng;

use protobuf::Message;

use ap::vault::make_vault;
use ap::messages::{ProofReq, SignedProofReq,
                   ProofResp, SignedProofResp,
                   VaultMsg, SignedVaultMsg};

static SERVER_URL: &'static str = "http://oxjhc.club";

fn ping(seq: u64, sock: &UdpSocket) {
  // convert apid and seq to a byte array
  let data : [u8; 8] = [
    (seq >> 56) as u8, (seq >> 48) as u8, (seq >> 40) as u8, (seq >> 32) as u8,
    (seq >> 24) as u8, (seq >> 16) as u8, (seq >> 8) as u8, seq as u8
  ];
  sock.send_to(&data, "255.255.255.255:1832").unwrap();
}

fn handle_req(mut req: Request, mut res: Response) {
  match req.uri.clone() {
    RequestUri::AbsolutePath(ref path) => match (&req.method, &path[..]) {
      (&hyper::Post, "/proof_req") => {
        // generate nonce
        let mut nonce = Vec::<u8>::new();
        nonce.reserve(10);
        let mut rand = OsRng::new().unwrap();
        for _ in 1..10 { nonce.push(rand.gen()); }

        // TODO: unwrapping here is BAD
        let mut sgn_prf_req: SignedProofReq = protobuf::parse_from_reader(&mut req).unwrap();
        let prf_req = sgn_prf_req.take_proofreq();

        *res.status_mut() = StatusCode::NotImplemented;
        res.send(b"This ap sucks right now. It probably sent a fake response to the server.").unwrap();

        gen_proof(nonce);
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

fn gen_proof(nonce: Vec<u8>) {
  let locn_tag = vec![ 243, 122, 33, 214 ];
  let vault = make_vault(locn_tag, 10, 100);

  let mut msg = VaultMsg::new();
  msg.set_vault(vault);
  msg.set_uid(vec![0x12]);
  msg.set_unonce(vec![0x34]);
  msg.set_apid(vec![0x56]);
  msg.set_apnonce(nonce);
  msg.set_time(time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap().as_secs());

  let mut sgn_msg = SignedVaultMsg::new();
  sgn_msg.set_vault_msg(msg);
  sgn_msg.set_sig(vec![0x78]);

  // send proof back to client (after connecting via wifi-direct and
  // generating proof

  // I really need a better error handling method
  let client = Client::new();
  let (mut rdr, mut wtr) = UnixStream::pair().unwrap();
  sgn_msg.write_to_writer(&mut wtr);
  wtr.shutdown(std::net::Shutdown::Write);
  println!("sending vault to server");
  let resp = client.post(format!("{}{}", SERVER_URL, "/vault").as_str())
    .header(header::ContentType("application/x-protobuf".parse().unwrap()))
    .body(&mut rdr)
    .send().unwrap();
  if resp.status == hyper::Ok {
    println!("vault successfully sent");
    // done.
  } else {
    println!("error sending vault: {}", resp.status);
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
  // spawn pinger
  // TODO: this... should be modifiable at runtime.
  // buuuuuuuut I'm lazy
  let pinger = UdpSocket::bind("127.0.0.1:1832").unwrap();
  pinger.set_broadcast(true).unwrap();
  thread::spawn(move || {
    let mut rand = OsRng::new().unwrap();
    loop {
      thread::sleep(time::Duration::from_millis(100));
      ping(rand.gen(), &pinger);
    }
  });

  for arg in env::args() {
    if arg == "-test" {
      gen_proof(vec![0x00]);
      exit(0);
    } else if arg == "-ping" {
      ping_server();
      exit(0);
    }
  }

  Server::http("0.0.0.0:1865").unwrap().handle(handle_req).unwrap();
}
