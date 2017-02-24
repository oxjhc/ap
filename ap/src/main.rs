extern crate ap;
extern crate tiny_http;
extern crate protobuf;

use std::{thread, time};
use std::net::UdpSocket;
use std::env;

use tiny_http::{Server, Request, Response};

use ap::vault::make_vault;

fn ping(apid: u32, seq: u64, sock: &UdpSocket) {
  let data : [u8; 12] = [
    (apid >> 24) as u8, (apid >> 16) as u8, (apid >> 8) as u8, (apid) as u8,
    (seq >> 56) as u8, (seq >> 48) as u8, (seq >> 40) as u8, (seq >> 32) as u8,
    (seq >> 24) as u8, (seq >> 16) as u8, (seq >> 8) as u8, seq as u8
  ];
  sock.send_to(&data, "255.255.255.255:1832");
}

fn handle_req(req: Request) {
  let locn_tag = vec![ 243, 122, 33, 214 ];
  let vault = make_vault(locn_tag, 10, 100);
  req.respond(Response::from_string("Bleh."));
  // TODO: this... fix
}

fn main() {
  let args: Vec<_> = env::args().collect();
  let mut apid: u32 = 0;
  if args.len() > 1 {
    apid = args[1].parse::<u32>().unwrap();
  } else {
    panic!("No ap id passed");
  }
  // spawn pinger
  // TODO: ports should be customisable
  let pinger = UdpSocket::bind("127.0.0.1:1832").unwrap();
  pinger.set_broadcast(true);
  thread::spawn(move || {
    let mut seq = 0;
    loop {
      seq += 1;
      thread::sleep(time::Duration::from_millis(100));
      ping(apid, seq, &pinger);
    }
  });

  // Every request spawns a new thread a la Apache.
  // This is a terrible idea but I'm rather lazy.
  // Maybe Sam can make it better ;)

  let server = Server::http("0.0.0.0:1865").unwrap();

  for req in server.incoming_requests() {
    thread::spawn(move || handle_req(req));
  }
}
