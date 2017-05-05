extern crate ap;
extern crate hyper;
extern crate untrusted;
extern crate ring;
extern crate openssl;
extern crate toml;
extern crate base64;
extern crate get_if_addrs;
//extern crate wpactrl;

use std::env;
use std::process::exit;

use hyper::server::Server;

use ap::Dormouse;

fn main() {
  let dormouse = Dormouse::new();

  for arg in env::args() {
    if arg == "-test" {
      println!("sending test location proof");
      dormouse.server_test();
      exit(0);
    } else if arg == "-ping" {
      println!("pinging server");
      dormouse.ping_server();
      exit(0);
    }
  }

  println!("starting dormouse");
  Server::http(format!("0.0.0.0:{}", dormouse.cfg.http_port)).unwrap().handle(dormouse).unwrap();
}
