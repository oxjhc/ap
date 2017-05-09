extern crate ap;
extern crate hyper;
//extern crate wpactrl;

use std::env;
use std::fs::File;
use std::io::Write;
use std::process::{Command, exit};

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

  print!("restarting wpa_supplicant... ");
  Command::new("systemctl").arg("restart").arg("wpa_supplicant-nl80211@wlan0").output().unwrap();
  println!("done");
  print!("flushing p2p state... ");
  let out = Command::new("wpa_cli").arg("p2p_flush").output().unwrap();
  println!("done.");
  //println!("stdout: {}", String::from_utf8(out.stdout).unwrap());
  //println!("stderr: {}", String::from_utf8(out.stderr).unwrap());
  print!("flushing p2p services... ");
  let out = Command::new("wpa_cli").arg("p2p_service_flush").output().unwrap();
  println!("done.");
  //println!("stdout: {}", String::from_utf8(out.stdout).unwrap());
  //println!("stderr: {}", String::from_utf8(out.stderr).unwrap());
  print!("adding service... ");
  let out = Command::new("wpa_cli")
    .arg("p2p_service_add").arg("upnp").arg("10")
    .arg("uuid:Dormouse::urn:schemas-oxjhc-club:service:TeaParty:1")
    .output().unwrap();
  println!("done.");
  //println!("stdout: {}", String::from_utf8(out.stdout).unwrap());
  //println!("stderr: {}", String::from_utf8(out.stderr).unwrap());
  print!("starting p2p listen... ");
  let out = Command::new("wpa_cli").arg("p2p_listen").output().unwrap();
  println!("done.");
  //println!("stdout: {}", String::from_utf8(out.stdout).unwrap());
  //println!("stderr: {}", String::from_utf8(out.stderr).unwrap());

  print!("spawning p2p controller... ");
  // TODO: use tempfile
  let mut tmpscript = File::create("tmp.sh").unwrap();
  tmpscript.write_all(br#"#!/bin/bash
journalctl -flu wpa_supplicant-nl80211@wlan0 -n0 |
  stdbuf -oL sed -n 's/.*P2P-GO-NEG-REQUEST\ \([0-9a-f:]*\)\ .*/\1/p' |
  while IFS= read -r line; do
    echo "connecting to $line"
    wpa_cli p2p_connect "$line" pbc
  done
"#).unwrap();
  Command::new("bash")
    .arg("tmp.sh")
    .spawn().unwrap();
  println!("done.");

  print!("starting dormouse... ");
  Server::http(format!("0.0.0.0:{}", dormouse.cfg.http_port)).unwrap().handle(dormouse).unwrap();
  println!("done.");
}
