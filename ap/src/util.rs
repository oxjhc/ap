#![macro_use]
extern crate std;
extern crate get_if_addrs;

use std::net::Ipv4Addr;
use self::get_if_addrs::{get_if_addrs as ifaces, IfAddr};

use error::Error;

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

macro_rules! print64 {
  ($x:expr) => {
    println!("{}", base64::encode(&$x));
  }
}

pub fn p2p_iface_name() -> Result<Option<String>, Error> {
  for iface in ifaces()? {
    if iface.name.starts_with("p2p") {
      return Ok(Some(iface.name))
    }
  }
  return Ok(None)
}

pub fn p2p_iface_braddr() -> Result<Option<Ipv4Addr>, Error> {
  for iface in ifaces()? {
    if iface.name.starts_with("p2p") {
      match iface.addr {
        IfAddr::V4(addr) => match addr.broadcast {
          Some(v) => return Ok(Some(v)),
          _ => {}
        },
        _ => {}
      }
    }
  }
  return Ok(None)
}
