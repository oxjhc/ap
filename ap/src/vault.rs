extern crate rand;

use std::vec::Vec;
use std::collections::HashSet;

use self::rand::Rng;
use self::rand::os::OsRng;

use galois_field::GF;
use galois_field::FIELD_SZ;

use messages::{VaultMsg_Vault as Vault, VaultMsg_Vault_Point as Point};

use protobuf;

fn make_poly(locn_tag: &[GF]) -> Box<Fn(GF) -> GF> {
  let mut my_tag = Vec::with_capacity(locn_tag.len());
  for x in locn_tag {
    my_tag.push(*x);
  }
  Box::new(move |x: GF| {
    let mut ret = GF::new(0);
    for (i, loc) in my_tag.iter().enumerate() {
      ret += x.pow(i as u32) * *loc;
    }
    ret
  })
}

fn make_point(x: GF, y: GF) -> Point {
  let mut pt = Point::new();
  pt.set_x(x.into());
  pt.set_y(y.into());
  return pt;
}

pub fn make_vault(locn_tag: &[GF], data_sz: usize, vault_sz: usize) -> (Vault, Vec<u16>) {
  let mut vault = Vault::new();
  let mut points = protobuf::RepeatedField::new();

  // bad bad bad unwrapping
  let mut rand = OsRng::new().unwrap();

  let poly = make_poly(locn_tag);
  let mut key = Vec::with_capacity(data_sz);
  let mut xs = HashSet::<GF>::new();
  let mut ys = HashSet::<GF>::new();
  for _ in 0..data_sz {
    let mut x = GF::new(rand.gen_range(0, FIELD_SZ));
    while xs.contains(&x) { x = GF::new(rand.gen_range(0, FIELD_SZ)); }
    let y = poly(x);
    let xi: u16 = x.into();
    key.push(xi);
    xs.insert(x);
    ys.insert(y);

    points.push(make_point(x, y));
  }

  for _ in data_sz..vault_sz {
    let mut x = GF::new(rand.gen_range(0, FIELD_SZ));
    while xs.contains(&x) { x = GF::new(rand.gen_range(0, FIELD_SZ)); }
    xs.insert(x);

    let mut y = GF::new(rand.gen_range(0, FIELD_SZ));
    while ys.contains(&y) { y = GF::new(rand.gen_range(0, FIELD_SZ)); }
    ys.insert(y);

    points.push(make_point(x, y));
  }

  vault.set_points(points);

  (vault, key)
}
