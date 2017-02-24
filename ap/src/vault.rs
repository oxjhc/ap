use std::vec::Vec;
use std::collections::HashSet;
use std::ops::Fn;
use rand::Rng;
use rand::os::OsRng;

use galois_field::GF;
use galois_field::FIELD_SZ;

#[derive(Eq, PartialEq, Hash)]
pub struct Point<T>(T, T);

fn make_poly(locn_tag: Vec<u8>) -> Box<Fn(GF) -> GF> {
  let mut gf_locn_tag = Vec::with_capacity(locn_tag.len());
  for loc in locn_tag {
    gf_locn_tag.push(GF::new8(loc));
  }
  Box::new(move |x: GF| {
    let mut ret = GF::new(1);
    for loc in gf_locn_tag.iter() {
      ret *= x-*loc;
    }
    ret
  })
}

pub fn make_vault(locn_tag: Vec<u8>, data_sz: usize, vault_sz: usize) -> HashSet<Point<GF>> {
  let mut vault = HashSet::new();

  // bad bad bad unwrapping
  let mut rand = OsRng::new().unwrap();

  let poly = make_poly(locn_tag);
  let mut xs = HashSet::<GF>::new();
  let mut ys = HashSet::<GF>::new();
  for _ in 0..data_sz {
    let mut x = GF::new(rand.gen_range(0, FIELD_SZ));
    while xs.contains(&x) { x = GF::new(rand.gen_range(0, FIELD_SZ)); }
    let y = poly(x);
    xs.insert(x);
    ys.insert(y);
    vault.insert(Point(x,y));
  }

  for _ in data_sz..vault_sz {
    let mut x = GF::new(rand.gen_range(0, FIELD_SZ));
    while xs.contains(&x) { x = GF::new(rand.gen_range(0, FIELD_SZ)); }
    xs.insert(x);

    let mut y = GF::new(rand.gen_range(0, FIELD_SZ));
    while ys.contains(&y) { y = GF::new(rand.gen_range(0, FIELD_SZ)); }
    ys.insert(y);

    vault.insert(Point(x,y));
  }

  vault
}
