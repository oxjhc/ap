use std::ops::*;
use std::convert::Into;
use std::convert::From;

pub const FIELD_SZ: u32 = 65521;

/*
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Debug, Default)]
pub struct GF(u8);

impl GF {
  pub fn new(val: u8) -> GF {
    GF(val)
  }
}

impl Sub for GF {
  type Output = GF;
  fn sub(self, other: GF) -> GF {
    self.0 ^ other.0
  }
}

impl SubAssign for GF {
  fn sub(&mut self, other: GF) {
    *self = self.0 ^ other.0;
  }
}

impl Add for GF {
  type Output = GF;
  fn add(self, other: GF) -> GF {
    self.0 ^ other.0
  }
}
*/

// TODO: CRYPTOGRAPHICALLY HARDEN
// IMPORTANT!

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Debug, Default)]
pub struct GF(u32);

impl GF {
  pub fn new(val: u32) -> GF {
    GF(val % FIELD_SZ)
  }
  pub fn new8(val: u8) -> GF {
    GF(val as u32 % FIELD_SZ)
  }
}

impl Sub for GF {
  type Output = GF;
  fn sub(self, other: GF) -> GF {
    GF::new(self.0 + (FIELD_SZ-other.0))
  }
}

impl SubAssign for GF {
  fn sub_assign(&mut self, other: GF) {
    *self = *self - other;
  }
}

impl Add for GF {
  type Output = GF;
  fn add(self, other: GF) -> GF {
    GF::new(self.0 + other.0)
  }
}

impl AddAssign for GF {
  fn add_assign(&mut self, other: GF)  {
    *self = *self + other;
  }
}

impl Mul for GF {
  type Output = GF;
  fn mul(self, other: GF) -> GF {
    GF::new(self.0 * other.0)
 }
}

impl MulAssign for GF {
  fn mul_assign(&mut self, other: GF) {
    *self = *self * other;
  }
}

// maybe implement div

impl Neg for GF {
  type Output = GF;

  fn neg(self) -> GF {
    GF(FIELD_SZ-self.0)
  }
}

impl Rem<u32> for GF {
  type Output = GF;

  fn rem(self, other: u32) -> GF {
    let GF(n) = self;
    GF(n%other)
  }
}

impl From<u32> for GF {
  fn from(val: u32) -> GF {
    GF(val % FIELD_SZ)
  }
}

impl Into<u32> for GF {
  fn into(self) -> u32 {
    self.0 as u32
  }
}

impl Into<u16> for GF {
  fn into(self) -> u16 {
    self.0 as u16
  }
}
