extern crate openssl;
extern crate base64;

use std::fs::File;
use std::io::Read;

use self::openssl::bn::BigNumContext;
use self::openssl::pkey::PKey;
use self::openssl::sign::{Signer, Verifier};
use self::openssl::nid::X9_62_PRIME256V1 as CURVE;
use self::openssl::ec::{POINT_CONVERSION_UNCOMPRESSED, EcPoint, EcGroup, EcKey};
use self::openssl::hash::MessageDigest;

use error::Error;

pub struct PrivKey {
  pkey: PKey
}

#[allow(dead_code)]
impl PrivKey {
  pub fn from_file(fname: &str) -> Result<PrivKey, Error> {
    let mut key_file = File::open(fname)?;
    let mut contents = String::new();
    key_file.read_to_string(&mut contents).unwrap();
    let pkey = PKey::private_key_from_pem(contents.as_bytes()).unwrap();
    Ok(PrivKey{pkey})
  }

  pub fn pub_to_der(&self) -> Result<Vec<u8>, Error> {
    Ok(self.pkey.public_key_to_der()?)
  }

  pub fn sign(&self, digest: MessageDigest, msg: &[u8]) -> Result<Vec<u8>, Error> {
    let mut signer = Signer::new(digest, &self.pkey).unwrap();
    signer.update(msg)?;
    Ok(signer.finish()?)
  }
}

pub struct PubKey {
  pkey: PKey
}

#[allow(dead_code)]
impl PubKey {
  pub fn new(pkey: PKey) -> PubKey {
    PubKey{pkey}
  }

  pub fn from_file(fname: &String) -> Result<PubKey, Error> {
    let mut key_file = File::open(fname)?;
    let mut contents = String::new();
    key_file.read_to_string(&mut contents).unwrap();
    let pkey = PKey::public_key_from_pem(contents.as_bytes()).unwrap();
    Ok(PubKey{pkey})
  }

  pub fn from_pem(pem: &[u8]) -> Result<PubKey, Error> {
    Ok(PubKey::new(PKey::public_key_from_pem(pem)?))
  }

  pub fn from_der(der: &[u8]) -> Result<PubKey, Error> {
    Ok(PubKey::new(PKey::public_key_from_pem(
      format!("-----BEGIN PUBLIC KEY-----\n{}\n-----END PUBLIC KEY-----",
              base64::encode(der)).into_bytes().as_slice())?))
  }

  pub fn from_point(bytes: &[u8]) -> Result<PubKey, Error> {
    let group = EcGroup::from_curve_name(CURVE)?;
    let mut ctx = BigNumContext::new()?;
    let point = EcPoint::from_bytes(&group, bytes, &mut ctx)?;
    Ok(PubKey::new(PKey::from_ec_key(EcKey::from_public_key(&group, &point)?)?))
  }

  pub fn pub_to_der(&self) -> Result<Vec<u8>, Error> {
    Ok(self.pkey.public_key_to_der()?)
  }

  pub fn verify(&self, digest: MessageDigest, msg: &[u8], sig: &[u8]) -> Result<bool, Error> {
    let mut verifier = Verifier::new(digest, &self.pkey)?;
    verifier.update(msg)?;
    Ok(verifier.finish(sig)?)
  }

  pub fn to_point_bytes(&self) -> Result<Vec<u8>, Error> {
    let group = EcGroup::from_curve_name(CURVE)?;
    let mut ctx = BigNumContext::new()?;
    let eckey = self.pkey.ec_key()?;
    let point = match eckey.public_key() {
      Some(v) => v,
      None => {
        return Err(Error::StrErr("Something went horribly wrong.".to_string()));
      }
    };
    Ok(point.to_bytes(&group, POINT_CONVERSION_UNCOMPRESSED, &mut ctx)?)
  }
}
