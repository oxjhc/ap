extern crate untrusted;
extern crate ring;
extern crate openssl;
extern crate base64;

use std::fs::File;
use std::io::Read;

use self::untrusted::Input;

use self::ring::{agreement, rand, pbkdf2, aead};
use self::ring::agreement::EphemeralPrivateKey;
use self::ring::aead::SealingKey;
use self::ring::rand::SecureRandom;

use self::openssl::bn::BigNumContext;
use self::openssl::pkey::PKey;
use self::openssl::sign::{Signer, Verifier};
use self::openssl::nid::X9_62_PRIME256V1 as CURVE;
use self::openssl::ec::{POINT_CONVERSION_UNCOMPRESSED, EcPoint, EcGroup, EcKey};
use self::openssl::hash::MessageDigest;

use error::Error;

pub fn gen_nonce() -> Vec<u8> {
  let mut nonce = vec![0x42; 10];
  let rand = rand::SystemRandom::new();
  match rand.fill(nonce.as_mut_slice()) {
    Ok(_) => (),
    Err(e) => {
      println!("Failed to create nonce: {}", e);
    }
  }
  nonce
}

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

  pub fn sign(&self, msg: &[u8]) -> Result<Vec<u8>, Error> {
    let mut signer = Signer::new(MessageDigest::sha256(), &self.pkey).unwrap();
    signer.update(msg)?;
    Ok(signer.finish()?)
  }
}

pub struct EphKey {
  key: EphemeralPrivateKey,
  pubkey: PubKey
}

impl EphKey {
  pub fn new() -> Result<EphKey, Error> {
    let rand = rand::SystemRandom::new();
    let key = agreement::EphemeralPrivateKey::generate(&agreement::ECDH_P256, &rand)?;
    let mut pubept = vec![0u8; key.public_key_len()];
    key.compute_public_key(&mut pubept)?;
    let pubkey = PubKey::from_point(pubept.as_slice())?;
    Ok(EphKey{key, pubkey})
  }

  pub fn pub_to_der(&self) -> Result<Vec<u8>, Error> {
    self.pubkey.pub_to_der()
  }

  fn ecdh(self, vid: &PubKey, nonce: &[u8]) -> Result<SealingKey, Error> {
    let bytes = vid.point_bytes()?;
    let vidin = Input::from(bytes.as_slice());
    Ok(agreement::agree_ephemeral(
      self.key, &agreement::ECDH_P256, vidin, ring::error::Unspecified,
      |_key_material| {
        let mut key = vec![0; 32]; //digest::SHA256.output_len
        pbkdf2::derive(&pbkdf2::HMAC_SHA256, 30, nonce,
                       _key_material, key.as_mut_slice());
        //printhex!(&key);
        SealingKey::new(&aead::AES_256_GCM, key.as_slice())
      })?)
  }

  pub fn seal(self, data: &[u8], vid: &PubKey, nonce: &[u8]) -> Result<Vec<u8>, Error> {
    let mut data = data.to_vec();
    data.extend(vec![0; 16]);

    let key = self.ecdh(vid, nonce)?;

    match aead::seal_in_place(&key, &[0; 12], &[], data.as_mut_slice(), 16) {
      Ok(len) => Ok(data[..len].to_vec()),
      Err(e) => Err(From::from(e))
    }
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

  pub fn verify(&self, msg: &[u8], sig: &[u8]) -> Result<bool, Error> {
    let mut verifier = Verifier::new(MessageDigest::sha256(), &self.pkey)?;
    verifier.update(msg)?;
    Ok(verifier.finish(sig)?)
  }

  pub fn point_bytes(&self) -> Result<Vec<u8>, Error> {
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
