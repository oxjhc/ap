extern crate std;
extern crate toml;
extern crate ring;
extern crate openssl;
extern crate hyper;

use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Error {
  CfgErr(String),
  IoErr(std::io::Error),
  TomlDeErr(toml::de::Error),
  RingErr(ring::error::Unspecified),
  OpenSslErr(openssl::error::ErrorStack),
  HyperErr(hyper::Error),
  StrErr(String)
}

impl Error {
  pub fn cfg_err(str: &str) -> Error {
    Error::CfgErr(str.to_string())
  }

  pub fn str_err(str: &str) -> Error {
    Error::StrErr(str.to_string())
  }
}

impl From<std::io::Error> for Error {
  fn from(err: std::io::Error) -> Self {
    Error::IoErr(err)
  }
}

impl From<toml::de::Error> for Error {
  fn from(err: toml::de::Error) -> Self {
    Error::TomlDeErr(err)
  }
}

impl From<openssl::error::ErrorStack> for Error {
  fn from(err: openssl::error::ErrorStack) -> Self {
    Error::OpenSslErr(err)
  }
}

impl From<ring::error::Unspecified> for Error {
  fn from(err: ring::error::Unspecified) -> Self {
    Error::RingErr(err)
  }
}

impl From<hyper::Error> for Error {
  fn from(err: hyper::Error) -> Self {
    Error::HyperErr(err)
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
    match self {
      &Error::CfgErr(ref s) => s.fmt(f),
      &Error::IoErr(ref e) => e.fmt(f),
      &Error::TomlDeErr(ref e) => e.fmt(f),
      &Error::RingErr(ref e) => e.fmt(f),
      &Error::OpenSslErr(ref e) => e.fmt(f),
      &Error::HyperErr(ref e) => e.fmt(f),
      &Error::StrErr(ref s) => s.fmt(f)
    }
  }
}

