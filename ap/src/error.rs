extern crate std;
extern crate toml;
extern crate openssl;

use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Error {
  ConfErr(String),
  IoErr(std::io::Error),
  TomlDeErr(toml::de::Error),
  OpenSslErr(openssl::error::ErrorStack),
  StrErr(String)
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

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
    match self {
      &Error::ConfErr(ref s) => s.fmt(f),
      &Error::IoErr(ref e) => e.fmt(f),
      &Error::TomlDeErr(ref e) => e.fmt(f),
      &Error::OpenSslErr(ref e) => e.fmt(f),
      &Error::StrErr(ref s) => s.fmt(f)
    }
  }
}

