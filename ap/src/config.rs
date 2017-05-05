extern crate toml;

use std::collections::BTreeMap as Map;
use std::fs::File;
use std::io::Read;

use self::toml::Value;

use crypto::PrivKey;
use error::Error;

pub struct Config {
  pub key: PrivKey,
  pub ping_port: i64,
  pub http_port: i64,
  pub _t_vid_file: String
}

impl Config {
  pub fn new() -> Result<Config, Error> {
    let cfgv = match Config::read_cfg() {
      Ok(res) => res,
      Err(err) => panic!("Error reading config: {:?}", err)
    };

    match cfgv.get("config") {
      Some(v) => match v.as_table() {
        Some(keys) =>
          Ok(Config{
            key: match keys.get("privkey") {
              Some(v) => match v.as_str() {
                Some(key_fname) => {
                  PrivKey::from_file(key_fname).unwrap()
                },
                None => panic!("Error reading config: privkey isn't a string!")
              },
              None => panic!("Error reading config: no privkey provided")
            },

            ping_port: match keys.get("ping_port") {
              Some(v) => match v.as_integer() {
                Some(v) => Ok(v),
                None =>
                  Err(Error::cfg_err("Error reading config: ping_port isn't an integer!"))
              },
              None => Ok(1832)
            }?,

            http_port: match keys.get("http_port") {
              Some(v) => match v.as_integer() {
                Some(v) => Ok(v),
                None =>
                  Err(Error::cfg_err("Error reading config: http_port isn't an integer!"))
              },
              None => Ok(80)
            }?,

            _t_vid_file: match keys.get("_t_verif_pubkey") {
              Some(v) => match v.as_str() {
                Some(v) => Ok(v),
                None => Err(Error::cfg_err("Error reading config: _t_verif_pubkey isn't a string"))
              },
              None => Ok("")
            }?.to_string()
          }),
        None => Err(Error::cfg_err("'key' should be a table"))
      },
      None => Err(Error::cfg_err("Error reading config: section 'config' does not exist"))
    }
  }

  fn read_cfg() -> Result<Map<String, Value>, Error> {
    let mut cfg_file = File::open("/etc/dormouse/config.toml")?;
    let mut contents = String::new();
    cfg_file.read_to_string(&mut contents)?;
    match contents.parse::<Value>() {
      Ok(res) => match res.as_table() {
        Some(table) => Ok(table.clone()),
        None => Err(Error::cfg_err("not a table"))
      },
      Err(err) => Err(Error::TomlDeErr(err))
    }
  }
}
