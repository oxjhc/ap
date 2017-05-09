extern crate std;
extern crate wpactrl;

use std::thread;

use self::wpactrl::WpaCtrl;

use error::Error;

pub struct P2pCtrl{}

impl P2pCtrl {
  pub fn new(ctrl_path: &String, name: &String) -> Result<P2pCtrl, Error> {
    let ctrl = WpaCtrl::new(ctrl_path)?;
    let res = ctrl.request(
      &format!("P2P_SERVICE_ADD upnp 10 uuid:{}::urn:schemas-oxjhc-club:service:TeaParty:1", name),
      None
    )?;
    if res != "ok" {
      return Err(Error::StrErr(
        format!("Failed to add service. wpa_supplicant returned {}", res))
      );
    }

    let child = thread::spawn(move || {

    });

    Ok(P2pCtrl{})
  }
}
