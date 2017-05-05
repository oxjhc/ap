#![macro_use]

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
