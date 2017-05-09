use std::process::Command;

fn main() {
  println!("cargo:rerun-if-changed=../protos/messages.proto");
  let output = Command::new("protoc")
    .arg("--rust_out")
    .arg("src")
    .arg("-I").arg("../protos")
    .arg("../protos/messages.proto")
    .output().unwrap();
  println!("{:?}", output.stderr);
}
