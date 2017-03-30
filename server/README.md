# Server
The server can send and receive protobufs via RESTful endpoints. The datatype
for the protobuf is not automatically updated from the .proto file, but there
are strong type level guarantees.

# Building
To build the server,

1. Install `stack`, a common Haskell build tool. Information can be found
   [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Run `stack setup`. This sets up `stack`'s sandbox which holds specific
   versions of GHC and packages.
3. Run `stack build`. This will build all the targets listed in `server.cabal`.

# Running
To run the server, run `stack exec server-exe`. `server-exe` is the name of an
executable target in `server.cabal`. The server currently runs at port 8080 on
localhost. You can use `curl` etc. to interact with it.
