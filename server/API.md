## GET /ping

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
Success!
```

## POST /proof

#### Relevant Schemas

SignedLocnProof is the protobuf in the request body and follows:

```
message SignedLocnProof {
  required LocnProof locnproof = 1;
  required bytes sig = 2;
}
```

```
message LocnProof {
  required bytes vault_key = 1;
  required bytes uid = 2;
  required bytes unonce = 3;
  required bytes apid = 4;
  required bytes apnonce = 5;
  required fixed64 time = 6;
}
```

SignedToken is the protobuf in the response and follows:

```
message SignedToken {
  required Token token = 1;
  required bytes sig = 2;
}
```

```
message Token {
  required bytes vnonce = 1;
  required bytes locn_tag = 2;
}
```

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/x-protobuf`

- Example: `application/x-protobuf`

```
0A200A033132331204313233341A01782204353637382A0179314000000000000000120130
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/x-protobuf`

- Response body as below.

```
0A090A0461626364120130120130
```

## POST /vault

#### Relevant Schemas

SignedVaultMsg is the protobuf in the request body and follows:

```
message SignedVaultMsg {
  required VaultMsg vault_msg = 1;
  required bytes sig = 2;
}
```

```
message VaultMsg {
  required Vault vault = 1;
  required bytes uid = 2;
  required bytes unonce = 3;
  required bytes apid = 4;
  required bytes apnonce = 5;
  required fixed64 time = 6;
}
```

```
message Vault {
  required Point points = 1;
}
```

```
message Point {
  required uint32 x = 1;
  required uint32 y = 2;
}
```

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/x-protobuf`

- Example: `application/x-protobuf`

```
0A290A0C0A04080110020A04080310041204313233341A01782204353637382A0179314000000000000000120130
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
VaultMsg {vault = RM {unRM = Field {runField = Required {runRequired = Always {runAlways = Message {runMessage = Vault {points = Field {runField = Repeated {runRepeated = [Message {runMessage = Point {x = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = 1}}}}}, y = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = 2}}}}}}},Message {runMessage = Point {x = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = 3}}}}}, y = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = 4}}}}}}}]}}}}}}}}, uid = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = "1234"}}}}}, unonce = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = "x"}}}}}, apid = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = "5678"}}}}}, apnonce = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = "y"}}}}}, time = RV {unRV = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = Fixed 64}}}}}}
```

