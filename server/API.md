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

LocnProof is the protobuf in the request body and follows:

```
message LocnProof' {
  required bytes vault_key = 1;
  required bytes uid = 2;
  required bytes unonce = 3;
  required bytes apid = 4;
  required bytes apnonce = 5;
  required fixed64 time = 6;
  required bytes sig = 7;
}
```

Token is the protobuf in the response and follows:

```
message Token' {
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
0A060000000100021204313233341A01782204353637382A01793140000000000000003A026D65
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/x-protobuf`

- Response body as below.

```
0A04616263641200
```

## POST /vault

#### Relevant Schemas

VaultMsg is the protobuf in the request body and follows:

```
message VaultMsg' {
  required Vault vault = 1;
  required bytes uid = 2;
  required bytes unonce = 3;
  required bytes apid = 4;
  required bytes apnonce = 5;
  required fixed64 time = 6;
  required bytes sig = 7;
}
```

```
message Vault' {
  required Point points = 1;
}
```

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/x-protobuf`

- Example: `application/x-protobuf`

```
0A0C0A04080110020A04080310041204313233341A01782204353637382A01793140000000000000003A026D65
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
VaultMsg {vault = Vault [(1,2),(3,4)], uid = "1234", unonce = "x", apid = "5678", apnonce = "y", time = 64, sig = "me"}
```

