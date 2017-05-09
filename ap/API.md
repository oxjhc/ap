## POST /proofreq

#### Relevant Schemas

ProofReq is the protobuf in the request body and follows:

```
message ProofReq {
  required bytes uid = 1;
  required bytes unonce = 2;
  required int64 seqid = 3;
  required bytes vid = 4;
  required bytes sig = 5;
}
```

ProofResp is the protobuf in the response and follows:

```
message ProofResp {
  required bytes uid = 1;
  required bytes unonce = 2;
  required bytes sig = 3;
}
```

Clients must supply the following data

#### Request:

- Supported content types are:
    - application/x-protbuf
- Example: application/x-protobuf (to be added)

#### Response:

- Status code 200
- Headers: []
- Supported content types are:
    - application/x-protobuf
- Response body as below. (to be added)
