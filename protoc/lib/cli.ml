type msg = {
  name:  string [@key 1];
  value: int    [@key 2];
} [@@deriving protobuf { protoc }]
