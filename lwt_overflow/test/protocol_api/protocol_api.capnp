using Go = import "/go.capnp";
$Go.package("protocol_api");
$Go.import("protocol_api");

@0xb604ad1fc1bf9052;

struct EchoTest {
  text @0 : Text;
}
