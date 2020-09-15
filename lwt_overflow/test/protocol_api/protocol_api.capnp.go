// Code generated by capnpc-go. DO NOT EDIT.

package protocol_api

import (
	capnp "zombiezen.com/go/capnproto2"
	text "zombiezen.com/go/capnproto2/encoding/text"
	schemas "zombiezen.com/go/capnproto2/schemas"
)

type EchoTest struct{ capnp.Struct }

// EchoTest_TypeID is the unique identifier for the type EchoTest.
const EchoTest_TypeID = 0xad87dbf7b9941657

func NewEchoTest(s *capnp.Segment) (EchoTest, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 0, PointerCount: 1})
	return EchoTest{st}, err
}

func NewRootEchoTest(s *capnp.Segment) (EchoTest, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 0, PointerCount: 1})
	return EchoTest{st}, err
}

func ReadRootEchoTest(msg *capnp.Message) (EchoTest, error) {
	root, err := msg.RootPtr()
	return EchoTest{root.Struct()}, err
}

func (s EchoTest) String() string {
	str, _ := text.Marshal(0xad87dbf7b9941657, s.Struct)
	return str
}

func (s EchoTest) Text() (string, error) {
	p, err := s.Struct.Ptr(0)
	return p.Text(), err
}

func (s EchoTest) HasText() bool {
	p, err := s.Struct.Ptr(0)
	return p.IsValid() || err != nil
}

func (s EchoTest) TextBytes() ([]byte, error) {
	p, err := s.Struct.Ptr(0)
	return p.TextBytes(), err
}

func (s EchoTest) SetText(v string) error {
	return s.Struct.SetText(0, v)
}

// EchoTest_List is a list of EchoTest.
type EchoTest_List struct{ capnp.List }

// NewEchoTest creates a new list of EchoTest.
func NewEchoTest_List(s *capnp.Segment, sz int32) (EchoTest_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 0, PointerCount: 1}, sz)
	return EchoTest_List{l}, err
}

func (s EchoTest_List) At(i int) EchoTest { return EchoTest{s.List.Struct(i)} }

func (s EchoTest_List) Set(i int, v EchoTest) error { return s.List.SetStruct(i, v.Struct) }

func (s EchoTest_List) String() string {
	str, _ := text.MarshalList(0xad87dbf7b9941657, s.List)
	return str
}

// EchoTest_Promise is a wrapper for a EchoTest promised by a client call.
type EchoTest_Promise struct{ *capnp.Pipeline }

func (p EchoTest_Promise) Struct() (EchoTest, error) {
	s, err := p.Pipeline.Struct()
	return EchoTest{s}, err
}

const schema_b604ad1fc1bf9052 = "x\xda\x12Pq`\x12d\x8dg`\x08dae\xfb" +
	"\x1f.6e\xe7\xf7\xdb\xedk\x19\x04\x85\x19\xff\x07M" +
	"\xd8\x7fP~-\xcb6\x06VFv\x06\x06A\xd1G" +
	"\x82\x8a Z\xd6\x9e\x81\xf1\x7fAQ~I~r~" +
	"\x0eS|bA\xa6^rbA^\x81\x95krF" +
	"~\x08{jqI\x00#c \x0b3\x0b\x03\x03\x0b" +
	"#\x03\x83 \xaf\x16\x03C \x073c\xa0\x08\x13#" +
	"\x7fIjE\x09#\x0f\x03\x13#\x0f\x03# \x00\x00" +
	"\xff\xff&y\x1d0"

func init() {
	schemas.Register(schema_b604ad1fc1bf9052,
		0xad87dbf7b9941657)
}
