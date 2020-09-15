package main

import (
	"fmt"
	"log"
	"time"
	capnp "zombiezen.com/go/capnproto2"
	api "./protocol_api"
	conn "github.com/Cjen1/rc_op_go/capnp_go_conn"
)

func main() {
	log.Printf("Starting test")
	client := conn.Create_PersistConn("127.0.0.1:5000", 2)

	{
		log.Printf("Sending first message")
		init_msg, seg, err := capnp.NewMessage(capnp.SingleSegment(nil))
		if err != nil {
			panic(err)
		}
		msg, err := api.NewRootEchoTest(seg)
		msg.SetText("asdf")
		client.Write(init_msg)
	}

	{
		log.Printf("Receiving first message")
		msg_capnp := client.Read()
		log.Printf("Received")
		msg, _ := api.ReadRootEchoTest(msg_capnp)
		target := "test_msg"
		msg_text, _ := msg.Text()
		if msg_text != target{
			log.Printf("Failed to get correct input got %s instead\n", msg_text)
			init_resp, seg, err := capnp.NewMessage(capnp.SingleSegment(nil))
			if err != nil {
				panic(err)
			}
			resp, err := api.NewRootEchoTest(seg)
			resp.SetText(fmt.Sprintf("Got %s instead of %s", msg_text, target))
			client.Write(init_resp)
		}

		log.Printf("Correct msg %s received sending response", msg_text)
		init_resp, seg, _ := capnp.NewMessage(capnp.SingleSegment(nil))
		resp, _ := api.NewRootEchoTest(seg)
		resp.SetText("success")
		client.Write(init_resp)
		log.Printf("Dispatched response");
	}
	time.Sleep(10 * time.Second)
	log.Printf("Exiting")
}
