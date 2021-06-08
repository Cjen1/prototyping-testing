package main

import (
	"fmt"
	"os"
	"bufio"
	"io"
	"encoding/binary"
	"encoding/hex"
	"log"
)

func init(){
	log.SetPrefix("Client: ")
	log.SetFlags(log.Ldate | log.Lmicroseconds | log.Llongfile)
	log.Println("init started")
}

func main() {
	log.Println("Client: Starting client")

	reader := bufio.NewReader(os.Stdin)

	for {
		log.Println("Trying to read len")
		var size uint32
		if err := binary.Read(reader, binary.LittleEndian, &size); err != nil {
			log.Panicf("Failed to read len, got err: %v", err)
		}

		fmt.Printf("Size = %v\n", size)

		payload := make([]byte, size)
		if _, err := io.ReadFull(reader, payload); err != nil {
			log.Panicf("Failed to read payload, got err: %v", err)
		}
		fmt.Printf("payload = \n%s\n-------", (hex.EncodeToString(payload)))
	}
}
