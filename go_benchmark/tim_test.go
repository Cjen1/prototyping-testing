package main

import (
	"sync"
	"testing"
	"go.etcd.io/etcd/clientv3"
	"log"
	"time"
)

func check(e error) {
	if e != nil {
		log.Fatal(e)
	}
}

func BenchmarkTime(b *testing.B) {
	eps := []string{"http://127.0.0.1:2379"}
	nc := 100
	clients := make([]*clientv3.Client, nc)
	for i:= 0; i < nc; i++ {
		cli, err := clientv3.New(clientv3.Config{
			Endpoints:eps,
		})
		check(err)
		clients[i] = cli
	}

	var wg sync.WaitGroup
	for n := 0; n < b.N; n++ {
		wg.Add(1)
		if true {
		time.Sleep(100*time.Nanosecond)
	}
		go func(n int) {
			defer wg.Done()
			time_call(clients[n%nc])
		} (n)
	}
	wg.Wait()
}
