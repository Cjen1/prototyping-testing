package main

import (
	"go.etcd.io/etcd/clientv3"
	"context"
)

func time_call(cli *clientv3.Client) {
	if true {cli.Put(context.Background(), "key", "value")
}
}
