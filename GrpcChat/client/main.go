package main

import (
	"google.golang.org/grpc"
	"flag"
	pb "grpc_chat/grpc_chat"
	"log"
	"time"
	"context"
	"io"
	"fmt"
	"strconv"
	"os"
)

var (
	Server  string // 服务端ip地址
	MyNames = &pb.Names{} // 我的名字
	client  pb.ChatServiceClient // rpc client
)

func init() {
	flag.StringVar(&Server, "server", "127.0.0.1:8080", "grpc server address")
	flag.StringVar(&MyNames.FirstName, "fn", "", "first name")
	flag.StringVar(&MyNames.SecondName, "sn", "", "second name")
	flag.Parse()

	if conn, err := grpc.Dial(Server, grpc.WithInsecure()); err != nil {
		log.Fatal("connect to server failed", err)
	} else {
		client = pb.NewChatServiceClient(conn)
	}
}

// 用于发送消息的chan
var sendMsg = make(chan string, 10);

// 聊天双协程实现
func chat() {
	stream, err := client.Chat(context.Background())
	if err != nil {
		log.Fatalf("%v.Chat(_) = _, %v", client, err)
	}
	waitc := make(chan struct{})
	// 读协程
	go func() {
		for {
			in, err := stream.Recv()
			if err == io.EOF {
				close(waitc)
				return
			}
			if err != nil {
				log.Fatalf("Failed to receive a msg : %v", err)
			}
			log.Printf(">>>>%s:%s", in.GetSender(), in.GetMsg())
		}
	}()
	// 写协程
loop2:
	for {
		select {
		case s := <-sendMsg:
			if s == "exit" {
				stream.CloseSend();
				return;
			}
			stream.Send(&pb.Msg{
				MyNames.FirstName + "." + MyNames.SecondName,
				s,
			})
		case <-waitc:
			break loop2
		}
	}
}

// 获取服务端列表
func get() {
	stream, err := client.GetList(context.Background(), &pb.Count{10, 0});
	if err != nil {
		log.Fatalf("getlist error :%v", err)
	}
	for {
		in, err := stream.Recv()
		if err == io.EOF {
			return
		}
		if err != nil {
			log.Printf("Failed to receive a list : %v", err)
			return
		}
		log.Printf("==names===%s:%s", in.FirstName, in.SecondName)
	}
}

// 发送一个假的列表
func send() {
	stream, err := client.SendList(context.Background())
	if err != nil {
		log.Fatalf("send list error :%v", err)
	}
	for i := 0; i < 10; i++ {
		err := stream.Send(&pb.Names{strconv.Itoa(i), strconv.Itoa(i)})
		if err != nil {
			log.Print("send error:", err)
			return
		}
	}
	c, err := stream.CloseAndRecv()
	if err != nil {
		log.Println("closeAndRecv failed:", err)
	} else {
		log.Println("send success", c.Count, c.Offset);
	}
}

// 输入指令，由于scan需要读取到两个串，所以输入时，即使像exit这样没有参数的指令，也要输入两个单词才行
func input() {
	for {
		var cmd, s string
		fmt.Scan(&cmd, &s)
		fmt.Println(cmd, s);
		switch cmd {
		case "get":
			get()
		case "send":
			send()
		case "msg":
			sendMsg <- s;
		case "exit":
			os.Exit(0);
		}
	}
}

func main() {
	// 上来先打招呼
	ctx, _ := context.WithTimeout(context.Background(), 1*time.Second)
	sNames, err := client.ExchangeNames(ctx, MyNames);
	if err != nil {
		log.Fatalf("%v.ExchangeNames(_) = _, %v", client, err)
	}
	log.Printf("greet ok, server name: %s %s\n", sNames.FirstName, sNames.SecondName)

	go input()

	chat()
}
