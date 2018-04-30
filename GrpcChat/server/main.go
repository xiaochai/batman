package main

import (
	pb "grpc_chat/grpc_chat"
	"context"
	"log"
	"google.golang.org/grpc/reflection"
	"net"
	"google.golang.org/grpc"
	"flag"
	"sync"
	"errors"
	"io"
	"fmt"
	"strings"
)

//go:generate protoc -I ../grpc_chat --go_out=plugins=grpc:../grpc_chat ../grpc_chat/tls.proto ../grpc_chat/grpc_chat.proto

type chatService struct{}

// 朋友列表，在ExchangeNames的时候添加
var MyFriends = []*pb.Names{}
var MyFriendsLock = sync.RWMutex{}

// 交换名字
func (cs *chatService) ExchangeNames(ctx context.Context, names *pb.Names) (*pb.Names, error) {
	MyFriends = append(MyFriends, names);
	log.Printf("%s %s is comming for exchange names\n", names.FirstName, names.SecondName)
	return MyNames, nil;
}

// 获取所有与服务端交换过名字的用户列表
func (cs *chatService) GetList(c *pb.Count, gls pb.ChatService_GetListServer) error {
	MyFriendsLock.RLock();
	defer MyFriendsLock.RUnlock();
	if c.Offset > int32(len(MyFriends)) {
		return errors.New("no more friends")
	}
	count := c.Offset + c.Count;
	if count > int32(len(MyFriends)) {
		count = int32(len(MyFriends))
	}
	sendNames := MyFriends[c.Offset:count]
	for _, names := range sendNames {
		if err := gls.Send(names); err != nil {
			return err;
		}
	}
	return nil
}

// 实际无意义，用来接收客户端发上来的列表
func (cs *chatService) SendList(sls pb.ChatService_SendListServer) error {
	var ReciveNames []*pb.Names;
	for {
		names, err := sls.Recv()
		if err == io.EOF {
			return sls.SendAndClose(&pb.Count{
				int32(len(ReciveNames)),
				0,
			})
		}
		if err != nil {
			return err
		}
		ReciveNames = append(ReciveNames, names)
		log.Println("send :", names.FirstName, names.SecondName)
	}
}

// 用于记录有多少客户端在chat
type Clients struct {
	sync.RWMutex
	Msg map[pb.ChatService_ChatServer]chan *pb.Msg
}
var clients = &Clients{Msg: map[pb.ChatService_ChatServer]chan *pb.Msg{}}

func (c *Clients) add(scs pb.ChatService_ChatServer, sendMsg chan *pb.Msg) {
	clients.Lock()
	clients.Msg[scs] = sendMsg
	clients.Unlock()
}

func (c *Clients) del(scs pb.ChatService_ChatServer) {
	clients.Lock()
	delete(clients.Msg, scs)
	clients.Unlock()
}

// 聊天双向流rpc
func (cs *chatService) Chat(scs pb.ChatService_ChatServer) error {
	sendMsg := make(chan *pb.Msg, 10)
	clients.add(scs, sendMsg)
	defer func() {
		clients.del(scs)
	}()
	waitc := make(chan struct{})
	go func() {
		for {
			in, err := scs.Recv()
			if err == io.EOF {
				close(waitc)
				return
			}
			if err != nil {
				log.Printf("receive error : %v\n", err)
				close(waitc)
				return
			}
			log.Printf(">>>>%s:%s\n", in.GetSender(), in.GetMsg())
			broadMsg(in)
		}
	}()
loop2:
	for {
		select {
		case s := <-sendMsg:
			scs.Send(s)
		case <-waitc:
			break loop2
		}
	}
	return nil
}

// 开启rpc服务所需要的端口
var Listen = ":8080"

// 开启rpc服务
func startService() {
	lis, err := net.Listen("tcp", Listen)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	pb.RegisterChatServiceServer(s, &chatService{})
	reflection.Register(s)
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}

// 我的名字
var MyNames = &pb.Names{}

func init() {
	flag.StringVar(&MyNames.FirstName, "fn", "", "first name")
	flag.StringVar(&MyNames.SecondName, "sn", "", "second name")
	flag.StringVar(&Listen, "l", ":8080", "listen address")
	flag.Parse()
}

func main() {
	go inputMsg();
	go startService();
	select {};
}

// 收集输入，广播消息
func inputMsg() {
	for {
		var s string;
		fmt.Scanln(&s)
		if strings.Trim(s, "\n\r ") != "" {
			broadMsg(&pb.Msg{MyNames.FirstName + "." + MyNames.SecondName, s})
		}
	}
}

// 将消息广播到所有连接上
func broadMsg(s *pb.Msg) {
	clients.RLock()
	for _, v := range clients.Msg {
		select {
		case v <- s:
		default:
			log.Println("busy! Wait for a second");
		}
	}
	clients.RUnlock()
}
