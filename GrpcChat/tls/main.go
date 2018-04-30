package main

import (
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc"
	"log"
	pb "grpc_chat/grpc_chat"

	"net"
	"google.golang.org/grpc/grpclog"
	"context"
	"fmt"
	"time"
	"google.golang.org/grpc/metadata"
	"errors"
)

type tlsService struct{}

func unaryInterceptor(ctx context.Context, req interface{}, info *grpc.UnaryServerInfo, handler grpc.UnaryHandler) (interface{}, error) {
	md, ok := metadata.FromIncomingContext(ctx);
	if !ok {
		return nil, errors.New("unaryInterceptor FromIncomingContext not ok ")
	}else if pass, ok := md["lqs"]; !ok || pass[0] != "pass123" {
		return nil, errors.New("auth failed");
	}else{
		log.Println("auth succ")
	}
	return handler(ctx, req)
}


func (ts *tlsService) SendMsg(ctx context.Context, msg *pb.Msg) (*pb.Msg, error) {
	md, err := metadata.FromIncomingContext(ctx);
	log.Printf("%s >>> %s, %v, %v\n", msg.Sender, msg.Msg, md, err);
	return &pb.Msg{"server", "receive"}, nil
}

func server() {
	listen, err := net.Listen("tcp", ":8080")
	if err != nil {
		grpclog.Fatalf("failed to listen: %v", err)
	}

	// TLS认证
	creds, err := credentials.NewServerTLSFromFile("./server.pem", "./server.key")
	if err != nil {
		grpclog.Fatalf("Failed to generate credentials %v", err)
	}

	// 实例化grpc Server, 并开启TLS认证
	s := grpc.NewServer(grpc.Creds(creds), grpc.UnaryInterceptor(unaryInterceptor))

	// 注册HelloService
	pb.RegisterTlsServiceServer(s, &tlsService{})

	go s.Serve(listen)
}

func client() {
	creds, err := credentials.NewClientTLSFromFile("./server.pem", "a.com");
	if err != nil {
		log.Fatalln("NewClientTLSFromFile error", err)
	}
	conn, _ := grpc.Dial("localhost:8080", grpc.WithTransportCredentials(creds))
	// error handling omitted
	client := pb.NewTlsServiceClient(conn)
	ctx := metadata.NewOutgoingContext(context.Background(), metadata.Pairs("auth", "pass1234"))
	msg, err := client.SendMsg(ctx, &pb.Msg{"client", "hello server"})
	fmt.Println(msg, err)
}

func main() {
	server();
	time.Sleep(time.Second)
	client();
}
