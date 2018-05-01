<?php
include "vendor/autoload.php";

include(__DIR__ . "/Grpc_chat/ChatServiceClient.php");
include(__DIR__ . "/Grpc_chat/Count.php");
include(__DIR__ . "/Grpc_chat/Msg.php");
include(__DIR__ . "/Grpc_chat/Names.php");
include(__DIR__ . "/Grpc_chat/TlsServiceClient.php");
include(__DIR__ . "/GPBMetadata/GrpcChat.php");
include(__DIR__ . "/GPBMetadata/Tls.php");

$client = new Grpc_chat\ChatServiceClient("127.0.0.1:8080", ["credentials" => Grpc\ChannelCredentials::createInsecure()]);
$p = new Grpc_chat\Names();
$p->setFirstName("LL");
$p->setSecondName("EEE");

list($reply, $status) = $client->ExchangeNames($p)->wait();
var_dump($status, $reply->getFirstName(), $reply->getSecondName());