<?php

include "vendor/autoload.php";

include(__DIR__ . "/Grpc_chat/ChatServiceClient.php");
include(__DIR__ . "/Grpc_chat/Count.php");
include(__DIR__ . "/Grpc_chat/Msg.php");
include(__DIR__ . "/Grpc_chat/Names.php");
include(__DIR__ . "/Grpc_chat/TlsServiceClient.php");
include(__DIR__ . "/GPBMetadata/GrpcChat.php");
include(__DIR__ . "/GPBMetadata/Tls.php");


$override = "a.com";
$tlsClient = new Grpc_chat\TlsServiceClient("127.0.0.1:8080",
    [
        "credentials" => Grpc\ChannelCredentials::createSsl(file_get_contents(__DIR__ . "/../server.pem")),
        'grpc.ssl_target_name_override' => $override,
        'grpc.default_authority' => $override,
    ]
);

$msg = new Grpc_chat\Msg();
$msg->setSender("phpclient");
$msg->setMsg("hello");
list($reply, $status) = $tlsClient->SendMsg($msg, ["auth"=>["pass1234"]])->wait();
var_dump($reply->getSender(), $reply->getMsg(), $status);

