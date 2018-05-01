<?php
// GENERATED CODE -- DO NOT EDIT!

namespace Grpc_chat;

/**
 */
class ChatServiceClient extends \Grpc\BaseStub {

    /**
     * @param string $hostname hostname
     * @param array $opts channel options
     * @param \Grpc\Channel $channel (optional) re-use channel object
     */
    public function __construct($hostname, $opts, $channel = null) {
        parent::__construct($hostname, $opts, $channel);
    }

    /**
     * @param \Grpc_chat\Names $argument input argument
     * @param array $metadata metadata
     * @param array $options call options
     */
    public function ExchangeNames(\Grpc_chat\Names $argument,
      $metadata = [], $options = []) {
        return $this->_simpleRequest('/grpc_chat.ChatService/ExchangeNames',
        $argument,
        ['\Grpc_chat\Names', 'decode'],
        $metadata, $options);
    }

    /**
     * @param \Grpc_chat\Count $argument input argument
     * @param array $metadata metadata
     * @param array $options call options
     */
    public function GetList(\Grpc_chat\Count $argument,
      $metadata = [], $options = []) {
        return $this->_serverStreamRequest('/grpc_chat.ChatService/GetList',
        $argument,
        ['\Grpc_chat\Names', 'decode'],
        $metadata, $options);
    }

    /**
     * @param array $metadata metadata
     * @param array $options call options
     */
    public function SendList($metadata = [], $options = []) {
        return $this->_clientStreamRequest('/grpc_chat.ChatService/SendList',
        ['\Grpc_chat\Count','decode'],
        $metadata, $options);
    }

    /**
     * @param array $metadata metadata
     * @param array $options call options
     */
    public function Chat($metadata = [], $options = []) {
        return $this->_bidiRequest('/grpc_chat.ChatService/Chat',
        ['\Grpc_chat\Msg','decode'],
        $metadata, $options);
    }

}
