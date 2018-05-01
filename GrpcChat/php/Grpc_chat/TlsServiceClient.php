<?php
// GENERATED CODE -- DO NOT EDIT!

namespace Grpc_chat;

/**
 */
class TlsServiceClient extends \Grpc\BaseStub {

    /**
     * @param string $hostname hostname
     * @param array $opts channel options
     * @param \Grpc\Channel $channel (optional) re-use channel object
     */
    public function __construct($hostname, $opts, $channel = null) {
        parent::__construct($hostname, $opts, $channel);
    }

    /**
     * @param \Grpc_chat\Msg $argument input argument
     * @param array $metadata metadata
     * @param array $options call options
     */
    public function SendMsg(\Grpc_chat\Msg $argument,
      $metadata = [], $options = []) {
        return $this->_simpleRequest('/grpc_chat.TlsService/SendMsg',
        $argument,
        ['\Grpc_chat\Msg', 'decode'],
        $metadata, $options);
    }

}
