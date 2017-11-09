<?php
include_once "vendor/autoload.php";
use \Phplib\FileLineIterator;

$t = new FileLineIterator(__DIR__ . "/testRepo.php");
foreach($t as $k=>$v){
	var_dump($k, $v);
}
