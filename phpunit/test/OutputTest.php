<?php
use PHPUnit\Framework\TestCase;

class OutputTest extends TestCase{
	public function testString(){
		$this->expectOutputString("string");
		echo "string";
	}

	public function testRegExp(){
		$this->expectOutputRegex("/\d+\.\d+.\d+.\d+/");
		echo "192.168.1.1";
	}

	public function testCallback(){
		$this->setOutputCallback(function($s){
			return "a";
		});
		$this->expectOutputString("a");
		echo "test";
	}

	public function testGet(){
		echo "abc";
		$this->assertEquals("abc", $this->getActualOutput());
	}
}
