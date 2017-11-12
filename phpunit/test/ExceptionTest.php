<?php
use PHPUnit\Framework\TestCase;

class ExceptionTest extends TestCase{
	public function testException(){
		$this->expectException(Exception::class);
		throw new Exception("test", 1);
	}
	public function testExceptionCode(){
		$this->expectExceptionCode(1);
		throw new Exception("test", 1);
	}
	public function testExceptionMessage(){
		$this->expectExceptionMessage("test");
		throw new Exception("test", 1);
	}
	/**
	 * @expectedException Exception
	 */
	public function testExceptionLabel(){
		throw new Exception("test", 1);
	}
	/**
	 * @expectedException Exception
	 * @expectedExceptionCode 1
	 */
	public function testExceptionCodeLabel(){
		throw new Exception("test", 1);
	}
	/**
	 * @expectedException Exception
	 * @expectedExceptionMessage test
	 */
	public function testExceptionMessageLabel(){
		throw new Exception("test", 1);
	}

	/**
	 * @expectedException PHPUnit\Framework\Error\Notice
	 */
	public function testPHPNotice(){
		$k == 1;
	}

	/**
	 * @expectedException PHPUnit\Framework\Error\Warning
	 */
	public function testPHPWarning(){
		$a = "abc";
		$a["abc"];
	}
}
