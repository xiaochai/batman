<?php
use PHPUnit\Framework\TestCase;

class CoverageTest extends TestCase{
	public function testAdd(){
		$this->assertEquals(MyMath::add(1,2),3);
	}
	public function testMinus(){
		$this->assertEquals(MyMath::minus(1,2),-1);
	}
	public function testMultiply(){
		$this->assertEquals(MyMath::multiply(1,2),2);
	}
	public function testDivision(){
		$this->assertEquals(MyMath::division(4,2),2);
		$this->assertEquals(MyMath::division(4,0),false);
	}
}
