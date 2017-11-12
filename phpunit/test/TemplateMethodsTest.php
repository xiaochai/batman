<?php
use PHPUnit\Framework\TestCase;
class TemplateMethodsTest extends TestCase{
	// 在类初使化时调用，只会调用一次
	public static function setUpBeforeClass(){
		fwrite(STDOUT, __METHOD__ . "\n");
	}
	// 在每一次测试方法调用之前调用，每个测试用例都会调用一次
	public function setUp(){
		fwrite(STDOUT, __METHOD__ . "\n");
	}
	// 用户来验证基境是否正确，在setUp方法之后调用
	public function assertPreConditions(){
		fwrite(STDOUT, __METHOD__ . "\n");
	}
	public function testOne(){
		fwrite(STDOUT, __METHOD__ . "\n");
		$this->assertTrue(true);
	}
	public function testTwo(){
		fwrite(STDOUT, __METHOD__ . "\n");
		$this->assertTrue(true);// @todo 可以改成$this->assertTrue(false)来不通过测试
	}
	// 在每一次测试方法调用完成之后调用，用来测试方法运行后的状态
	public function assertPostConditions(){
		fwrite(STDOUT, __METHOD__ . "\n");
	}
	// 与setUp配对，用来销毁由setUp创建的对象或者资源
	public function tearDown(){
		fwrite(STDOUT, __METHOD__ . "\n");
	}
	// 当有不成功case的时候运行
	public function onNotSuccessfulTest(Throwable $t){
		fwrite(STDOUT, __METHOD__ . "\n");
		throw $t;
	}
	// 与setUpBeforeClass配对，在所有测试用例运行之后运行
	public static function tearDownAfterClass(){
		fwrite(STDOUT, __METHOD__ . "\n");
	}
}
