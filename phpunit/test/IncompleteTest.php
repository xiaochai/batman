<?php
use PHPUnit\Framework\TestCase;

class IncompleteTest extends TestCase{
	// 使用markTestIncomplete来表示这个测试用例未完成，会生成一个I标识，参数为说明信息，可选
	public function testIncomplete(){
		$this->assertTrue(true);
		$this->markTestIncomplete("not finished");
	}

	// 使用markTestSkipped来跳过这个测试用例，会生成一个S标识，可用于当某个条件不满足时，跳过特定测试用例
	public function testSkip(){
		$this->markTestSkipped("skip this");
		$this->assertTrue(true);
	}

	/**
	 * 当requires标注不满足时，也会跳过这个测试
	 * requires的支持的参数
	 * PHP version
	 * PHPUnit version
	 * OS Linux|WIN32|WINNT
	 * function ReflectionMethod::setAccessible
	 * extension redis [version]
	 * @requires PHP 100.0.0
	 */
	public function testRequire(){
		$this->assertTrue(true);
	}
}
