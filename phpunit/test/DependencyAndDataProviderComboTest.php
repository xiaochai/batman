<?php
use PHPUnit\Framework\TestCase;

class DependencyAndDataProviderComboTest extends TestCase{
	public function provider(){
		return [
			["provider1"],
			["provider1"],// @todo 可以改成provider2来测试不通过
		];
	}

	public function testProducerFirst(){
		$this->assertTrue(true);
		return 'first';
	}

	public function testProducerSecond(){
		$this->assertTrue(true);
		return 'second';
	}

	/**
	 * @depends testProducerFirst
	 * @depends testProducerSecond
	 * @dataProvider provider
	 */
	public function testConsumer()
	{
		$this->assertEquals(
			['provider1', 'first', 'second'],
			func_get_args()
		);
	}
}
