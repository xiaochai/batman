<?php
use PHPUnit\Framework\TestCase;
class RiskTest extends TestCase{
	/**
	 * 无任何断言，包括预期的标注
	 * 此各类型默认开启，可以通过--dont-report-useless-tests选项来关闭
	 * 著使用<phpunit bootstrap="./autoload.php" beStrictAboutTestsThatDoNotTestAnything="false">来关闭
	 */
	public function testNothing(){
	}

	/**
	 * 意外的代码覆盖
	 * @todo 还没有理解意思
	 *
	 */
	public function testCoverage(){
		$this->assertTrue(true);
	}

	/**
	 * 执行过程中有输出的
	 * 此类型默认关闭，可通过--disallow-test-output打开
	 * 或者在xml中使用beStrictAboutOutputDuringTests="true"来开启
	 */
	public function testOutput(){
		echo "output";
		$this->assertTrue(true);
	}

	/**
	 * @todo 需要安装PHP_Invoker包并且pcntl扩展才可用
	 * 执行超地指定时间
	 * 通过--enforce-time-limit或者beStrictAboutTestSize="true"来开启
	 * 如果执行时间超过1秒，则视为Risk，可通过标注@large或者@media来设置这个时间为10秒、60秒
	 * 默认为@small超时为1秒
	 * 可通过配置timeoutForSmallTests值来修改@small代表的时间
	 */
	public function testTimeout(){
		sleep(2);
		$this->assertTrue(true);
	}
	
	/**
	 * @todo 没有实现过，需要重新实现
	 * 可以更严格对待篡改全局状态的测试
	 * 通过--strict-global-state或者beStrictAboutChangesToGlobalState="true"来启用
	*/
	public function testGlobal(){
		$GLOBAL["a"] = "2r";
		$this->assertTrue(true);
	}
}
