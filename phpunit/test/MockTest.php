<?php
use PHPUnit\Framework\TestCase;

class MockTest extends TestCase{
	public function testStub(){
		$stub = $this->createMock(HardObject::class);
		$stub->method('hardJob')->willReturn("YES");
		$this->assertEquals($stub->hardJob(), "YES");
	}

	public function testReturn(){
		$stub = $this->createMock(HardObject::class);
		// 永远返回第一个参数值
		$stub->method("hardJob")->will($this->returnArgument(0));
		$this->assertEquals($stub->hardJob("OTHER"), "OTHER");

		// 返回桩件本身
		$stub1 = $this->createMock(HardObject::class);
		$stub1->method("hardJob")->will($this->returnSelf());
		$this->assertSame($stub1->hardJob(), $stub1);

		// 定义对应参数对应返回值，map数组中每一组的最后一个值为返回值，前面的值都为参数
		$map = [
			["a", "b", "c", "d"],
			["e", "f", "g", "h"],
		];
		$stub2 = $this->createMock(HardObject::class);
		$stub2->method("hardJob")->will($this->returnValueMap($map));
		$this->assertEquals($stub2->hardJob("a", "b", "c"), "d");
		$this->assertEquals($stub2->hardJob("e", "f", "g"), "h");


		// 将函数指定到另外一个函数
		$stub3 = $this->createMock(HardObject::class);
		$stub3->method("hardJob")->will($this->returnCallback("otherFunc"));
		// 实际执行了otherFunc("OK")
		$this->assertEquals($stub3->hardJob("OK"), "OTHER:OK");

		// 每次调用依次返回给定的数组值
		$stub4 = $this->createMock(HardObject::class);
		$stub4->method("hardJob")->will($this->onConsecutiveCalls("a", 1, "tt", "YES"));
		$this->assertEquals($stub4->hardJob(), "a");
		$this->assertEquals($stub4->hardJob(), 1);
		$this->assertEquals($stub4->hardJob(), "tt");
		$this->assertEquals($stub4->hardJob(), "YES");
	
		// 每次调用依次返回给定的数组值
		$stub5 = $this->createMock(HardObject::class);
		$stub5->method("hardJob")->will($this->throwException(new Exception("", 1111)));
		try{
			$stub5->hardJob();
		}catch(Exception $e){}
		$this->assertEquals($e->getCode(), 1111);
	}

	public function testMock(){
		// 为 Observer 类建立仿件对象
		// 如果不调用setMethods方法的话，默认所有的方法都会被模仿，而调用了之后，其它的方法保留原来的功能
		$observer = $this->getMockBuilder(Observer::class)->/*setMethods(["update"])->*/getMock();
		// 预期一个update函数只调用一次(如果多次可以用$this->exactly(2))，并以something做为参数
		$observer->expects($this->once())->method("update")->with($this->equalTo("something"));

		$subject = new Subject("My subject");
		$subject->attach($observer);
		$subject->doSomething();

		// 这里的with可以换成withConsecutive，他接收多个参数，每个参数为数组，即with的参数，表示多个可能的值
		// with的第三个参数$this->anything()表示任意，也可以替换成更复杂的回调函数验证方式
		// $this->callback(function($subject){ return is_callable([$subject, 'getName']) && $subject->getName() == 'My subject';})
		$observer->expects($this->once())->method("reportError")->with($this->greaterThan(0), $this->stringContains("Something"), $this->anything());
		$subject->doSomethingBad();
	}
}

class Subject{
	protected $observers = [];
	protected $name;
	public function __construct($name){
		$this->name = $name;
	}
	public function getName(){
		return $this->name;
	}
	public function attach(Observer $observer){
		$this->observers[] = $observer;
	}
	public function doSomething(){
		// 做点什么 // ...
		// 通知观察者发生了些什么
		$this->notify('something');
	}
	public function doSomethingBad(){
		foreach ($this->observers as $observer) {
			$observer->reportError(42, 'Something bad happened', $this);
		} 
	}
	protected function notify($argument){
		foreach ($this->observers as $observer) {
			$observer->update($argument);
		}
	}
}

class Observer
{
	public function update($argument){
		var_dump("JFSsfklasjlkfjsf");
		// 做点什么。
	}
	public function reportError($errorCode, $errorMessage, Subject $subject){
		var_dump("OOOOOOOOOOO");
		// 做点什么。
	}
}

class HardObject{
	function hardJob(){
		return "NO";
	}
}

function otherFunc($a){
	return "OTHER:$a";
}
