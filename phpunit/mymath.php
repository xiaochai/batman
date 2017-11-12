<?php
class MyMath {
	public static function add($a, $b){
		return $a+$b;
	}
	public static function minus($a, $b){
		return $a-$b;
	}

	/**
	 * @codeCoverageIgnore
	 */
	public static function multiply($a, $b){
		return $a * $b;
	}

	public static function division($a, $b){
		// @codeCoverageIgnoreStart
		if($b === 0){
			return false;
		}else{
			// @codeCoverageIgnoreEnd
			return $a/$b;
		}
	}
}
