//! # Adder
//!
//! 测试做为包的注释，包含有一些小组件

use std::ops::Add;

/// 定义一个泛型加法
///
/// # Example
///
/// 例子，一般是测试用例
/// ```
/// assert_eq!(5, adder::adder(3,2));
/// assert_eq!(9.8, adder::adder(3.3, 6.5));
/// ```
/// # Panics
///
/// 可能Panic的场景
///
/// # Errors
/// 如果返回Result时，这里写明Error返回的场景，以及返回的Error值
///
/// # Safety
///
/// 当使用了unsafe关键字时，这里可以说明使用unsafe的原因，以及调用者的注意事项
///
pub fn adder<T: Add + Add<Output=T>>(a: T, b: T) -> T {
    return a + b;
}

#[cfg(test)]
mod tests {
    // 引入所有父模块的所有包，这样可以直接使用adder方法，否则adder需要指定完整路径才能使用
    use super::*;
    use std::fs::File;
    use std::io::Read;

    // 标记以下函数是一个测试函数，如果没有这个标记，cargo test的时候不会运行
    #[test]
    fn it_works() {
        // 相等断言
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn adder_test() {
        // 注意使用了assert_eq和assert_ne这两个断言时，两个参数必须实现了PartialEq和Debug这两个trait，一个用于判断相等，一个用于出错时输出信息

        // 以下两个写法等价，推荐使用第一个写法，因为在失败时可以打印出两个参数的值，方便分析原因
        assert_eq!(adder(2, 2), 4);
        assert!(adder(2, 2) == 4); // assert!接收一个bool类型的参数，只有为true时测试才通过
        // assert_ne!用于断言两个值不相等
        assert_ne!(adder(2.0, 3.0), 6.0);

        // 这些断言在必要参数之后的信息都会被用于format!输出额外的信息
        assert_eq!(adder(3, 4), 7, "{}+{} is not equal 7", 3, 4);
    }

    #[test]
    // should_panic属性用于表示这个函数会发生panic
    // 参数expected会比较panic的信息是否与此相匹配，注意中人包含expected的内容即可
    #[should_panic(expected = "panicked")]
    fn check_panic() {
        panic!("i panicked")
    }

    #[test]
    // 使用Result做为测试用例的返回值时，只要有Err返回，就测试失败
    // 这种情况下可以使用?表达式来简化测试用例的编写
    fn use_result() -> Result<(), std::io::Error> {
        let mut f = File::open("./cargo.toml")?;
        let mut s = String::new();
        let _ = f.read_to_string(&mut s)?;
        return Ok(());
    }

    #[test]
    #[ignore] // 默认情况下忽略此case，可以通过--ignored来专门运行这种case
    fn long_time_test() {}
}
