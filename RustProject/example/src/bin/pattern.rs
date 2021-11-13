fn main() {
    // match匹配模式
    // 必须穷尽所有的可能性；为了满足这一条件，可以使用两种方式
    // 1. 最后一个分支使用全匹配模式，可以使用一个变量名来处理
    // 2. 使用下划线这一特殊的模式来匹配所有值
    let m = 5;
    match m {
        1 => println!("is 1"),
        x => println!("is not 1, is {}", x)
    }
    match m {
        1..5 => println!("between")
    }
}