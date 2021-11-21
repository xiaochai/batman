// 标准库中定义的比较结果的枚举
use std::cmp::Ordering;
// 使用use语句进行包导入；rust默认会预导入(prelude)一部分常用的类型，而std::io不在此范围，需要使用use语句
use std::io;

// 首先使用了rand包，需要在Cargo.toml中添加rand = "0.8.0"
// 在run或者build的时候，会根据crates.io上的最新版本、依赖关系下载所需要的包
// rand::Rng为trait(后面解析)，gen_range定义于此Trait中
// 如果不导入，调用gen_range将报错，因为ThreadRng的对应实现定义于Rng trait中
use rand::Rng;

fn main() {
    // rand::thread_rng()将返回位于本地线程空间的随机数生成器ThreadRng，实现了rand::Rng这一trait
    // gen_range的参数签名在0.7.0的包和0.8.x的包上不一样，在旧版中支持两个参数，而新版本中只支持一个参数
    // 1..101的用法后面介绍，这一行表示生成[1,101)的随机数
    let secret_num = rand::thread_rng().gen_range(1..101);
    println!("secret number is {}", secret_num);

    // 死循环
    loop {
        // let关键字用于创建变量
        // 默认变量都是不可变的，使用mut关键字修饰可以使变量可变
        // String是标准库中的字符串类型，内部我问个他UTF-8编码并可动态扩展
        // new是String的一个关联函数(静态方法)，用于创建一个空的字段串
        let mut guess = String::new();

        println!("Guess the number!\nPlease input your guess:");

        // std::io::stdin()会返回标准输入的句柄
        // 参数&mut guess表示read_line接收一个可变字符串的引用(后面介绍)，将读取到的值存入其中
        // read_line返回io::Result枚举类型，有Ok和Err两个变体(枚举类型的值列表称为变体)
        // 返回Ok时表示正常并通过expect提取附带的值(字节数)；返回Err时expect将中断程序，并将参数显示出来
        // 不带expect时也能通过编译，但会收到Result没有被处理的警告(warning: unused `Result` that must be used)
        io::stdin().read_line(&mut guess).expect("Failed to read line");

        // Rust中允许使用同名新变量来隐藏(shadow)旧值
        // guess:u32是明确guess的类型，以此来使得让编译器推到出parse要返回包含u32的值
        // parse的返回值是一个Result枚举，有Ok和Err两个变体(枚举值)，用match来判断两种情况
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(e) => {
                println!("Please type a number!");
                // continue回到loop开头继续
                continue;
            }
        };

        // {}为println!的占位符，第1个花括号表示格式化字符串后的第一个参数的值，以此类推
        println!("You guessed:{}", guess);

        // 模式匹配，由match表达式和多个分支组成，Rust可以保证使用match时不会漏掉任何一种情况
        // Rust会将secret_num也推导成u32与guess比较
        match guess.cmp(&secret_num) {
            Ordering::Less => println!("Too small!"),
            Ordering::Equal => {
                println!("You WIN!");
                // 退出循环
                break;
            }
            Ordering::Greater => { println!("Too big!") }
        }
    }
}
