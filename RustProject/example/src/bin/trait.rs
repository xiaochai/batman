use std::fmt::{Display, Debug};

fn main() {
    // 定义多种文章共有的摘要功能trait
    pub trait Summary {
        fn summarize(&self) -> String;
        // trait也可以提供一个默认实现，这样实现了这一trait的结构体，如果没有提供实现，则以默认实现为准
        fn summarize2(&self) -> String {
            String::from("Read more")
        }
    }

    pub struct NewsArticle {
        pub headline: String,
        pub location: String,
        pub author: String,
        pub content: String,
    }
    // 为某一个结构实现trait，使用关键字impl和for
    // 实现的trait跟普通函数一样，可以被调用
    impl Summary for NewsArticle {
        fn summarize(&self) -> String {
            format!("{},{},{}", self.headline, self.author, self.location)
        }
    }
    pub struct Tweet {
        pub username: String,
        pub content: String,
        pub reply: bool,
        pub retweet: bool,
    }
    impl Summary for Tweet {
        fn summarize(&self) -> String {
            format!("{},{}", self.username, self.content)
        }
        // 重载了默认实现，这样就无法调用到默认实现了
        fn summarize2(&self) -> String {
            format!("summarize2....")
        }
    }
    let tweet = Tweet {
        username: "lee".to_string(),
        content: "content".to_string(),
        reply: false,
        retweet: false,
    };
    // trait实现的函数，可以像普通函数一样调用，summarize2调用则是默认的实现
    println!("tweet: {}, summary2:{}", tweet.summarize(), tweet.summarize2());

    // 将trait作为参数
    // 这个函数接收实现了Summary trait的结构体类型，在这里可传入Tweet和NewsArticle
    pub fn notify<T: Summary>(item: &T) {
        println!("breaking news1:{}", item.summarize())
    }
    notify(&tweet);
    // 可以使用impl形式的语法糖来简化写法，与之前的一致
    // 是否简化也区别于实际场景，例如多个函数使用同一个约束时，使用泛型表达式则更加方便
    pub fn notify2(item: &impl Summary) {
        println!("breaking news2:{}", item.summarize())
    }
    notify2(&tweet);

    // 使用多个约束时使用+号来处理，这里的item必须实现Display和Summary两个trait
    pub fn notify3<T: Display + Summary>(item: T) {}
    // 在复杂情况下使用where语句可以使得函数签名更清晰，以下两种方式是等价的
    fn some_func<T: Display + Clone, U: Clone + Debug>(t: T, u: U) -> i32 { 1 }
    fn some_func2<T, U>(t: T, u: U) -> i32 where T: Display + Clone, U: Clone + Debug { 1 }

    // 可以返回可以使用impl形式，但只能返回一中类型，要么是Tweet，要么是NewsArticle，不能在不同的分支返回两种类型
    fn return_summarizable() -> impl Summary {
        Tweet {
            username: "".to_string(),
            content: "".to_string(),
            reply: false,
            retweet: false,
        }
    }
    // 以下无法通过编译，对于泛型，需要深入研究一下机制，为什么以下函数无法通过编译
    // fn return_summarizable2<T: Summary>(item:T) -> T {
    //     Tweet {
    //         username: "".to_string(),
    //         content: "".to_string(),
    //         reply: false,
    //         retweet: false,
    //     }
    // }

    // 使用trait约束来有条件地实现方法
    struct Point<T> {
        x: T,
        y: T,
    }
    // 为所有类型的T的Point实现new方法
    impl<T> Point<T> {
        // 大写的Self与小写的self区别
        fn new(x: T, y: T) -> Self {
            Point { x, y }
        }
    }
    // 只为实现了PartialOrd和Display的Point实现cmp方法
    impl<T: PartialOrd + Display> Point<T> {
        fn cmp(&self) {
            if self.x > self.y {
                println!("x>y")
            } else {
                println!("x<=y")
            }
        }
    }
    // 也可以使用一个trait约束来实现另外一个trait，称之为覆盖实现(blanket implementation)
    // 例如以下的例子，为了实现了Display的类型实现Summary方法
    impl<T: Display> Summary for T {
        fn summarize(&self) -> String {
            format!("read more:{}", self)
        }
    }
    // 以下例子无法通过编译，因为Display不在此包中，T这一也不在此包中，受孤儿规则限制，将报错
    // 报错：Only traits defined in the current crate can be implemented for arbitrary types [E0117]
    // impl<T: Summary> Display for T {
    //     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    //         write!(f, "({}, {})", self.x, self.y)
    //     }
    // }

    // 因为上面为实现Display的类型实现了Summary，而i32实现了Display，所以i32实现了Summary
    // 输出read more:2
    println!("{}", 2.summarize());
}