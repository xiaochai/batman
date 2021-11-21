fn main() {
    let mut r = &2;
    {
        let x = 5;
        // 以下无法通过编译，因为x在内部作用域内，而r在main作用域，r的生命周期大于x，无法使用x的引用。
        // `x` does not live long enough
        // r = &x
    }
    println!("{}", r);

    // 如果不加生命周期标注的话，无法确定返回值的生命周期，无法通过编译
    // 由于x,y两个参数都用于做为引用返回，所以x,y必须都要标明生命周期
    // 这里的'a会被具体化为x,y的生命周期中重叠的那一部分
    fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
        if x.len() > y.len() {
            x
        } else {
            y
        }
    }

    let s1 = String::from("s11");
    let mut s4 = "";
    {
        let s2 = String::from("s2");
        // longest返回值的生命周期是s1和s2中生命周期较短的那个，即s2的生命周期
        // s3的生命周期与s2一样，所以能通过编译
        let s3 = longest(&s1, &s2);
        println!("{}", s3);
        // 以下无法通过编译，因为s4的生命周期大于s2
        // s4 = longest(&s1, &s2);
    }
    println!("{}", s4);

    // 以下这个函数不满足生命周期省略规则，所以必须手动标注
    // fn longest(x:&str, y:&str)->&str{}
    // 以下函数应用规则1和规则2后，所有的输入输出引用参数周期都确定，所以可以省略
    // fn first_word(x:&str)->&str{}


    // 结构体中引用字段的生命周期标
    struct ImportantExcerpt<'a> {
        part: &'a str,
    }

    impl<'a> ImportantExcerpt<'a> {
        fn level(&self) -> i32 {
            2
        }
        // 应用第一条规则和第三条规则，可以得出正确的生命周期，所以以下这个可以周期可以省略
        fn announce_and_return_part(&self, announcement: &str) -> &str {
            self.part
        }
        // 应用第一条和第三条规则，得出的生命周期不正确，所以在没有生命周期房间里时编译报错
        fn display_part_and_return<'b>(&self, announcement: &'b str) -> &'b str {
            announcement
        }
    }

    // 'static是固定写法，表示生命周期是整个程序的执行周期
    // 所有字符串字面量都拥有'static生命周期
    let s: &'static str = "abc";

    use std::fmt::Display;
    // 同时使用生命周期，泛型，trait约束的例子
    // 生命周期必须在泛型类型声明之前，也可以使用where进行trait约束
    // 生命周期也是泛型的一种？？
    fn longest_with_announcement<'a, T:Display>(x: &'a str, y: &'a str, anno: T) -> &'a str {
        println!("{}", anno);
        if x.len() > y.len() {
            x
        } else {
            y
        }
    }
}