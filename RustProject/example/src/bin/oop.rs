fn main() {
    // 定义一个含有draw方法的trait
    trait Draw{
        fn draw(&self);
    }
    // 以下两个类型，都实现了Draw
    struct Button();
    impl Draw for Button{
        fn draw(&self) {
            println!("drawing Button")
        }
    }
    struct TextField();
    impl Draw for TextField{
        fn draw(&self) {
            println!("drawing TextField")
        }
    }

    // 使用vec来保存实现了Draw trait的变量
    // Box<dyn Draw>表示实现了Draw trait的对象
    // 与泛型不同，这种方式使得在component里可以存储任意多种类型，只要实现了Draw即可
    let mut components:Vec<Box<dyn Draw>> = Vec::new();

    // 可以将实现了Draw的类型push进components
    components.push(Box::new(Button{}));
    components.push(Box::new(TextField{}));
    // 并且可以遍历调用draw方法，而不需要关心存储的具体类型是什么
    for c in components{
        c.draw();
    }

    // 不满足对象安全的trait不能转化为trait对象
    // `Clone` cannot be made into an object
    // note: the trait cannot be made into an object because it requires `Self: Sized`
    //  note: for a trait to be "object safe" it needs to allow building a vtable to allow the call to be resolvable dynamically;
    // let wrong:Vec<Box<dyn Clone>>  = vec![];
}