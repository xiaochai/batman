fn main() {
    // 迭代器允许你依次为序列中的每一个元素执行某些任务
    // 迭代器是惰性的，除非主动调用方法来消耗，否则不会有任何作用
    // 迭代器实现了Iterator 这一trait，Iterator有多个方法，但大多都提供了默认的实现
    // 要实现Iterator只需要实现next方法即可
    trait Iterator1 {
        // 关联类型，用于存储next返回值的类型
        type Item;
        // Self::Item表示返回的Option存储的是Item的类型
        // next返回被包裹在Some中的元素，在遍历结束时返回None
        fn next(&mut self) -> Option<Self::Item>;
        // 省略默认实现的方法
        // ...
    }

    let mut v = vec![1, 2, 3];
    // 必须将iter标识成mut的，因为调用next会改变iter内部的状态
    // iter()方法返回的是一个不可变引用迭代器，next的返回值是数组中元素的不可变引用
    let mut iter = v.iter();
    // 1,2,3,true
    println!("{},{},{},{}", iter.next().unwrap(), iter.next().unwrap(), iter.next().unwrap(), iter.next() == None);
    // 返回可变引用
    for i in v.iter_mut() {
        *i += 1;
    }
    // [2,3,4]
    println!("{:?}", v);

    // 使用into_iter获取其所有权，后续v不再可用
    println!("{}", v.into_iter().next().unwrap());

    // 迭代适配器，以及与闭包共同实现
    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
    let add2: Vec<i32> = v.iter().map(|x| x + 2).collect();
    let min = 5;
    // 这里使用filter的给到的闭包参数为&&x，要么使用**x解引用，要么在参数中使用&&x来表示x是一个i32
    let big: Vec<&i32> = v.iter().filter(|&&x| x + 2 > min).collect();
    // [3, 4, 5, 6, 7, 8, 9, 10, 11],[6, 7, 8, 9]
    println!("{:?},{:?}", add2, big);

    // 创建自定义的迭代器
    struct Counter {
        count: i32,
    }
    impl Counter {
        fn new() -> Counter {
            Counter { count: 0 }
        }
    }
    impl std::iter::Iterator for Counter {
        type Item = i32;

        fn next(&mut self) -> Option<Self::Item> {
            self.count += 1;
            if self.count > 5 {
                None
            } else {
                Some(self.count)
            }
        }
    }
    for i in Counter::new() {
        println!("{}", i)
    }
    // 一些迭代器复杂的用法
    // Iterator这个trait有很多方法的默认实现，依赖于next方法，我们可以直接使用完成复杂的功能
    // 以下有一个特殊的语法，在方法后面添加::<typeHint>，可以给编译器提示返回的值类型为typeHint，用于无法推断类型的场景时使用
    // Counter::new() is [1, 2, 3, 4, 5]
    // Counter::new().skip(1) is [2, 3, 4, 5]
    // Counter::new().zip(Counter::new().skip(1)) is [(1, 2), (2, 3), (3, 4), (4, 5)]
    //     .map(|(a,b)| a*b) is [2, 6, 12, 20]
    //     .filter(|x| x % 3 == 0) [6, 12]
    //     .sum() is 18
    let c: i32 = Counter::new().zip(Counter::new().skip(1))
        .map(|(a, b)| a * b)
        .filter(|x| x % 3 == 0)
        .sum();
    println!("Counter::new() is {:?}", Counter::new().collect::<Vec<i32>>());
    println!("Counter::new().skip(1) is {:?}", Counter::new().skip(1).collect::<Vec<i32>>());
    println!("Counter::new().zip(Counter::new().skip(1)) is {:?}", Counter::new().zip(Counter::new().skip(1)).collect::<Vec<(i32, i32)>>());
    println!("    .map(|(a,b)| a*b) is {:?}", Counter::new().zip(Counter::new().skip(1)).map(|(a, b)| a * b).collect::<Vec<i32>>());
    println!("    .filter(|x| x % 3 == 0) {:?}", Counter::new().zip(Counter::new().skip(1)).map(|(a, b)| a * b).filter(|x| x % 3 == 0).collect::<Vec<i32>>());
    println!("    .sum() is {:?}", c);
}