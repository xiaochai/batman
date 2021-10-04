fn main() {
    // 在方法中使用泛型，在函数名称后添加尖括号，并在括号中添加类型说明，
    // <T:PartialOrd + Copy>表示这个类型必须实现PartialOrd和Copy这两个Trait
    // 参数为对应类型的数组切片，返回对应类型的值
    // 由于对元素使用了大于比较计算符，所以类型T必须实现std::cmp::PartialOrd
    // 由于需要从list[0]中取出数据，所以需要实现Copy；也可以使用引用来处理
    fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
        let mut max = list[0];
        for &item in list.iter() {
            if item > max {
                max = item
            }
        }
        max
    }
    // 使用引用的版本，不需要实现Copy
    fn largest2<T: PartialOrd>(list: &[T]) -> &T {
        let mut max = &list[0];
        for item in list.iter() {
            if *item > *max {
                max = item
            }
        }
        max
    }
    println!(
        "{},{},{}, {}",
        largest(&[1, 2, 3, 4, 5, 6, 9, 3, 4, 6]),
        largest(&[1.0, 3.0, 1.1, 5.5, -1.0, -2.4]),
        // 动态数组可以转化为数组切片
        largest(&vec![1, 2, 3, 4, 5, 4, 3, 2, 1]),
        largest(&vec!['a', 'b', 'e', 'd', 'k', 'i', 'g']),
    );

    // 在结构体中使用泛型，在结构体名之后使用尖括号来声明
    struct Point<T> {
        x: T,
        y: T,
    }
    impl<T> Point<T> {
        fn x(&self) -> &T {
            &self.x
        }
        // 在泛型的结构里定义泛型的方法
        fn other<U>(&self, other: Point<U>) -> Point<U> {
            other
        }
    }

    // 在枚举中使用泛型，我们之前看到的Option和Result都有使用，不再举例
    enum Option<T> {
        Some(T),
        None,
    }
    enum Result<T, E> {
        Ok(T),
        Err(E),
    }
}
