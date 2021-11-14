fn main() {
    // 解引用裸指针
    let mut num = 5;
    // 如果只是创建裸针针，并不需要unsafe关键字
    // 使用as关键来转化类型，以下两个指针都是从有效引用创建的，所以是有效的
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;
    // 从任意地址创建指针，这里的r3可能就不是一个可用的指针
    let t = 0x423243242usize;
    let _r3 = t as *const i32;

    // 如果需要解引用裸指针，则要将代码包在unsafe里
    unsafe {
        // 只有可变的裸指针才能赋值，不能对*r1进行赋值
        // 其实以下如果不是裸指针，已经破坏了引用规则，同一个地址的可变和不可变引用，但在裸指针中不受此限制
        *r2 = 10;
        println!("r1 val:{}", *r1); // r1 val:10
        // 以下由于指到了不合法的内存地址，直接段错误： segmentation fault
        // println!("r3 val:{}", *_r3);
    }

    // 使用不安全的函数
    // 不安全的方法与普通方法一样，只是在fn之前添加unsafe关键字
    // 在不安全的函数体内使用不安全特性就不需要包裹unsafe了
    unsafe fn dangerous() {}
    // 不安全的方法需要包裹在unsafe中调用
    unsafe {
        dangerous();
    }

    // 测试标准库中的split_at_mut函数
    let mut v = vec![1, 2, 3, 4, 5];
    let r = &mut v[..];
    let (a, b) = r.split_at_mut(3);
    println!("{:?}, {:?}", a, b);

    // 尝试实现自己的简化版本split_at_mut，使用了不安全特性
    fn split_at_mut(v: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
        assert!(mid <= v.len());
        // 仅仅使用安全的Rust，会报两个可变指针的错误，而我们能保证这两个可变指针其实指向的是切片非重合的两部分
        // (&mut v[..mid], &mut v[mid..])
        // 所以可以使用unsafe特性来实现
        // 获取指向切片的可变裸指针
        let p = v.as_mut_ptr();
        unsafe {
            // 以下的from_raw_parts_mut函数是unsafe的，所以这块要包含在unsafe中
            (core::slice::from_raw_parts_mut(p, mid),
             core::slice::from_raw_parts_mut(p.offset(mid as isize), v.len() - mid)
            )
        }
    }
    let mut v = vec![1, 2, 3, 4, 5];
    let (a, b) = split_at_mut(&mut v[..], 3);
    println!("{:?}, {:?}", a, b);

    // 使用extern关键字可以引入其它语言的函数，所有通过extern引入的函数都是不安全的，需要使用unsafe关键字
    extern "C" {
        fn abs(input: i32) -> i32;
    }

    unsafe {
        println!("{}", abs(-20));
    }
    incr_counter();
    println!("{}", get_counter());

    // 不安全的trait
    unsafe trait Foo {}
    unsafe impl Foo for i32 {}

    // 在并发章节说明的Send和Sync这两个trait就是不安全的trait，要为某个不安全的类型实现trait，需要使用unsafe关键字
    struct Point {
        x: i32,
        y: i32,
    }
    unsafe impl Send for Point {}
    unsafe impl Sync for Point {}
}

static mut COUNTER: u32 = 10;

fn incr_counter() {
    unsafe {
        COUNTER += 1;
    }
}

fn get_counter() -> u32 {
    unsafe {
        COUNTER
    }
}