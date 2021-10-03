fn main() {
    // 创建一个动态数组
    let v: Vec<i32> = Vec::new();
    // 使用vec!宏来快速创建一个带有初始值的数组
    let v = vec![1, 2, 3];
    // 必须是mut的动态数组才能push进数据
    let mut v: Vec<i32> = Vec::new();
    // 往动态数组里添加数据
    v.push(5);
    v.push(6);

    // 获取数组元素的引用，如果数组越界将发生panic
    let e1: &i32 = &v[0];
    // 返回Option<&T>类型，如果不越界将返回None
    let e2: Option<&i32> = v.get(0);

    // 注意数组元素的引用会对整个数组造成影响，这就导致了在只读引用存在的情况下无法往数组中push元素
    // 以下行无法通过编译：cannot borrow `v` as mutable because it is also borrowed as immutable
    // v.push(7);
    println!("{}", e1);

    // 使用for来遍历所有元素，这里的i为&i32
    for i in &v {
        // 5 6
        println!("{}", i)
    }
    // 这里的i值为&mut i32
    for i in &mut v {
        // 将i解引用，指向对应的值，并修改
        *i *= 2;
    }
    for i in &v {
        // 10 12
        println!("{}", i)
    }

    // 字符串字面量是&str类型
    let c: &str = "ab";
    println!("{}", c);
    // 创建一个新空字符串
    let s: String = String::new();
    // 从字面量创建一个字符串的两种方法，String::from的静态方法，&str的to_string方法
    let s = String::from("hello");
    let s = "hello".to_string();
    // 修改字符串
    let mut s = String::from("abc");
    // 往后添加字符串，push_str的参数是引用的形式&str
    s.push_str("def");
    // 插入单个字符，单个字符使用单引号
    s.push('g');
    // 使用+号拼接字符串，加号的左边是String类型，右边是&str类型，左边的变量的所有权将被加号获取而不再有效
    let s1 = String::from("hello");
    let s2 = String::from(", world");
    // s1 的所有权将被转移，不再可用，而s2由于使用引用，所以可以继续使用
    // 加号的第二个签名是&str，而我们传入的是&String也是合法的，因为Rust使用使用解引用强制转换的技术，将&s2转化为&s2[..]
    // &s2[..] 是&str类型
    let s = s1 + &s2;
    println!("{},{}", s, s2);
    let k: &str = &s[2..];
    // 使用format!宏来拼接字符串，format!不会夺取任何参数的所有权
    let s = format!("{}, {}! {}.", "hello", "world", "lee");
    println!("{}", s);
    // String采用utf-8编码，所以一个中文占用3个字节；以下输出3
    println!("{}", "我".to_string().len());
    // 虽然字符串不允许使用下标直接访问，但可以使用切片获取某个范围的字符串
    println!("{}", &s[0..1]);
    let s = "我是中国人".to_string();
    // 需要注意如果切片的范围不是一个合法的字符串，则会直接panic
    // 以下将发生运行时panic：thread 'main' panicked at 'byte index 1 is not a char boundary; it is inside '我' (bytes 0..3) of `我`', src/main.rs:69:21
    // println!("{}", &s[0..1]);


    // 使用chars函数，可以获取根据编码获取字符串中的字符值
    for i in s.chars() {
        println!("{}", i);
    }
    // 与此相对，使用bytes，则获取每一个字节的内容
    for i in s.bytes() {
        println!("{}", i);
    }

    use std::collections::HashMap;
    // 初始化一个hashmap，不指定类型，编译器可以从h.insert里推断出类型来为HashMap<&str,i32>
    let mut h = HashMap::new();
    h.insert("lee", 1220);
    // 使用Vec来构建HashMap
    let teams = vec![String::from("blue"), String::from("yellow")];
    let scores = vec![10, 50];
    // h的类型声明是必须的，因为collect可以返回多种类型，需要明确这里需要返回的类型，但是泛型可以使用_代替，由编译器来推断
    // h的类型为HashMap<&String, &i32>
    let mut h: HashMap<_, _> = teams.iter().zip(scores.iter()).collect();
    // 获取HashMap中的值，注意这里的值是&String，不能直接使用&str
    let blue = String::from("blue");
    // get函数获取的值为Option，如果不存在，则返回None
    // get取得的结果是value的引用值，在这个场景中为&&i32
    let blue_team_score = match h.get(&blue) {
        // 由于值是&&i32，所以需要两次解引用成i32值，否则与None的返回值不匹配
        Some(i) => **i,
        None => 0,
    };
    println!("{}", blue_team_score);
    // 使用for循环获取HashMap里的值，由于使用&h，所以这里的key为&&String，value为&&i32
    for (key, value) in &h {
        println!("key:{}, value:{}", key, value);
    }


    let k = String::from("blue");
    // 更新值，如果是直接覆盖，使用insert即可
    h.insert(&k, &20);
    // 通过entry函数返回Entry枚举类型，其or_insert方法可以判断值是否存在，不存在则插入，存在则不处理
    // 其返回HashMap中value的可变引用，在此为&mut &i32，可以对其进行修改
    let e = h.entry(&k).or_insert(&30);
    *e = &11;
    // {"blue": 11, "yellow": 50}
    println!("{:?}", h);

    // 例子，查看一个字符串中每一个字符出现的次数
    let text = "hello world hello lee ok";
    let mut map = HashMap::new();
    for i in text.split_whitespace() {
        let count = map.entry(i).or_insert(0);
        *count += 1;
    }
    // {"ok": 1, "world": 1, "lee": 1, "hello": 2}
    println!("{:?}", map);

    // 计算平均数
    fn avg(arr: &Vec<i32>) -> i32 {
        let mut sum = 0;
        let mut len = 0;
        for i in arr {
            sum += *i;
            len += 1;
        }
        sum / len
    }
    // 计算众数
    fn zong(arr: &Vec<i32>) -> i32 {
        let mut map = HashMap::new();
        for i in arr {
            let e = map.entry(*i).or_insert(0);
            *e += 1;
        }
        let mut max = 0;
        let mut max_count = 0;
        for (k, v) in map {
            if v > max_count {
                max = k;
                max_count = v;
            }
        }
        max
    }
    // 计算中位数
    fn mid(arr: &mut Vec<i32>) -> i32 {
        // 简单的冒泡算法
        for i in 0..arr.len() {
            for j in i..arr.len() {
                if arr[i] > arr[j] {
                    let tmp = arr[i];
                    arr[i] = arr[j];
                    arr[j] = tmp;
                }
            }
        }
        return arr[arr.len() / 2];
    }

    let mut t = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 1, 9, 9];
    println!("{}, {}, {}, {:?}", avg(&t), zong(&t), mid(&mut t), t);

    // first -> irst-fay；apple-> apply-hay;
    fn string_to_latin(s: &String) -> String {
        let mut i = 0;
        let mut first: char = ' ';
        let mut left: String = String::new();
        for c in s.chars() {
            if i == 0 {
                first = c;
                left = String::new();
                i += 1;
            } else {
                left = left + &c.to_string();
                i += 1;
            }
        }
        if first == 'a' {
            s.clone() + "-hay"
        } else {
            format!("{}-{}ay", left.clone(), first.to_string())
        }
    }
    println!("{},{}", string_to_latin(&String::from("apple")), string_to_latin(&String::from("first")));

    // company
    struct Company {
        employee: HashMap<String, String>,
    }
    impl Company {
        fn new() -> Company {
            Company {
                employee: HashMap::new(),
            }
        }
        fn add(&mut self, name: String, part: String) {
            self.employee.insert(name, part);
        }
        fn get(&self, part: &String) -> Vec<&String> {
            let mut v: Vec<&String> = Vec::new();
            for (key, val) in &self.employee {
                if val == part {
                    v.push(key);
                }
            }
            v
        }
        fn getAll(&self) -> Vec<&String> {
            let mut v: Vec<&String> = Vec::new();
            for (key, val) in &self.employee {
                v.push(key);
            }
            v
        }
    }
    let mut company = Company::new();
    company.add("lee".to_string(), "it".to_string());
    company.add("neo".to_string(), "it".to_string());
    company.add("kitty".to_string(), "it".to_string());
    company.add("roi".to_string(), "it".to_string());
    company.add("fob".to_string(), "hr".to_string());

    println!("{:?},{:?}", company.getAll(), company.get(&("it".to_string())));
}