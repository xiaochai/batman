use std::fs::File;
use std::io::{Read, ErrorKind, self};

fn main() {
    let file_name = "Cargo.toml";
    // 对于有可能出错的函数可以返回Result，Ok表示正常返回，Err表示异常
    let mut f = match File::open(file_name) {
        Ok(file) => file,
        Err(e) => panic!("open file error:{:?}", e),
    };
    // 读取文件的内容，并输出
    let mut c = String::new();
    f.read_to_string(&mut c);
    println!("{}", c);

    let file_name = "hello.txt";
    let f = match File::open(file_name) {
        Ok(file) => file,
        // 使用e.kind()为区别不一样的类型
        Err(e) => match e.kind() {
            // 不存在的时候就创建，返回成功创建的句柄
            ErrorKind::NotFound => match File::create(file_name) {
                Ok(file) => file,
                Err(e) => panic!("create file error:{:?}", e),
            },
            // 其它错误统一命中这个分支，报错panic
            other_error => panic!("open file error:{:?}", other_error),
        }
    };

    // 使用Result.unwrap()函数来快速获取Ok的值，如果是Err，则直接panic
    let f: File = File::open(file_name).unwrap();
    // 与unwrap一样，只是传入了一个字符串做为panic时的信息
    let f: File = File::open(file_name).expect("Fail to open file");

    // 传播错误
    // 以下函数的功能等同于std::fs::read_to_string(file_name)
    fn get_content_by_file_name(name: &str) -> Result<String, io::Error> {
        let mut f = match File::open(name) {
            Ok(file) => file,
            Err(e) => return Err(e),
        };
        // 读取文件的内容，并输出
        let mut c = String::new();
        return match f.read_to_string(&mut c) {
            Ok(_) => Ok(c),
            Err(e) => Err(e),
        };
    }
    // 使用?运算符简化写法
    // 注意使用?运算符与match不一样的地方是在Err类型不匹配的时候，会自动调用from函数进行隐式转换(需要实现From trait)
    fn get_content_by_file_name2(name: &str) -> Result<String, io::Error> {
        // 如果需要将Result的Err返回，则在最后使用?表达式来达到目的
        let mut f = File::open(name)?;
        // 读取文件的内容，并输出
        let mut c = String::new();
        f.read_to_string(&mut c)?;
        Ok(c)
    }
    // 使用链式调用更加简化写法
    fn get_content_by_file_name3(name: &str) -> Result<String, io::Error> {
        let mut c = String::new();
        File::open(name)?.read_to_string(&mut c)?;
        Ok(c)
    }


}