// 返回Result中的E为Box<dyn std::error::Error>，表示为实现了Error这一trait的类型
// 具体的类型需要在具体的场景中才能确定，这里的dyn关键字也说明了是一个动态的类型
pub fn run(config: &Config) -> Result<(), Box<dyn std::error::Error>> {
    let contents = std::fs::read_to_string(&config.filename)?;
    let result = if config.case_sensitive {
        search(&config.query, &contents)
    } else {
        search_case_insensitive(&config.query, &contents)
    };

    for l in result {
        println!("{}", l);
    }
    Ok(())
}

pub struct Config {
    query: String,
    filename: String,
    case_sensitive: bool,
}

impl Config {
    // 使用命令行参数来构造Config结构，返回Result结构，在出错时返回str
    // 返回值中的str中声明了生命周期为全局的，因为字面量常量可以是全局的，此处不设置其实也没有问题，生命周期与输入一致
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 3 {
            return Err("not enough arguments!");
        }
        let query = args[1].clone();
        let filename = args[2].clone();
        // 读取环境变量来确定是否需要大小写敏感，读取到无error则case_sensitive=false，不区分大小写，如下
        // CASE_INSENSITIVE=1 cargo run Nobody poem.txt
        let case_sensitive = std::env::var("CASE_INSENSITIVE").is_err();
        // 变量与结构体字段重名时，可以使用此种方式快速构建
        Ok(Config { query, filename, case_sensitive })
    }
    // 使用迭代器版本
    pub fn new2(mut args: std::env::Args) -> Result<Config, &'static str> {
        args.next();
        let query = match args.next() {
            Some(q) => q,
            None => return Err("no query")
        };
        let filename = match args.next() {
            Some(f) => f,
            None => return Err("no filename")
        };
        let case_sensitive = std::env::var("CASE_INSENSITIVE").is_err();
        Ok(Config { query, filename, case_sensitive })
    }
}

// 这块需要指定生命周期，表明Vec返回的&str，与contents的生命周期绑定
// 如果contents失败了，那么返回值也将没有意义了
fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut ret: Vec<&str> = Vec::new();
    for line in contents.lines() {
        if line.contains(query) {
            ret.push(line)
        }
    }
    ret
    // 使用迭代器版本
    // contents.lines().filter(|line| line.contains(query)).collect()
}

fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut ret: Vec<&str> = Vec::new();
    let q = query.to_lowercase();
    for l in contents.lines() {
        if l.to_lowercase().contains(&q) {
            ret.push(l)
        }
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_result() {
        let query = "duct";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
        ";
        assert_eq!(
            vec!["safe, fast, productive."],
            search(query, contents),
        );
    }

    #[test]
    fn case_insensitive() {
        let query = "rust";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
        ";
        assert_eq!(
            vec!["Rust:"],
            search_case_insensitive(query, contents),
        );
    }
}