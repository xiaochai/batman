// 创建宏，创建一个与简化版本的vec!
// #[macro_export]标注使得外部包可以使用这个宏
#[macro_export]
// 使用macro_rules!这一固定语法创建出宏vec2
macro_rules! vec2{
    ($($x:expr),*)=>{
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    }
}

fn main() {
    let t = vec![1,2,3];
    let f = vec2![1,2,3];
    println!("{:?}, {:?}", t, f); // [1, 2, 3], [1, 2, 3]



}
