// use example::kinds::PrimaryColor;
// use example::utils::mixed;

// 使用pub use重新导出包结构后，就可以使用直接使用一级目录的结构，不用关心原包中的结构了
use example::PrimaryColor;
use example::mixed;

fn main(){
    let red = PrimaryColor::Red;
    let yellow = PrimaryColor::Yellow;
    mixed(red, yellow);

}