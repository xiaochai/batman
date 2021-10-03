mod front_of_house;
pub fn eat_at_restaurant() {
    // 以下两种调用是等价的
    // 使用绝对路径，从crate关键字（即根节点）开始
    crate::front_of_house::hosting::add_to_waitlist();
    // 使用相对路径，从当前模块开始
    front_of_house::hosting::add_to_waitlist();
}

// 使用use关键字可以简化路径
// 以下两行等价，self关键字可能在后续版本中去掉
use front_of_house::hosting;
// use self::front_of_house::hosting;

pub fn eat_at_restaurant2() {
    hosting::add_to_waitlist();
}

// 为防止重名，使用as来重命名
use std::fmt::Result;
use std::io::Result as IOResult;
// 等价于导入std::cmp::Ordering和std::io这两个包
use std::{cmp::Ordering, fs};
// 等价于导入std::io和std::io::Write这两个包
use std::io::{self, Write};
// 导入std::collections下的所有包，一般不推荐，容易造成命名冲突
use std::collections::*;
pub fn test(){
    Result::Ok(());
    IOResult::Ok("FD");
}