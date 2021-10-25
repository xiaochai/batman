//! # Art
//!
//! 测试用于艺术建模的库

pub use crate::kinds::PrimaryColor;
pub use crate::kinds::SecondaryColor;
pub use crate::utils::mixed;

pub mod kinds {
    pub enum PrimaryColor {
        Red,
        Yellow,
        Blue,
    }

    pub enum SecondaryColor {
        Orange,
        Green,
        Purple,
    }
}

pub mod utils{
    use crate::kinds::*;

    pub fn mixed(c1:PrimaryColor, c2:PrimaryColor) -> SecondaryColor{
        SecondaryColor::Orange
    }
}