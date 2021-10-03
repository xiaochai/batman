// 定义模块，可以嵌套子模块
pub mod hosting;

pub mod serving {
    fn take_order() {}

    fn serve_order() {}

    fn take_payment() {}

    mod back_of_house {
        fn fix_incorrect_order() {
            // 使用super关键字引用父模块的，由于子模块可以使用父模块的所有条目，包括私有
            super::serve_order();
            cook_order();
        }

        fn cook_order() {}

        pub struct Breakfast {
            pub toast: String,
            // 虽然结构体是公有的，但字段默认还是私有的，需要用pub指定
            season_fruit: String,
        }

        impl Breakfast {
            pub fn summer(toast: &str) -> Breakfast {
                Breakfast {
                    toast: String::from(toast),
                    season_fruit: String::from("peaches"),
                }
            }
        }

        pub enum Appetizer {
            // 以下两个变体是公开的
            Soup,
            Salad,
        }
    }
}
