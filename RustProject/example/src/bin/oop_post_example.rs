// 实现状态模式
// Post表示发布文章的流程，从草稿到审核再到发布
fn main() {
    let mut post = Post::new();
    post.add_text("I'm greate");
    println!("after add_text:{}", post.content());
    post.request_review();
    println!("after request_review:{}", post.content());
    post.approve();
    println!("after approve:{}", post.content());

}


struct Post {
    // 状态机：草稿->审核->发布
    // 为什么要用Option呢？如果不用Option在处理所有权的时候会比较麻烦，在所有权处理的时候需要注意
    state: Option<Box<dyn State>>,
    content: String,
}

impl Post {
    fn new() -> Post {
        Post {
            // 默认是草稿的状态
            state: Some(Box::new(Draft {})),
            content: String::new(),
        }
    }
    fn add_text(&mut self, text: &str) {
        self.content.push_str(text)
    }
    fn request_review(&mut self){
        // 将这些操作都代理给state，这些处理会导致新的状态，替换原来旧的状态
        // Option::take()获取变量的所有权，在这里把state的所有权取出，因为马上就要使用新的state来替换进去了
        if let Some(s) = self.state.take(){
            self.state = Some(s.request_review())
        }
    }    
    fn approve(&mut self){
        if let Some(s) = self.state.take(){
            self.state = Some(s.approve())
        }
    }
    fn content(&self)-> &str{
        self.state.as_ref().unwrap().content(self)
    }
}

trait State {
    fn request_review(self: Box<Self>) -> Box<dyn State>;
    fn approve(self: Box<Self>) -> Box<dyn State>;
    fn content<'a>(&self, post:&'a Post)->&'a str{
        ""
    }
}

struct Draft {}
impl State for Draft {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        Box::new(PendingReview{})
    }
    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }
}

struct PendingReview {}
impl State for PendingReview {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }
    fn approve(self: Box<Self>) -> Box<dyn State> {
        Box::new(Publish{})
    }
}

struct Publish {}
impl State for Publish {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }
    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }
    fn content<'a>(&self, post:&'a Post)->&'a str{
        &post.content
    }
}
