package generic;

class Human extends Age implements Flyable, Runable{
    @Override
    public void fly() {}

    @Override
    public void run() {}
}

interface Flyable{
    void fly();
}

interface Runable{
    void run();
}

class Age{
    int age;
}

// 可以限制多个边界，使用 & 符号连接
// 边界为类的只能有一个，并且必须是第一个
// 接口可以是多个
public class Boundary<T extends Age & Flyable & Runable>{
    public T item;

    public Boundary(T item) {
        this.item = item;
    }
    public void fly(){
        item.fly();
    }
    public static void main(){
        // 写进Boundary的对象必须继承自Age，并且实现了Flyable和Runable
        Boundary<Human> h = new Boundary<>(new Human());
    }
}

// 如果继承了某个泛型类，注意至少需要定义同样的边界或者更多的边界限制
// class Boundary2<T extends Age> extends Boundary<T>这样就无法通过编译，因为这个T不满足Boundary的限制
class Boundary2<T extends Age & Flyable & Runable> extends Boundary<T>{
    public Boundary2(T item) {
        super(item);
    }
}
