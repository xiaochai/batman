package thinkinjava;

public class Multi {
    void f(){}
    class Inner1{
        void f1(){}
        class Inner2{
            void f2(){
                f();
                f1();
            }
        }
    }
}


class A {}
class B {}

class C extends A{
    B makeB(){
        return new B(){
        };
    }
}

class D{
    class E{}
}
class F extends D.E{
    public F(D d) {
        d.super();
    }
}