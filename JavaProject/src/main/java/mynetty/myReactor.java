package mynetty;

import reactor.core.publisher.Flux;

import java.util.function.Consumer;

public class myReactor {
    public static void main(String[] args){
        Con<C> t1 = C::f2;
        Con<C> t2 = C::f;
    }
}


class C{
    public static void f(C c){
        System.out.println("static");
    }
    public void f2(){
        System.out.println("not static");
    }
}

@FunctionalInterface
interface Con<T>{
    public void f(T t);
}

interface Fun<T,R> { R apply(T arg); }

class D {
    int size() { return 0; }
//    static int size(Object arg) { return 0; }

    public static void main(String[] args){
        Fun<D, Integer> f1 = D::size;

        // Error: instance method size()
        // or static method size(Object)?
    }
}