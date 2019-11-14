package generic;

import java.util.ArrayList;


class Food{}

class Meat extends Food{}

class Fruit extends Food{}

class Apple extends Fruit {}

class Banana extends Fruit {}

public class TypeTransform {
    public static void main(String[] args){
        ArrayList<? extends Fruit> fruits = new ArrayList<Apple>();
        // 以下代码都无法通过编译，使用了上界通配符之后，就无法对其赋值，但可以正常取出
        //fruits.add(new Fruit());
        //fruits.add(new Apple());
        //fruits.add(new Food());
        Fruit f = fruits.get(0);

        ArrayList<? super Fruit> fruits2 = new ArrayList<Food>();
        fruits2.add(new Fruit());
        fruits2.add(new Apple());
        // 任何Fruit的超类List都可以赋值到fruits2，所以fruits2里的元素只能是Fruit或者其子类才能满足这个要求
        // 以下句子无法通过编译，因为如果fruits2的值是ArrayList<Fruit>，此时的Food无法转型成Fruit
        // fruits2.add(new Food());
        // 以下代码无法通过编译，使用了下界通配符之后，无法确定获取的返回值，因为fruits的元素可能是任何Fruit的超类对象
        // Fruit f2 = fruits2.get(0);
        // 毕竟，可以把任何对象赋值给Object
        Object f3 = fruits2.get(0);

        ArrayList<?> list = new ArrayList<Fruit>();
        // 因为无法确认list的边界，所以以下4个语句无法编译通过
        // list.add(new Food());
        // list.add(new Fruit());
        // list.add(new Apple());
        // Food f1 = list.get(0);
        Object o1 = list.get(0);


        ArrayList list2 = new ArrayList<Fruit>();
        list2.add(new Food());
        list2.add(new Fruit());
        list2.add(new Meat());
        list2.add(new Integer(10));
        Object o2 = list2.get(0);

        // 泛型的不变性，以下语句无法编译通过
        // ArrayList<Object> list3 = new ArrayList<Fruit>();
    }
}

