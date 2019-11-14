package generic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class Pecs {

    // Comparable需要用super的理由是如果实现Comparable的不一定是本类，也有可能是基类，这个? super T涵盖了这种情况
    // 如果直接使用Comparable<T>，并且Collection没有使用extends的话，则max(apples)将会编译失败
    // 参数Collections添加? extends T，是为了在函数中不修改这个集合
    public static <T extends Comparable<? super T>> T max(Collection<? extends T> c) {
        if (c.isEmpty()) {
            return null;
        }
        Iterator<? extends T> iterator = c.iterator();
        T max = iterator.next();
        while (iterator.hasNext()) {
            T t = iterator.next();
            if (t.compareTo(max) > 0) {
                max = t;
            }
        }
        return max;
    }

    // src只读，dest只写
    public static <T> void copy(ArrayList<? extends T> src, ArrayList<? super T> dest) {
        for (int i = 0; i < src.size(); i++) {
            dest.set(i, src.get(i));
        }
    }

    public static void main() {
        List<Fruit> fruits = new ArrayList<Fruit>();
        List<Apple> apples = new ArrayList<Apple>();
        max(fruits);
        max(apples);
    }


    class Fruit implements Comparable<Fruit> {
        @Override
        public int compareTo(Fruit o) {
            return 0;
        }
    }

    class Apple extends Fruit {
    }
}
