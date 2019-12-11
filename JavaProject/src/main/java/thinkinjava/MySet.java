package thinkinjava;

import java.util.*;

public class MySet {
    public static void main(String[] args) {
        SetT a = new SetT();
        SetT b = new SetT();
        HashSet<SetT> hashSet = new HashSet<>(10);
        hashSet.add(a);
        hashSet.add(b);
        System.out.println(hashSet.size());

        HashMap<Integer, Integer> hashMap = new HashMap<>(10, 0.1f);

        LinkedHashMap<Integer, Integer> linkedHashMap = new LinkedHashMap<>(10, 0.1f, true);


        Collection<String> c = new ArrayList<>();
        Iterator<String> it = c.iterator();
        c.add("aaa");
        String s = it.next(); // 抛出ConcurrentModificationException异常
    }
}

class SetT{
    @Override
    public int hashCode() {
        return 1;
    }

    @Override
    public boolean equals(Object obj) {
        return true;
    }
}