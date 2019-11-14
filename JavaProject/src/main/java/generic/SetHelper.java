package generic;

import java.util.HashSet;
import java.util.Set;

public class SetHelper {
    public static <T> Set<T> union(Set<T> a, Set<T> b){
        Set<T> result = new HashSet<>(a);
        result.addAll(b);
        return result;
    }
    public static void main(String[] args){
        HashSet<Integer> a = new HashSet<>();
        a.add(10);
        HashSet<Integer> b = new HashSet<>();
        b.add(11);
        Set<Integer> u = SetHelper.union(a,b);
        System.out.println(u);
    }
}
