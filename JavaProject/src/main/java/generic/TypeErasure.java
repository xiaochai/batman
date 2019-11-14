package generic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class TypeErasure {
    public static void main(String[] args){
        Class c1 = new ArrayList<String>().getClass();
        Class c2 = new ArrayList<Integer>().getClass();
        System.out.println(c1); // java.util.ArrayList
        System.out.println(c2); // java.util.ArrayList
        System.out.println(c1==c2); // true

        Class c3 = new HashMap<String, Integer>().getClass();
        System.out.println(Arrays.toString(c1.getTypeParameters())); // [E]
        System.out.println(Arrays.toString(c2.getTypeParameters())); // [E]
        System.out.println(Arrays.toString(c3.getTypeParameters())); // [K, V]
    }
}
