package generic;

import java.util.ArrayList;
import java.util.List;

public class CaptureConversion {

    public static void testGetSet(ArrayList<?> list){
        getSetHelper(list);
    }

    public static <T>  void getSetHelper(ArrayList<T> list){
        list.add(list.get(0));
    }

    public static void main(String[] args) {
        List<?> list;
        if(false){
            list = new ArrayList<Integer>(){{
                this.add(new Integer(10));
            }};
        }else{
            list = new ArrayList<Double>(){{
                this.add(new Double(10.1));
            }};
        }
        f1(list);
        Object d = f1Helper(list);
    }

    static void f1(List<?> list) {
        putHelper(list);
    }

    static <T> T putHelper(List<T> list){
        list.add(list.get(0));
        return list.get(0);
    }

    // helper method created so that the wildcard can be captured
    // through type inference
    // the compiler uses inference to dertermine T is CAP#1
    static <T> T f1Helper(List<T> list) {
        T item = list.get(0);
        System.out.println(item.getClass().getSimpleName());
        return item;
    }

}
