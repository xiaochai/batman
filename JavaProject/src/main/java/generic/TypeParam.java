package generic;

import java.util.Arrays;

public class TypeParam {
    public static void main(String[] args){
        Dummy dummy = new Dummy();
        dummy.set("hello world");
        String[] s = dummy.<String>get().split(" ");
        System.out.println(Arrays.toString(s));
    }
}


class Dummy {
    private Object object;
    public <T> T get(){
        return (T) object;
    }
    public <T> void set(T t){
        object = t;
    }
}
