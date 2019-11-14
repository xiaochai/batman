package generic;

public class Tuple<E,F> {
    private final E first;
    private final F second;
    public Tuple(E a, F b){
        first = a;
        second = b;
    }
    public E getFirst(){
        return first;
    }
    public F getSecond(){
        return second;
    }
    public static void main(String[] args){
        Tuple<Integer, String> tuple = new Tuple<>(10, "人");
        Integer i = tuple.getFirst();
        String s = tuple.getSecond();
    }
}