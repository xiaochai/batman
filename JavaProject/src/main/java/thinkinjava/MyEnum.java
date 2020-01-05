package thinkinjava;

import generic.Generator;

import java.util.EnumMap;
import java.util.EnumSet;

public class MyEnum {
    public static void main(String[] args){
        EnumSet<Signal> signals = EnumSet.noneOf(Signal.class);
        signals.add(Signal.YELLOW);
        System.out.println(signals); // [YELLOW]
        signals.addAll(EnumSet.of(Signal.RED, Signal.GREEN));
        System.out.println(signals); // [GREEN, YELLOW, RED]
        signals = EnumSet.allOf(Signal.class);
        System.out.println(signals); //[GREEN, YELLOW, RED]
        signals.removeAll(EnumSet.of(Signal.RED));
        System.out.println(signals); // [GREEN, YELLOW]
        signals.removeAll(EnumSet.range(Signal.GREEN, Signal.RED));
        System.out.println(signals); // []
        signals = EnumSet.complementOf(signals);
        System.out.println(signals); // [GREEN, YELLOW, RED]

        EnumMap<Signal, Integer> enumMap = new EnumMap<Signal, Integer>(Signal.class);
        enumMap.put(Signal.RED, 1);
        enumMap.put(Signal.RED, 2);
        enumMap.put(Signal.GREEN, 3);
        enumMap.put(Signal.YELLOW, 4);
        System.out.println(enumMap); // {GREEN=3, YELLOW=4, RED=2}
    }
}

interface Food {
    enum Appetizer implements Food{
        SALAD, SOUP, SPRING_ROLLS
    }
    enum MainCourse implements Food{
        LASAGNE,PAD_THAI,LENTIS
    }
}


enum Signal implements Generator<Signal>{
    GREEN, YELLOW, RED;

    @Override
    public Signal next() {
        switch (this){
            case RED: return GREEN;
            case GREEN:return YELLOW;
            case YELLOW:return RED;
            default:
                return RED;
        }
    }

    public static <T> T printNext(Generator<T> g){
        T t = g.next();
        System.out.print(t + ",");
        return t;
    }
    public static void main(String[] args){
        Signal s = GREEN;
        for(int i = 0; i < 10; i++){
            s = printNext(s);
            //YELLOW,RED,GREEN,YELLOW,RED,GREEN,YELLOW,RED,GREEN,YELLOW,
        }
    }
}

enum LikeClasses{
    WINKEN{
        @Override
        void behavior() {
            System.out.println("behavior");
        }
    };
    abstract void behavior();
}