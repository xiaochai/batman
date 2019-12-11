package thinkinjava;

import generic.Generator;

import java.util.EnumSet;

public class MyEnum {
    public static void main(String[] args){
        EnumSet<Signal> signals = EnumSet.noneOf(Signal.class);
        signals.add(Signal.YELLOW);
        System.out.println(signals);
        signals.addAll(EnumSet.of(Signal.RED, Signal.GREEN));
        signals = EnumSet.allOf(Signal.class);
        signals.removeAll(EnumSet.of(Signal.RED));
        signals.removeAll(EnumSet.range(Signal.GREEN, Signal.RED));
        EnumSet.complementOf(signals);
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
