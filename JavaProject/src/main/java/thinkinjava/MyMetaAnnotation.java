package thinkinjava;

import io.lettuce.core.dynamic.annotation.Param;

import java.lang.annotation.*;
import java.util.ArrayList;

public class MyMetaAnnotation<@InTypeParameter @InTypeUse T>{
    public @InTypeUse T f(){
        ArrayList<@InTypeUse Integer> t = new @InTypeUse ArrayList<>();
        return null;
    }
}

@Target({ElementType.TYPE_PARAMETER})
@interface InTypeParameter {}

@Target({ElementType.TYPE_USE})
@interface InTypeUse {}

