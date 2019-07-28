package DesignPatterns.Coffee;

import org.junit.Test;

public class CoffeeTest {
    @Test
    public void testDeco(){
        ICoffee coffee = new Coffee();
        coffee = new Milk(coffee);
        coffee = new Soy(coffee);
        coffee = new Mocha(coffee);
        coffee = new Mocha(coffee);
        System.out.printf("%s costs %f\n", coffee.desc(), coffee.cost());
    }
}
