package DesignPatterns.Pizza.FactoryInterface;

import DesignPatterns.Pizza.CheesePizza;
import DesignPatterns.Pizza.NormalPizz;
import DesignPatterns.Pizza.Pizza;
import DesignPatterns.Pizza.VeggiePizza;

public class PizzaFactory2 implements PizzaFactory {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if (type.equals("veggie")) {
            pizza = new VeggiePizza();
        } else {
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
