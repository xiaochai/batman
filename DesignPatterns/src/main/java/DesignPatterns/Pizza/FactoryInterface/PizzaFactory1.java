package DesignPatterns.Pizza.FactoryInterface;

import DesignPatterns.Pizza.CheesePizza;
import DesignPatterns.Pizza.NormalPizz;
import DesignPatterns.Pizza.Pizza;

public class PizzaFactory1 implements PizzaFactory {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if (type.equals("cheese")) {
            pizza = new CheesePizza();
        } else {
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
