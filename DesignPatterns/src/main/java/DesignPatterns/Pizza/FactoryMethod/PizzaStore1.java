package DesignPatterns.Pizza.FactoryMethod;

import DesignPatterns.Pizza.CheesePizza;
import DesignPatterns.Pizza.NormalPizz;
import DesignPatterns.Pizza.Pizza;

public class PizzaStore1 extends PizzaStoreBase {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if(type.equals("cheese")){
            pizza = new CheesePizza();
        }else{
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
