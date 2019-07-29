package DesignPatterns.Pizza.FactoryMethod;

import DesignPatterns.Pizza.NormalPizz;
import DesignPatterns.Pizza.Pizza;
import DesignPatterns.Pizza.VeggiePizza;

public class PizzaStore2 extends PizzaStoreBase {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if(type.equals("veggie")){
            pizza = new VeggiePizza();
        }else{
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
