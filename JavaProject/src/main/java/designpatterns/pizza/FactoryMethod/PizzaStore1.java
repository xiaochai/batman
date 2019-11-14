package designpatterns.pizza.FactoryMethod;

import designpatterns.pizza.CheesePizza;
import designpatterns.pizza.NormalPizz;
import designpatterns.pizza.Pizza;

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
