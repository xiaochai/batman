package DesignPatterns.Pizza;

public class SimplePizzaFactory {
    public Pizza createPizza(String type){
        Pizza pizza;
        if(type.equals("cheese")){
            pizza = new CheesePizza();
        }else if(type.equals("veggie")){
            pizza = new VeggiePizza();
        }else{
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
