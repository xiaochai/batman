package DesignPatterns.Template;

public abstract class CaffeineBeverage {
    public void prepareRecipe(){

    }
    public abstract void brew();
    public abstract void addCondiments();
    public void boilWater(){/* boil water*/}
    public void pourInCup(){/* pour in cup*/}

}
