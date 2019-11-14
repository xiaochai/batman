package generic.selfbounded;

public class Creature {
    private String species;
    public void setSpecies(String species){
        this.species = species;
    }
    public String getSpecies(){
        return species;
    }
    public static void main(String[] args){
        Cat cat = new Cat();
        cat.setSpecies("cat");
        cat.setCoatColor("red");
        System.out.println(String.format("%s,%s", cat.getSpecies(), cat.getCoatColor()));
    }
}

class Cat extends Creature{
    private String coatColor;
    public void setCoatColor(String coatColor){
        this.coatColor = coatColor;
    }
    public String getCoatColor(){
        return coatColor;
    }
}