package generic.selfbounded;

public class CreatureL {
    private String species;
    public CreatureL setSpecies(String species){
        this.species = species;
        return this;
    }
    public String getSpecies(){
        return species;
    }
    public static void main(String[] args){
        // 以下代码无法通过编译
        // CatL cat = new CatL().setSpecies("cat").setCoatColor("red");
        // 并没有达到链式调用的目的
        CatL cat = new CatL();
        cat.setSpecies("cat");
        cat.setCoatColor("red");
        System.out.println(String.format("%s,%s", cat.getSpecies(), cat.getCoatColor()));
    }
}

class CatL extends CreatureL{
    private String coatColor;
    public CatL setCoatColor(String coatColor){
        this.coatColor = coatColor;
        return this;
    }
    public String getCoatColor(){
        return coatColor;
    }
}