package generic.selfbounded;

public class CreatureSelfBounded<T extends CreatureSelfBounded<T>> {
    private String species;

    public T setSpecies(String species) {
        this.species = species;
        return (T) this;
    }

    public String getSpecies() {
        return species;
    }

    public static void main(String[] args) {
        CatSelfBounded cat = new CatSelfBounded().setSpecies("cat").setCoatColor("red");
        System.out.println(String.format("%s,%s", cat.getSpecies(), cat.getCoatColor()));

        // 以下无法通过编译
        //SmallCatSelfBounded scat = new SmallCatSelfBounded().setSpecies("smallcat").setSize();
    }
}

class CatSelfBounded extends CreatureSelfBounded<CatSelfBounded> {
    private String coatColor;

    public CatSelfBounded setCoatColor(String coatColor) {
        this.coatColor = coatColor;
        return this;
    }

    public String getCoatColor() {
        return coatColor;
    }
}

class SmallCatSelfBounded extends CreatureSelfBounded<CatSelfBounded>{
    private Integer size;
    public SmallCatSelfBounded setSize(Integer size){
        this.size = size;
        return this;
    }
    public Integer getSize(){
        return size;
    }

}
