package generic;

public class Covariance {
    public static void main(String[] args){
        System.out.println(new Base1().f());
        System.out.println(new Derive1().f());
    }
}

class Base1{
    public Number f(){ return 3;}
}

class Derive1 extends Base1{
    @Override
    public Integer f(){return 4;}
}