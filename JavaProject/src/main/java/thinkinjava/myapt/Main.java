package thinkinjava.myapt;

public class Main {
    public static void main(String[] args){
        POJO pojo = new POJO(10);
//        System.out.println(pojo.getId());
    }
}


@MyService()
class POJO {
    private int id;

    public POJO(int id) {
        this.id = id;
    }
}