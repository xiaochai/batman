package proxy;

public class Bird implements IBird{

    @Override
    public void fly(){
        System.out.println("fly");
    }

    @Override
    public void fly2(){
        fly(); // 动态代理无法再代理这个内部调用
        System.out.println("fly2");
    }
}
