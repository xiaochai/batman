package designpatterns.visitor;

public class Visitor {
    public void visit(ElementA elementA){
        System.out.println("访问到A--->" + elementA.getName());
    }
    public void visit(ElementB elementB){
        System.out.println("访问到B--->" + elementB.getName());
    }

}
