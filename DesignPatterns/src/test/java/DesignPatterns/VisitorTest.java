package DesignPatterns;

import DesignPatterns.Visitor.Element;
import DesignPatterns.Visitor.ElementA;
import DesignPatterns.Visitor.ElementB;
import DesignPatterns.Visitor.Visitor;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;


public class VisitorTest {
    @Test
    public void test(){
        List<Element> list = new ArrayList<>();
        Visitor visitor = new Visitor();
        list.add(new ElementA());
        list.add(new ElementB());
        for(Element t: list){
            t.accept(visitor);
        }

    }
}
