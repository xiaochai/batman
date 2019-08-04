package DesignPatterns.Restaurent.Iterator;

import java.util.HashMap;
import java.util.Iterator;

public class MenuCombine {
    HashMap<String, Menu> menus;
    public MenuCombine(){
        menus = new HashMap<>();
        menus.put("A",new MenuOfA());
        menus.put("B", new MenuOfB());
    }
    public void printAll(){
        for(String name :menus.keySet()){
            Iterator<MenuItem> i = menus.get(name).createIterator();
            while (i.hasNext()){
                MenuItem menuItem = i.next();
                System.out.printf("%s 餐厅: %s,%s,%f\n", name, menuItem.name, menuItem.desc, menuItem.price);
            }
        }
    }
}
