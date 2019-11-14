package designpatterns;


import designpatterns.restaurent.Combine.Menu;
import designpatterns.restaurent.Iterator.MenuCombine;
import designpatterns.restaurent.MenuItem;
import designpatterns.restaurent.MenuOfA;
import designpatterns.restaurent.MenuOfB;
import org.junit.Test;

public class MenuTest {
    @Test
    public void old() {
        MenuOfA a = new MenuOfA();
        MenuOfB b = new MenuOfB();
        for (MenuItem i : a.getMenuItems()) {
            System.out.printf("A:%s, %s, %f\n", i.name, i.desc, i.price);
        }

        for (int j = 0; j < b.getNumOfItem(); j++) {
            MenuItem i = b.getMenuItems()[j];
            System.out.printf("B:%s, %s, %f\n", i.name, i.desc, i.price);
        }
    }

    @Test
    public void testIterator() {
        MenuCombine menuCombine = new MenuCombine();
        menuCombine.printAll();
    }

    @Test
    public void testCombine() {
        Menu all = new Menu("all");
        Menu menuA = new Menu("A");
        Menu menuB = new Menu("B");
        Menu menuC = new Menu("C");
        menuA.add(new designpatterns.restaurent.Combine.MenuItem("拉面", "兰州拉面", 9.9));
        menuB.add(new designpatterns.restaurent.Combine.MenuItem("尖椒腊肉盖饭", "尖椒腊肉盖饭", 11.9));


        Menu menuCDrink = new Menu("C Drink");
        menuCDrink.add(new designpatterns.restaurent.Combine.MenuItem("啤酒", "哈尔滨啤酒", 99.9));
        menuC.add(new designpatterns.restaurent.Combine.MenuItem("羊肉串", "羊肉串", 5.9));
        menuC.add(menuCDrink);

        all.add(menuA);
        all.add(menuB);
        all.add(menuC);
        all.print();
    }
}
