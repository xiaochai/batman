package designpatterns.restaurent.Iterator;

import java.util.Iterator;

public interface Menu {
    public Iterator<MenuItem> createIterator();
}
