package DesignPatterns.Restaurent.Iterator;

import java.util.Iterator;

public class ArrayMenuIterator implements Iterator<MenuItem> {
    private MenuItem[] menuItems;
    int pos = 0;

    public ArrayMenuIterator(MenuItem[] menuItems) {
        this.menuItems = menuItems;
    }

    @Override
    public boolean hasNext() {
        return pos < menuItems.length && menuItems[pos] != null;
    }

    @Override
    public MenuItem next() {
        return menuItems[pos++];
    }
}
