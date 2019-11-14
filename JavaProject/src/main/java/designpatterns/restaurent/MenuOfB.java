package designpatterns.restaurent;

public class MenuOfB {
    private static final int MAX_ITEMS = 6;
    private MenuItem[] menuItems;
    private int numOfItem;

    public MenuOfB() {
        menuItems = new MenuItem[MAX_ITEMS];
        addItem("尖椒腊肉盖饭", "尖椒腊肉盖饭", 11.9);
        addItem("红烧肉盖饭", "红烧肉盖饭", 21.9);
    }

    public void addItem(String name, String desc, double price) {
        MenuItem i = new MenuItem(name, desc, price);
        if(numOfItem >= MAX_ITEMS){
            return; //
        }
        menuItems[numOfItem++] = i;
    }

    public MenuItem[] getMenuItems(){
        return menuItems;
    }

    public int getNumOfItem() {
        return numOfItem;
    }
}
