package generic.compat;

public class SimpleHolder {

    private Object obj;
    public Object getObj() {
        return obj;
    }
    public void setObj(Object obj) {
        this.obj = obj;
    }
    public static void main(String[] args){
        SimpleHolder holder = new SimpleHolder();
        holder.setObj("Item");
        String s = (String)holder.getObj();
    }
}
