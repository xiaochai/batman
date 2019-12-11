package thinkinjava;

public class MyString {
    public static void main(String[] args){
        String s = new String("acccccd");
        System.out.println(s.length()); // 11
        System.out.println(s.charAt(2)); // c
        char[] cs = new char[10];
        s.getChars(1,2, cs, 0);
        System.out.println(new String(s.getBytes()));
        s.intern();
        System.out.println(s.matches("a?b?c?d?"));
    }
}
