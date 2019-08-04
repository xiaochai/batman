package DesignPatterns.Builder;


public class Company {
    private String name, address, postcode, tel, vary;

    public Company(String name, String address, String postcode, String tel, String vary) {
        this.name = name;
        this.address = address;
        this.postcode = postcode;
        this.tel = tel;
        this.vary = vary;
    }

    @Override
    public String toString() {
        return name + address + postcode + tel + vary;
    }
}
