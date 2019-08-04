package DesignPatterns.Builder;

public class CompanyBuilder implements javafx.util.Builder<Company> {
    private String name, address, postcode, tel, vary;

    public CompanyBuilder(String name, String address, String tel) {
        this.name = name;
        this.address = address;
        this.tel = tel;
    }
    public CompanyBuilder setPostcode(String postcode){
        this.postcode = postcode;
        return this;
    }

    public CompanyBuilder setVary(String vary) {
        this.vary = vary;
        return this;
    }

    @Override
    public Company build() {
        return new Company(name, address, postcode, tel, vary);
    }
}

