package DesignPatterns;

import DesignPatterns.Builder.Company;
import DesignPatterns.Builder.CompanyBuilder;
import org.junit.Test;

public class BuilderTest {
    @Test
    public void test(){
        CompanyBuilder companyBuilder = new CompanyBuilder("公司", "北京", "010 2222222");

        Company company = companyBuilder.setVary("图书").setPostcode("1111111").build();
        System.out.println(company.toString());
    }
}
