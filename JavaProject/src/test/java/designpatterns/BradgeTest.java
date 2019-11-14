package designpatterns;

import designpatterns.bridge.*;
import org.junit.Test;

public class BradgeTest {
    @Test
    public void test(){
        Software software1 = new Software1();
        Software software2 = new Software2();
        OS osa = new OSA(software1);
        OS osb = new OSB(software1);
        osa.run();
        osb.run();
    }
}
