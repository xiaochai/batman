import org.junit.Test;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Thread {
    ExecutorService executorService = Executors.newSingleThreadExecutor();
    @Test
    public void test(){
        getInt();
//        getInt2();
        System.out.println("test");

        try {
            java.lang.Thread.sleep(100000);
        }catch (InterruptedException e){
            System.out.println(e);
        }
    }

    public void getInt(){
        executorService.execute(()->{
            System.out.println("getInt");
            getInt2();
            try {
                java.lang.Thread.sleep(2000);
            }catch (InterruptedException e){
                System.out.println(e);
            }
        });
    }

    public void getInt2(){
        executorService.execute(()-> {
            System.out.println("getInt2");
            try {
                java.lang.Thread.sleep(2000);
            }catch (InterruptedException e){
                System.out.println(e);
            }
        });

    }



}
