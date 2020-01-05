package mynetty;

import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisFuture;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.async.RedisAsyncCommands;
import io.lettuce.core.api.reactive.RedisStringReactiveCommands;
import io.lettuce.core.api.sync.RedisCommands;

import java.util.concurrent.TimeUnit;

public class MyLettuce {
    public static void main(String[] args){
        RedisClient redisClient = RedisClient.create("redis://0533583f64d33ea5@10.208.255.208:6789/0");
//        StatefulRedisConnection<String, String> connection = redisClient.connect();
//        RedisCommands<String, String> syncCommands = connection.sync();
//
//        syncCommands.set("key", "Hello, Redis!");
//
//        connection.close();


        RedisAsyncCommands<String, String> commands = redisClient.connect().async();

        try {
            RedisFuture<String> future = commands.get("key");
            String value = future.get(1, TimeUnit.MINUTES);
            System.out.println(value);
        } catch (Exception e) {
            e.printStackTrace();
        }

//
//
//        MyLettuce.printThread();
//
//        RedisStringReactiveCommands<String, String> commands = redisClient.connect().reactive();
//        commands.get("key").subscribe(value -> {
//            System.out.println(value);
//            MyLettuce.printThread();
//        });
//
//
//        try{
//            Thread.sleep(1000);
//        }catch (InterruptedException e ){
//            e.printStackTrace();
//        }

        redisClient.shutdown();
    }

    public static void printThread(){
        Thread t = Thread.currentThread();
        System.out.println(t.getName());
    }
}
