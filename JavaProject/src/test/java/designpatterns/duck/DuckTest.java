/*
 * This Java source file was generated by the Gradle 'init' task.
 */
package designpatterns.duck;

import org.junit.Test;

public class DuckTest {
    @Test public void testFakeDuck() {
        Duck duck = new FakeDuck();
        duck.performFly();
        duck = new NormalDuck();
        duck.performFly();
    }
}
