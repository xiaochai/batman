package designpatterns;

import designpatterns.state.StateMachine;
import org.junit.Test;

public class StateTest {
    @Test
    public void number() {
        String[] ss = {
                "0.1111 2.3.23.2.32.3.23",
                "333.222 322.321",
                "324. 234. 0.",
                ".234 .3232 .3242",
                "242 23a 342"
        };

        for (String s : ss) {
            StateMachine stateMachine = new StateMachine();
            String[] arr = s.split("");
            try {
                for (String ch : arr) {
                    if (ch.equals(".")) {
                        stateMachine.meetDot();
                    } else if (ch.equals(" ")) {
                        stateMachine.meetSpace();
                    } else if (Integer.parseInt(ch) >= 0) {
                        stateMachine.meetNumber();
                    }
                }
                System.out.printf("ok %s \n", s);
            } catch (NumberFormatException e) {
                System.out.printf("not dot or number %s \n", s);
            } catch (UnsupportedOperationException e) {
                System.out.printf("format error %s \n", s);
            }
        }
    }
}

