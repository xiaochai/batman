package designpatterns;

import designpatterns.chain.Plugin1Handler;
import designpatterns.chain.Plugin2Handler;
import designpatterns.chain.PluginManager;
import designpatterns.chain.Request;
import org.junit.Test;

public class ChainTest {
    @Test
    public void test(){
        Request request = new Request();
        PluginManager pluginManager = new PluginManager(request);
        pluginManager.addPlugin(new Plugin1Handler());
        pluginManager.addPlugin(new Plugin2Handler());
        pluginManager.run();
    }
}
