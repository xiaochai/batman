package DesignPatterns;

import DesignPatterns.Chain.Plugin1Handler;
import DesignPatterns.Chain.Plugin2Handler;
import DesignPatterns.Chain.PluginManager;
import DesignPatterns.Chain.Request;
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
