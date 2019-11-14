package designpatterns.weatherData;

public class Logger implements Observer {

    @Override
    public void update(float temp, float humidity, float pressure) {
        System.out.printf("current log data %f,%f,%f\n", temp, humidity, pressure);
    }
}
