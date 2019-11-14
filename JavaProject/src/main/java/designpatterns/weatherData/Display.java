package designpatterns.weatherData;

public class Display implements Observer {
    @Override
    public void update(float temp, float humidity, float pressure) {
        System.out.printf("current display data %f,%f,%f\n", temp, humidity, pressure);
    }
}
