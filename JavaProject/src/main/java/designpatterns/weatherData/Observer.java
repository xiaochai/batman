package designpatterns.weatherData;

public interface Observer {
    public void update(float temp, float humidity, float pressure);
}
