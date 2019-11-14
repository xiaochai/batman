package designpatterns.weatherData;

public class BadWeatherData {
    private float temperature,humidity,pressure;

    public double getTemperature() {
        return temperature;
    }

    public double getHumidity() {
        return humidity;
    }

    public double getPressure() {
        return pressure;
    }

    public void measurementChanged(){
        Logger l = new Logger();
        Display d = new Display();
        l.update(temperature, humidity, pressure);
        d.update(temperature, humidity, pressure);
    }
}
