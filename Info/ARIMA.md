<script type="text/javascript"
  src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

#### ARIMA

The ARIMA forecasting model uses a regression-type equation in which the predictors consist of lags of the dependent variable and/or lags of the forecast errors. In an nonseasonal ARIMA model, $\mathbf{p}$ is the number of autoregressive terms, $\mathbf{d}$ is the number of nonseasonal differences needed for stationarity, and $\mathbf{q}$ is the number of lagged forecast errors in the prediction equation. 

Some of the common ARIMA models are:

  * ARIMA(1,0,0) first-order autoregressive: $\widehat{Y}_t  =  \mu  +  \phi_1 Y_{t-1}$ 
  * ARIMA(0,1,0) random walk: $\widehat{Y}_t  =  \mu  +  Y_{t-1}$ 
  * ARIMA(1,1,0) differenced first-order autoregressive model: $\widehat{Y}_t = \mu + Y_{t-1} + \phi_1 (Y_{t-1}-Y_{t-2})$
  * ARIMA(0,1,1) simple exponential smoothing with growth: $\widehat{Y}_t  =  \mu  +  Y_{t-1} -  \theta_1 e_{t-1}$ 

See also [Wikipedia](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) and [Duke](https://people.duke.edu/~rnau/411arim.htm).

