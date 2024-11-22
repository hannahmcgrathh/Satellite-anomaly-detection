# Satellite anomaly detection

Detecting anomalies in satellite orbits is hugely important, particularly given the increasing number of satellites and the importance of collision avoidance. In this project, I aim to identify anomalies (here maneuvers) from publicly-available 18th Space Defence Squadron data. I will compare three approaches: ARIMA, ARIMAX and XGBoost, and look at the results across seven satellites:
* CryoSat-2
* Fengyun-2D
* Fengyun-2E
* Jason-1
* Jason-3
* Sentinel-3A
* Sentinel-3B

The residuals generated by the forecasted element should indicate anomalies in the orbit, and thus should correspond to ground-truth maneuver data.


## Files
**satelliteanomalies.R**

The main R script that:

* Loads satellite data from the working directory.
* Cleans data, removing periods of noise / periods in which a satellite is transitioning to another orbit
* Trains and evaluates machine learning and statistical models.
* Generates plots and reports for anomaly detection:
    * dataframe 'eval_metrics' shows the F-score, best precision given recall >0.8, best recall given precision > 0.8, false negative rate for every model for every satellite
    * dataframe 'summary_metrics' shows the average of these metrics across each of the three models
    * plots visualising these performance metrics

As such, the 2LE unpropagated orbital element data for each satellite should be in the working directory, with filenames '(un)propagated_elements_[satellite].csv'

## Requirements
To run this project, you need the following:

### Software
R (>= 4.0.0)
Ensure R is installed on your system.

### R Libraries
The script uses several R packages. Install them using the following commands:

      
  install.packages(c("tidyverse", "xgboost", "data.table", "lubridate", "ggplot2", "plotly", "forecast", "dplyr", "yardstick"))
      

