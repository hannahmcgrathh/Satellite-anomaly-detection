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

This one file contains ALL the code for the project. It is organised into seven sections (these sections are more accessible in R-markdown, where they are foldable):

1. **Initial set up**. Loads satellite data from the working directory. Loads necesssary packages.
2. **Maneuvers**. True manuevers are added as a boolean to the dataframe containing the satellite data. Different approaches are necessary for the differently formatted data here
3. **EDA**. Contains functions (calls are commented out) for various data visualisations
    * 'plot_satellite_data' generates a plotly (interactive) plot for every orbital element, including maneuvers
    * 'plot_satellite_data_no_man' as above, no maneuvers shown.
    * 'plot_seasonal_stack' shows three years of data for element, stacked on top of each other.
4. **Cleaning**. Cleans data, removing periods of noise / periods in which a satellite is transitioning to another orbit
5. **Models**. Trains and evaluates machine learning and statistical models. Iterates through each model for each satellite
6. **Evaluation**. Uses data from models to create dataframes summarising performance metrics. Also contains functions for Precision-Recall curves
7. **Summary plots**. Creates plots visualising the performance metrics of the various models 

Upon running this entire file, the following will be generated:

* dataframe 'eval_metrics' shows the F-score, best precision given recall >0.8, best recall given precision > 0.8, false negative rate for every model for every satellite
* dataframe 'summary_metrics' shows the average of these metrics across each of the three models
* plots visualising these performance metrics (some excluding outlier jason-1)


### Files
As such, the 2LE unpropagated orbital element data for each satellite should be in the working directory, with filenames '(un)propagated_elements_[satellite].csv'
The maneuver data in folder './manoeuvres'

## Requirements
To run this project, you need the following:

### Software
R (>= 4.0.0)
Ensure R is installed on your system.

### R Libraries
The script uses several R packages. Install them using the following commands:
     
  install.packages(c("tidyverse", "xgboost", "data.table", "lubridate", "ggplot2", "plotly", "forecast", "dplyr", "yardstick", "patchwork"))
      
Then simply run the entire R file (i.e. Ctrl+shift+enter on windows)
