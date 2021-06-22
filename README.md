

This repository contains code and data to reproduce the methods in 
An Endemic-Epidemic Beta Model for Time Series of Infectious Disease Proportions.

The code consists of two parts

* Application 1 compares goodness of fit of different variants of univariate beta model and SARIMA model. 

* Application 2 assesses forecasts of different variants of multivariate beta model. 


## Repository structure

* `Data/` contains the downloaded and prepared datasets and associated R scripts:

    * `Data/load_data.R` loads *national* wILI data through the R package [**cdcfluview**](https://CRAN.R-project.org/package=cdcfluview). Details about this wILI data please see http://www.cdc.gov/flu/weekly/. Note that when ILINet members provide revisions or backfill reports for past weeks, the wILI data will be updated accordingly. This means that data will have small difference when loading at different time points. The data used for comparison in this paper were downloaded on March 03 2020 and saved in `Data/usflu.RData`.

    * `Data/load_data_holiday.R` adds necessary columns in loaded *national* wILI data, including holidays and sin, cos terms and saves the data in `Data/data_holidays.RData`.
    
    * `Data/load_region_data.R` loads *regional* wILI data through the R package [**cdcfluview**](https://CRAN.R-project.org/package=cdcfluview). Details about this wILI data please see http://www.cdc.gov/flu/weekly/. Note that when ILINet members provide revisions or backfill reports for past weeks, the wILI data will be updated accordingly. This means that data will have small difference when loading at different time points. The data used for comparison in this paper were downloaded on March 03 2020 and saved in `Data/Regionflu.RData`.

    * `Data/load_region_data_holiday.R` adds necessary columns in loaded *regional* wILI data, including holidays, saves the data in `Data/Region_data_holidays.RData`, and creates an adjacency matrix of HHS regions in `Data/adj_matrix.RData`.
    
* `Appl1/` contains code for application 1.

    * `Appl1/SARIMA.R` fits standard SARIMA model ([**forecast**](https://CRAN.R-project.org/package=forecast)`::auto.arima`) using national wILI data with logit transformation.

    * `Appl1/Beta.R` fits different variants of univariate beta model using national wILI data. This script is `source`d in `Summary1.R` and `acf_plot.R`.

    * `Appl1/Summary1.R` compares the goodness of fit of beta models and SARIMA.
   
    * `Appl1/acf_plot.R` makes ACF plots of conditional pearson residuals of fitted models.
   
    * `Appl1/wILI_plots.R` makes the time series plots of national wILI data.
    
* `Appl2/` contains code for application 2.

    * `Appl2/fit_mBeta.R` and `Appl2/forecast_mBeta.R` define functions to fit a mBeta model and forecast from it respectively using [**betareg**](https://CRAN.R-project.org/package=betareg)`::betareg()`.
    
    * `Appl2/forecast_script.R` makes one-step-ahead forecast from mBeta models using first-order neighbour matrix.
        
    * `Appl2/forecast_script_pw.R` makes one-step-ahead forecast from mBeta models using power-law model.
    
    * `Appl2/fan_plots.R` makes fan plots from forecasts of Model M4.
    
    * `Appl2/coef_plot.R` makes confidence interval plots of fitted full mBeta model.
    
    * `Appl2/mBeta_summary.R` compares forecast performance of mBeta models.
    
    * `Appl2/mBeta_pw_summary.R` compares forecast performance of mBeta models using power-law model.
    
* `Results/` have been produced in R 4.1.0 using betareg 3.1-4 and forecast 8.15.
