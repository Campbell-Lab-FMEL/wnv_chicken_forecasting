# [Toward Ecological Forecasting of West Nile Virus in Florida: Insights from Two Decades of Sentinel Chicken Surveillance](https://doi.org/10.32942/X2QH09)

### [J. Alex Baecher](https://www.alexbaecher.com/), V. A. Akshay, [Robert P. Guralnick](https://www.gurlab.net/), Amy M. Bauer, Yasmin N. Tavares, Yesenia Sánchez, [James T. Thorson](https://sites.google.com/site/thorsonresearch/), [Lindsay P. Campbell](https://lcampbelllab.wixsite.com/campbell-lab/)

### *In Review at* Science of the Total Environment 

### Preprint: [EcoEvoRxiv]([https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14282](https://doi.org/10.32942/X2QH09))

### Please contact the first author for questions about the code or data: J. Alex Baecher (jbaecher@gmail.com)
__________________________________________________________________________________________________________________________________________

## Abstract:  
West Nile Virus (WNV) is the leading cause of mosquito-borne disease in the United States, yet transmission activity remains difficult to predict. The present study used 20 years of digitized WNV seroconversion data from 526 sentinel chicken coops across Florida to develop spatiotemporal models with landscape and climate variables to predict WNV seroconversion at monthly and seasonal timescales. We found several environmental predictors hypothesized to impact WNV transmission were important at both timescales. Seroconversion of WNV was predicted to decrease with higher maximum temperatures during the sampling month and increase with higher precipitation and minimum temperature two months prior to sampling. In the seasonal model, intermediate values of cumulative precipitation in the previous season predicted higher WNV seroconversion. High accuracy in out-of-sample predictions at both timescales demonstrated the utility of our models toward ecological forecasting of enzootic transmission. Monthly models had higher precision than the seasonal model, but both timescales have potential to inform management decisions. Monthly predictions could guide targeted control efforts during active transmission seasons, while seasonal predictions provide a lead-time to improve preparedness and inform resource allocation. Retrospective statewide predictions across the 20-year time period provided qualitative correlations between areas of high predicted WNV transmission hazard among humans and equines, while also providing insights into WNV transmission ecology following its introduction in 2001. Overall, our framework provides a step forward in the use of spatiotemporal ecological modeling for public health and vector-borne disease ecology and management.

## Repository Directory

### [scripts](./scripts): Contains code for conducting an modeling spatiotemporal WNV transmission dynamics
  - [`scripts/data_processing`](./scripts/data_processing): Contains scripts for processing WNV monitoring data
  - [`scripts/environmental_data`](./scripts/environmental_data): Contains scripts for assembling environmental predictor data
  - [`scripts/models`](./scripts/models): Contains scripts for executing temporal and spatiotemporal models
    - [`glmmtmb`](./scripts/models/glmmtmb): Contains script for temporal modeling using [glmmTMB](https://github.com/glmmTMB/glmmTMB)
      - [`base_model_development.R`](./scripts/models/glmmtmb/base_model_development.R): Script for model selection and variable reduction
    - [`sdmtmb`](./scripts/models/sdmtmb): Contains scripts for spatiotemporaltemporal modeling with [sdmTMB](https://pbs-assess.github.io/sdmTMB/)
      - [`fitting`](./scripts/models/sdmtmb/fitting): Contains scripts for model calibration and selection
        - [`model_calibration.R/`](./scripts/models/sdmtmb/fitting/model_calibration.R): Fitting `sdmTMB` model
        - [`model_selection.R/`](./scripts/models/sdmtmb/fitting/model_selection.R): Comparing model metrics for **SI Table 1** and **SI Table 2**
      - [`prediction`](./scripts/models/sdmtmb/prediction): Contains scripts for obtaining predictions
        - [`calculate_rmse.R/`](./scripts/models/sdmtmb/fitting/calculate_rmse.R): Calculate rmse values for manuscript **Table 1**
        - [`point_based.R/`](./scripts/models/sdmtmb/fitting/point_based.R): Comparing epsilon-correction and plotting for **Figure 2** 
        - [`response_curves.R/`](./scripts/models/sdmtmb/fitting/response_curves.R): Obtaining predictions for response curves in **Figure 3**
        - [`statewide_preds.R/`](./scripts/models/sdmtmb/fitting/statewide_preds.R): Obtaining predictions for statewide plots in **Figure 4** and **Figure 5**
### [functions](./functions): Contains helper functions for executing code contained in [scripts](./scripts)
  - [`functions/buildmermod_to_glmmtmb.R`](./functions/buildmermod_to_glmmtmb.R): Converting `buildmermod` objects to `glmmTMB` objects
  - [`functions/compare_models.R`](./functions/compare_models.R): Automatically fits `glmmTMB` models and performs AIC selection
  - [`functions/extract_model_data.R`](./functions/extract_model_data.R): Custom methods for extracting parameter estimates (excluding unwanted parameters)
  - [`functions/stepwise_vif.R`](./functions/stepwise_vif.R): Iteratively performs VIF calculations while removing multicollinear variables
  - [`functions/truncate.R`](./functions/truncate.R): Truncates raster values based on supplied threshold values to improve visualization

## Data
Georeferenced sentinel chicken seroconversion data is available upon request through the Florida Department of Health Arbovirus Surveillance program upon agreement from participating Florida mosquito control programs through a memorandum of understanding. The authors did not receive special privileges in accessing the data that other researchers would not have. Contact information for data requests are available through the [FDOH website](https://www.floridahealth.gov/diseases-and-conditions/mosquito-borne-diseases/surveillance.html). 
