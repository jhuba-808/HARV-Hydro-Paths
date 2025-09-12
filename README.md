# HF-Hydro-Paths

Code accompanying the paper "Shifting Hydrologic Pathways in Temperate Forest Headwaters Under a Changing Climate"

Huba, J., Boutt, D.F., Moran, B.J., Winnick, M., Matthes, J., Hare, D.

This repository contains R scripts designed to reproduce the analyses and visualizations presented in the above paper. The scripts cover several hydrological analyses including:
- Storm event identification 
- Piecewise Regression for non-linear streamflow response to precipitation
- Response time identification and analysis
- Calculation of DWAAI (dry-wet abrupt alternation index) (Shan et al., 2018)

## **Getting Started**
Prerequisites

R (version 4.4.1 or higher)

Required R packages: segmented, tidyverse, lubridate, zoo, dplyr, ggplot2, trend, patchwork, minpack.lm

## **Installation**
**For HARV_EVENTS, HARV_PWR, HARV_RESP:**

1. **Clone the repository**:
    ```bash
    git clone https://github.com/jhuba-808/HF-Hydro-Paths.git
    ```
2. Open the scripts in **RStudio** or any R environment.

3. Install any missing packages using:
    ```r
    install.packages("package_name")
    ```

For DWAAI

1. 

## **Usage**

Each script is self-contained with comments and instructions for use. Modify the input file paths and parameters as needed for your data.

## **Related Publication**

For detailed methodology, results, and discussion, please refer to the paper:

{Link to Paper}

## **Contact**

For questions, please contact:
Julianna C. Huba
University of Massachusetts-Amherst
Email: jhuba@umass.edu
