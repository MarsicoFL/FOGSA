# Forensic Genetics Simulation Analysis (FOGSA)

## Description
Despite that the use of DNA in Missing Person Identification and Disaster Victim Identification cases has gained increasing focus during the last two decades, there still are several complications that the forensic scientists are faced with, for instance complex family structures and large scale comparisons are examples. Evaluating the impact of these complications in the identification process is one of the main tasks of the forensic genetics statistics. The development of algorithms and the application of statistical analysis in order to measure the uncertainty and minimize the probability of both false positive and false negatives types of error can be crucial to the purpose of the Missing Person Identification. FOGSA (Forensic Genetics Simulation Analysis) provides an userfriendly interface that allows the analysis of the simulation Raw Data from different softwares such as Familias R version and Forrel Package. FOGSA aimed at helping in the statistical analysis for decision making in forensic genetic caseworks.

## Installation
Please, download and unzip FOGSA-master.zip package. Once done, execute the App.R script with Rstudio and follow the instructions below.

### Installing pre-required packages
Some packages are required in order to execute FOGSA. Below the command lines for installing each package are shown:
```{r, eval = FALSE}
install.packages("shiny")
install.packages("shinydashboard")
install.packages("dplyr")
install.packages("plotly")
install.packages("crosstalk")
install.packages("tidyverse")
install.packages("viridis")
```

With this correctly done, we are able to run FOGSA app.

## Usage
### Generating simulation data with other softwares
FOGSA works with simulation raw data as in input. It could be generated by different softwares such as Familias (https://familias.no/), Famlink (http://www.famlink.se/) or Forrel package (https://github.com/magnusdv/forrel).
Lets take an example: the Family B is searching for a missing relative, the person 8. There is genetic information of the members 5 and 6. The identification will be done by database searching. 
So what we ask here is if we have enough statistical power to perform an identification and in the case that it is not, we want to select a Likelihood Ratio value in order to choose those cases where incorporate more genetic information (much more genetic markers for example).

![Screenshot](img/ExampleB.png)

### Input files for FOGSA
Once FOGSA is ran, the first step is uploading the output file from simulations previously generated. It should consist in two columns, one with the Likelihood ratios obtained under one hypothesis of pedigree and the other with those obtained under another hypothesis to be compared. 

### Power plot
For a given pedigree the statistical power of the DNA-based identification could be calculated from the simulated data. Two measures could help in order to characterize a family group:
Inclusion Power (IP): It is the probability of obtaining a likelihood ratio of zero when the tested person (POI) is not related with the family group.
Exclusion Power (EP): The probability of obtaining a likelihood ratio upper than 10000 when the tested person (POI) is actually the missing person (MP).
The Power Plot could be perfomed combining this two measures. A family group with low IP and EP has low statistical power where a high probability of false positives and false negatives could be expected. In this cases more genetic information must be incorporated to the pedigree. It could be done by the incorporation of more genetic markers or more members. As counterpart, a pedigree with high IP and EP allows performing an identification with an accurate result.
For example, for Family B we obtain the next powerplot:

![Screenshot](img/PowerPlot.png)


### Rates calculation in function of LR threshold selected
False positive and false negative rates could be estimated for each Likelihood ratio selected as threshold (please select one using the sliderbar). Taking into account this measures, Matthews Correlation Coefficient and Acuracy can be calculated. Both measures gives an idea about how accurate is the LR threshold selected. If you are working with massive comparisons in genetic databases the expected number of false positives could be approximated multiplying the false positive rate by the database size. Below we can see the obtained values for Family B considering a Likelihood Ratio of 1. Two different measures of accuracy are incorportad (Accuracy and Matthews Correlation Coefficient) that should be interpretated with caution, it doesn´t tell how accurate is the identification with the obtained LR, it only indicates how accurate is the clasification system based in the selected Likelihood Ratio threshold in a database searching context.

![Screenshot](img/Rates.png)


### Likelihood Ratio Decision Threshold
In massive comparisons carried out in Missing Person Identification cases, matches (LR values upper the LR threshold) are re-analyzed by geneticists and more genetic (Y-Chromosome, X-Chromosome and mitochondrial DNA) and non-genetic information (date of birth) is incorporated to the case in order to avoid false positives. In contrast, false negatives are not detected (low LR values) so they don't allow the re-analysis. With this in mind, calculations of true and false negatives rates with different numbers of LR thresholds (from 1 to 10000) for each family group could considered in order to select an appropiate LR threshold. This value could be used for the establishment of a LR threshold (named LR decision treshold or LRdt) with the aim of selecting those matches that need a re-evaluation with more genetic and non genetic information in order to minimize the probability of obtaining a false negative result.
The LR decision threshold could be selected taking into consideration:
- False negative rate minimization.
- Database size and resources for the re-analysis of the expected number of false positives (false positive rate * database size).
- The behavior of the LRdt plot. It indicates that, in some cases, increasing the LRdt value results in a high reduction of false positive rate whilst the false negative rate has a small increase.

Below the obtained plot for Family B is shown. The X axis correspond to the False Negative Rate and Y axis to False Positive Rate. Each point represent a LR value from 1 to 10000.

![Screenshot](img/LRdtPlot.png)


## Contributing
This software was developed and maintained by Franco Marsico. Any question or suggestion could be done sending an e-mail to franco.lmarsico@gmail.com

## License 
The FOGSA package is a free software; you can redistribute it and/or modify it under the terms of the GNU General Public License, version 3, as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose. See the GNU General Public License for more details.

A copy of the GNU General Public License, version 3, is available at https://www.r-project.org/Licenses/GPL-3
