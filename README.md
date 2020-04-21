# Forensic Genetics Simulation Analysis (FOGSA)

## Description
Despite that the use of DNA in Missing Person Identification and disaster victim identification cases has gained increasing focus during the last two decades, there still are several complications that the forensic scientists are faced with, for instance complex family structures and large scale comparisons are examples. Evaluating the impact of these complications in the identification process is one of the main task of the forensic genetics statistics. The development of algorithms and the application of statistical analysis in order to measure the uncertainty and minimize the probability of both false positive and false negatives identification errors can be crucial to the purpose of the Missing Person Identification. FOGSA (Forensic Genetics Simulation Analysis) provides an userfriendly interface that allows the analysis of the simulation Raw Data from different softwares such as Familias R version and Forrel Package. FOGSA aim at helping in the statistical analysis for decision making in forensic caseworks.

## Installation
Alternatively, you can obtain the latest development version from GitHub:
```{r, eval = FALSE}
# install.packages("devtools") # install devtools if needed
devtools::install_github("MarsicoFL/FOGSApp")
```

## Usage
### Getting started
Some packages are required in order to execute FOGSA. Below the command line for installing those are shown:
```{r, eval = FALSE}
install.packages("shiny")
install.packages("shinydashboard")
install.packages("dplyr")
install.packages("plotly")
install.packages("crosstalk")
install.packages("tidyverse")
install.packages("viridis")
```

### Input files
Once FOGSA is ran, the first step is uploading the output file for simulations. It should consist in two columns, one with the Likelihood ratios obtained under the one hypothesis of pedigree and the other with those obtained under another hypothesis to be compared. 

### Power plot
For a given pedigree the statistical power of the DNA-based identification could be calculated from the simulated data. Two measures could help to characterize a family group:
Inclution Power (IP): It is the probability of obtaining a likelihood ratio of zero when the tested person (POI) is not related with the family group.
Exclution Power (EP): The probability of obtaining a likelihood ratio upper than 10000 when the tested person (POI) is actually the missing person (MP).
The Power Plot could be perfomed combining this two measures. A family group with low PI and PE has low statistical power where a high probability of false positives and false negatives could be expected. In this cases more genetic information must be incorporated to the pedigree. It could be done by the incorporation of more genetic markers or more members. As couterpart, a pedigree with high IP and EP allows the identification with an accurate result.

### Rates calculation in function of LR threshold selected
False positive and false negative rates could be estimated for each Likelihood ratio selected as threshold (please select one using the sliderbar). Taking into account this measures we can estimate Matthews Correlation Coefficient and Acuracy. Both measures gives us an idea about how accurate is the LR threshold selected. If you are working with massive comparisons in genetic databases the expected number of false positives could be obtained multiplying the false positive rate by the database size.

### Likelihood Ratio Decision Threshold
In massive comparisons carried on in missing person identification, the matches (LR values upper the LR threshold) are re-analyzed by geneticists and more genetic (Y-Chromosome, X-Chromosome and mitochondrial DNA) and non-genetic (date of birth) data is incorporated to the case in order to avoid false positives. In contrast, false negatives are not detected (low LR values) so they don't allow the re-analysis. With this in mind, calculations of true and false negatives rates with different numbers of LR thresholds (from 1 to 10000) for each family group could be taken into consideration in order to select an appropiate LR threshold. This value could be used for the establishment of a LR threshold (named LR decision treshold or LRdt) with the aim of selecting those matches that need a re-evaluation with more genetic and non genetic data in order to minimize the probability of obtaining a false negative result.
The LR decision threshold should be selected taking into consideration:
- False negative rate minimization
- Database size and resources for the re-analysis of the expected number of false positive (false positive rate * database size).
- The behavior of the LRdt plot. It indicates that in some cases increasing the LRdt value results in a high reduction of false positive rate whilst the false negative rate has a small increase.

## Contributing
This software was developed and maintained by Franco Marsico. Any suggestion could be sending an e-mail to franco.lmarsico@gmail.com

## License 
GNU General Public Licence
