# nonet

nonet is unified solution for weighted average ensemble in supervised and unsupervised learning environment. It is a novel approach to provide weighted average ensembled predictions without using labels from outcome or response variable for weight computation. In a nutshell, nonet can be used in two scenarios: 

- This approach can be used in the unsupervised environment where outcome labels not available.
- This  approach can be used to impute the missing values in the real-time scenarios in supervised and unsupervised environment because nonet does not require training labels to compute the weights for ensemble. 

## Getting Started:
one of the best way to start with this project is, have a look at [vignettes](https://github.com/AviralVijay-GSLab/nonet/tree/master/vignettes).
Vignettes provides clear idea about how nonet can contribute to ensemble different models all together.

nonet also available on [Github Page](https://aviralvijay-gslab.github.io/nonet/)

### Installtion
This package can be downloaded from github using devtools:

- devtools::install_github("AviralVijay-GSLab/nonet") 


nonet uses below mentioned R version & packages:-

#### Requirements
- R (>= 3.5.1)

#### Used packages: 
- caret (>= 6.0.78),
- dplyr,
- randomForest,
- ggplot2,
- rlist (>= 0.4.6.1),
- glmnet,
- tidyverse,
- e1071,
- purrr,
- pROC (>= 1.13.0),
- rlang (<= 0.3.0.1),


## Contribution

nonet welcomes you to contribute and suggest the improvement.  Kindly raise the pull request for enhancement and raise the issue if you find any bugs.

for more details and support, one can reach out to us:

- Email: aviral.vijay@gslab.com
- (Alternate email): aviralvj@gmail.com
