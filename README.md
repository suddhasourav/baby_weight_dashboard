# Baby Weight Dashboard
## General Description
This is a web app to monitor the weight of a newborn/child, and works for children from birth up to 5 years of age. See it in action here: https://ssourav.shinyapps.io/baby_weight_dashboard/

[![A screenshot of the web app, showing some of the salient feature and the plot of a baby's weight](/feature_description/feature_description.png)](https://ssourav.shinyapps.io/baby_weight_dashboard/)

## Why I made this app
There are several similar apps out there for plotting the weight of a baby, but I wanted an app that fulfills the following criteria:

* Provides parents an easy way to document weight. More parents can use spreadsheets, so I stuck to an xlsx format
* Pretty yet accurate plotting
* Responsive, dynamic design
* The ability to automatically fit the display for newborns (0 - 13 weeks) vs. childrens 0-5 years of age based on the appropriate WHO norm data

The app started as an R script, but at one point I realized that a web app might be useful to other parents as well.

## What is a percentile curve?
Say, if your child is at the 25th percentile, it means that 25% of babies from the WHO norm data had a weight that was lower than or equal to this weight. On the other hand, 75% of the babies were above this weight.

Please remember that every child is different. The percentile curves can help you notice sudden weight losses, but it should not be used to set "target weights".

**Talk to a qualified health practitioner for your health-related questions, this app is for informational purpose only!**

## How are the curves generated?
In short, from the weight point data, a smooth curve is constructed. The smoothness of this curve is chosen to balance two important but opposing needs: the curve should not be too "wiggly" and fit noise, but it should also be wiggly enough to not systematically miss data points. An established statistical method to tackle such problems is the _generalized additive model_ (GAM), and I used the `mgcv` package by Simon Wood in R to fit the curves [1]. Plotting was doen with `ggplot` and `shiny` was used to create the web app [2, 3].

## References
1. Wood, S. N. (2001). mgcv: GAMs and generalized ridge regression for R. _R news, 1_(2), 20-25. https://cran.r-project.org/web/packages/mgcv/index.html
2. Wickham, H. et al. Welcome to the tidyverse. _J. Open Source Softw. 4_, 1686 (2019).
3. Chang, W., Cheng, J., Allaire, J., Xie, Y., & McPherson, J. (2015). Package ‘shiny’. https://shiny.posit.co/

