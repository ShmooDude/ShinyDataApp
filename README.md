## Purpose

This app is designed to allow data exploration and model building on a [Near Earth Objects Dataset](https://www.kaggle.com/datasets/sameepvani/nasa-nearest-earth-objects).

## Packages

-   `datamods`: For filtering a data frame according to column's type
-   `shinydashboard`: App framework for nicer tabs/sidepanels
-   `tidyverse`: Many useful functions
-   `skimr`: For quick summary statistics
-   `mathjaxr`: For math symbols.
-   `caret`: Model training
-   `parallel`: Parallel Processing
-   `doSNOW`: Parallel Processing
-   `rpart.plot`: Prettier plot for rpart
-   `waiter`: Automated Loading Screens
-   `randomForest`: For random forest

### Install Code

```
install.packages(c("datamods", "shinydashboard", "tidyverse", "skimr", "mathjaxr", "caret", "parallel", "doSNOW", "rpart.plot", "waiter", "randomForest"))
```

## To run ShinyDataApp

```
shiny::runGitHub("ShinyDataApp", "ShmooDude")
```
