## Purpose

This app is designed to allow data exploration and model building on a [Data Scientist Salaries dataset](https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries).

## Packages

-   `shinydashboard`: App framework
-   `shinyWidgets`: Progress bar
-   `tidyverse`: Many useful functions
-   `datamods`: For filtering a data frame according to column's type
-   `skimr`: For quick summary statistics
-   `magrittr`: %<>% operator

### Install Code

```
install.packages(c("shinydashboard", "tidyverse", "shinyWidgets", "datamods", "skimr"))
```

## To run ShinyDataApp

```
shiny::runGitHub()
```
