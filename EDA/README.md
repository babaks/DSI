# Data Exploration and Visualization using R and ggplot2

# Description
This course is intended to help people with little to no experience using R take their first steps on a data analysis. We provide an overview of introductory R programming, then focus on using R and ggplot2 for data exploration and visualization.  Please note that this workshop's focus is on getting you the tools you need to work with your data in R and visualize it; we will not be covering any statistical methods/tests.

Please follow the [Pre-Workshop Instructions](#Instructions) prior to coming to the workshop to maximize our time.


# Syllabus
## Session 1: 10am-noon
[**Part 1: R and RStudio basics:**](https://akseong.github.io/IntroEDA_Rggplot2/slides/Session_1_Rbasics.html) (html slides)
- data types, subsetting, reading/writing data
- constrol structures, functions

[**Part 2: Basic data exploration and plotting in base R:**](https://akseong.github.io/IntroEDA_Rggplot2/slides/Session_1_introEDA.html) (html slides)
- summaries, tables
- plotting different types of data


## Break: noon-1pm
[**Exercise 1**](https://akseong.github.io/IntroEDA_Rggplot2/exercises/Session_1_Rbasics---Exercises.html) (html doc; to be completed during the break)


## Session 2: 1pm-3pm
[**Part 1: Introduction to ggplot2 and the "grammar of graphics"**](https://akseong.github.io/IntroEDA_Rggplot2/slides/Session_2_Part1_ggplot2_intro.html) (html slides)
- understanding the logic and basic syntax of ggplot2
- overlaying geoms, visualizing strata for comparison

[**Part 2: ggplot2: beyond basics with an example**](https://akseong.github.io/IntroEDA_Rggplot2/slides/Session_2_Part2_ggplot2_beyondbasics.html) (html slides)
- data manipulation and preparation
- changing default options


## Break: 3pm-4pm
[**Exercise 2**](https://akseong.github.io/IntroEDA_Rggplot2/exercises/Session_2_EDA---Exercises.html) (html doc; to be completed during the break)

## Session3: 4pm-5pm
[**Worked example: making a choropleth map in ggplot2**](https://akseong.github.io/IntroEDA_Rggplot2/exercises/Session_3_choropleth_map.R) (download .R script)



# <a name="Instructions"></a>Pre-Workshop Instructions
### Step 1: Download and install R
First, visit [The R Project for Statistical Computing](https://www.r-project.org/). Click on `CRAN` under the Download section on the left-hand side of the page. Then, click on any of the nearby websites under the USA section near the bottom of the page.  Download R for your platform (Linux, Mac, or Windows), open the downloaded file and follow the instructions.

### Step 2: Download and install RStudio
RStudio is a set of integrated tools designed to help you be more productive with R. Also, it is far more user-friendly than base R. You will be doing essentially all of your programming in RStudio. To download RStudio, visit the [download page](https://www.rstudio.com/products/rstudio/download/), and install RStudio Desktop (free version).

### Step 3: Install required R packages
In R, packages are used to share code. A package bundles together code, data, documentation, and tests. As of May 2020, there were >15,000 packages available on the Comprehensive R Archive Network, or CRAN. This huge variety of packages is one of the reasons that R is so successful: the chances are that someone has already solved a problem that you’re working on, and you can benefit from their work by downloading their package and using their code.

In this workshop, we will be using a number of packages-- `ggplot2`, `dplyr`, `maps`. I suggest installing them before our sessions begin: open up RStudio and try to run the following lines of code.

```r
install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("maps", dependencies = TRUE)
```
Additional "dependencies," or other packages necessary to run the three above, will also be installed. This make take a few minutes.

### Step 4: Download the data
We will be working with a few datasets throughout the day.  Downloading these datasets beforehand may make things a bit easier / faster for you.  You can "save link as" for the links below, or look in the `data` folder of this repository.  Make a folder for this class, then make a sub-folder in that named "data", and save these in there.  
- [nytimes data](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv)
- [election data](https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv)
- [census data](https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv)

