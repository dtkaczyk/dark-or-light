Color Luminance Analysis
================
Dominika Tkaczyk
October 24, 2016

Introduction
============

In this document we analyze colors with respect to whether they are dark or light. From now on, we will call this feature luminance (<https://en.wikipedia.org/wiki/Luma_%28video%29>). The analysis deals with the following main problem: how can we automatically determine whether the color is dark or light?

The issue is not trivial for a couple of reasons:

-   a color is not a single number, for which we could use a single threshold, but rather a combination of a few components (for example red, green and blue light in case of RGB model), each of which may affect the luminance differently,
-   color's luminance is subjective and may differ depending on who is judging it,
-   even though there are colors for which the correct answer is obvious (eg. white or black), there are also a lot of colors in case of which the answer might be ambiguous (eg. pure green).

Problem statement
-----------------

We will represent colors using RGB model. In this model a color \(c\) is represented by a vector of three numbers \(c = [c_R, c_G, c_B]\), where the red, green and blue intensities are normalized, that is \(0 \leq c_R, c_G, c_B \leq 1\). In this model, white is represented by a vector \(white = [1, 1, 1]\), black by \(black = [0, 0, 0]\), blue will be \(blue = [0, 0, 1]\), and so on.

Color's luminance will be represented by a single categorical variable with two levels: *dark* and *light*.

The problem analyzed in this document is the following: find a function \(L: [0,1]^3 \rightarrow \{d,l\}\), which for a given color determines its luminance.

Data
====

A dedicated dataset was prepared manualy for the purpose of this analysis. First, 300 colors were generated by randomly sampling from uniform distribution for each RGB components separately. Then the colors were labelled manually as *dark* or *light*. For manual labelling we used scatter plots of green intensity against red intensity with points coloured by the "real" color of the data point and labelled with its number from the data set:

![](colorAnalysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

Exploratory analysis
====================

Before exploring our dataset, let's first divide it into train and test sets. All the analyses, plotting and exploring will be done using only train part. In the end, the test set will be used to give a fair, reliable estimate of the prediction's error.

``` r
data <- read.csv("data.csv")
set.seed(2101)
split <- 0.67
trainIndex <- createDataPartition(data$Lum, p = split, list = FALSE)
train <- data[trainIndex,]
row.names(train) <- NULL
test <- data[-trainIndex,]
row.names(test) <- NULL
```

Our train set contains 202 observation with 4 attributes describing each of them. Let's first see how the first few observation look like:

``` r
head(train, n = 10)
```

    ##          Red     Green       Blue Lum
    ## 1  0.7027588 0.5640857 0.94252398   L
    ## 2  0.4264704 0.1829865 0.99107870   L
    ## 3  0.4966223 0.5939583 0.65299197   L
    ## 4  0.5033762 0.4312600 0.02559950   L
    ## 5  0.2373066 0.7829079 0.56263702   L
    ## 6  0.2169887 0.6081824 0.64444006   L
    ## 7  0.7839110 0.8241925 0.06574628   L
    ## 8  0.1950080 0.9158516 0.69571098   L
    ## 9  0.8520693 0.7607886 0.40480641   L
    ## 10 0.1628224 0.3646892 0.32138643   D

**Red**, **Green** and **Blue** attributes are numeric, while **Lum** is a categorical variable with two levels: *L* and *D*:

``` r
str(train)
```

    ## 'data.frame':    202 obs. of  4 variables:
    ##  $ Red  : num  0.703 0.426 0.497 0.503 0.237 ...
    ##  $ Green: num  0.564 0.183 0.594 0.431 0.783 ...
    ##  $ Blue : num  0.9425 0.9911 0.653 0.0256 0.5626 ...
    ##  $ Lum  : Factor w/ 2 levels "D","L": 2 2 2 2 2 2 2 2 2 1 ...

Let's now plot the histograms of all the variables:

![](colorAnalysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

The distributions of red, green and blue components seem fairly uniform (this is to be expected, based on how the data set was generated). The dark and light classes are also well balanced, we have similar number of observations in each class.

Let's also examine the boxplots of each variable divided by the luminance:

![](colorAnalysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

It can be clearly seen that color components affect the luminance differently:

-   green seems to keep the most information about the luminance; the means of green components for dark and light colors differ greatly, and the IQRs of the distributions do not overlap,
-   red seem to be less correlated with luminance; the means are still far apart, but the IQRs are closer to each other,
-   blue separated the dark and light colors the worst, the two distributions are pretty similar.

We can also perform statistical tests to confirm these observations more formally. We will use non-parametric Kolmogorov–Smirnov test to compare two samples: red/green/blue values for dark and light colors. The p-value will indicate how probable it is that the samples come from the same distibution.

``` r
ks.test(train[train$Lum=='D',]$Red, train[train$Lum=='L',]$Red)
```

    ## 
    ##  Two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  train[train$Lum == "D", ]$Red and train[train$Lum == "L", ]$Red
    ## D = 0.32547, p-value = 4.633e-05
    ## alternative hypothesis: two-sided

``` r
ks.test(train[train$Lum=='D',]$Green, train[train$Lum=='L',]$Green)
```

    ## 
    ##  Two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  train[train$Lum == "D", ]$Green and train[train$Lum == "L", ]$Green
    ## D = 0.60751, p-value < 2.2e-16
    ## alternative hypothesis: two-sided

``` r
ks.test(train[train$Lum=='D',]$Blue, train[train$Lum=='L',]$Blue)
```

    ## 
    ##  Two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  train[train$Lum == "D", ]$Blue and train[train$Lum == "L", ]$Blue
    ## D = 0.13522, p-value = 0.3157
    ## alternative hypothesis: two-sided

As observed before, in the case of *Blue* we obtained a large p-value, suggesting that the samples are not that different. *Red* and *Green*, however, resulted in a very small p-values, so in these cases the distributions indeed vary a lot.

Let's also examine the correlations between the variables:

``` r
cor(train[,1:3])
```

    ##               Red       Green        Blue
    ## Red    1.00000000 -0.03045707  0.02304655
    ## Green -0.03045707  1.00000000 -0.13269322
    ## Blue   0.02304655 -0.13269322  1.00000000

There seem to be no correlations between *Red*, *Green* and *Blue* componenets in our data set. Again, this is to be expected considering how the data set was built.

Let's also plot variables against each other and color the points with luminance:

``` r
pairs(train, col=train$Lum)
```

![](colorAnalysis_files/figure-markdown_github/unnamed-chunk-10-1.png)

The plot of *Red* vs. *Green* shows a fairly clear linear separation between the luminance classes. Similar trend can be observed in the *Green* vs. *Blue* plot, but somewhat weaker: more points of different classes lie close together in the middle area. The plot *Red* vs. *Blue* shows no pattern.

The initial analysis results in the following conclusions:

-   green components seem to be the most informative of the luminance, and blue - the least,
-   linear methods might be a good approach to the prediction problem, as the plots show the possibility of a linear separation between dark and light classes.

Evaluation
==========

Prediction models
=================

Conclusions
===========