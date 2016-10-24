Color Luminance Analysis
================
Dominika Tkaczyk
October 24, 2016

Introduction
============

In this document we attempt to analyze colors with respect to whether they are dark or light. From now on, we will call it color's luminance (<https://en.wikipedia.org/wiki/Luma_%28video%29>). The main problem analyzed is the following: how can we automatically determine whether the color is dark or light.

The issue is not trivial for a couple of reasons:

-   a color is not a single number, for which we could use a single threshold, but rather a combination of a few components (for example red, green and blue light in case of RGB model), each of which may affect the luminance differently
-   color's luminance is subjective and may differ depending on who is judging it
-   even though there are colors for which the correct answer is obivous (eg. white or black), there are also a lot of colors for which it is much less obvious (eg. pure green)

Problem statement
-----------------

We will represent colors using RGB model. A color \(c\) is represented by a vector of three numbers \(c = [c_R, c_G, c_B]\), where the RGB intensities are normalized, that is \(0 \leq c_R, c_G, c_B \leq 1\).

Color's luminance will be represented by a single categorical variable with two levels: *dark* and *light*.

The problem analyzed in this document is the following: find a function \(L: [0,1]^3 \rightarrow \{d,l\}\), which for a given color determines its luminance.

    ##        R                  G                  B            L      
    ##  Min.   :0.001176   Min.   :0.007518   Min.   :0.004466   D:142  
    ##  1st Qu.:0.254549   1st Qu.:0.229155   1st Qu.:0.281304   L:158  
    ##  Median :0.500458   Median :0.476798   Median :0.533076          
    ##  Mean   :0.499175   Mean   :0.477213   Mean   :0.514634          
    ##  3rd Qu.:0.741142   3rd Qu.:0.713870   3rd Qu.:0.748852          
    ##  Max.   :0.998121   Max.   :0.999064   Max.   :0.997008
