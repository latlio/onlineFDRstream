# onlineFDR Stream

## Introduction
Welcome to the onlineFDR Stream Shiny app. This repository contains an application that is an interface to the onlineFDR package hosted on Bioconductor. It is designed to assist researchers in controlling the False Discovery Rate (FDR) in an online manner. The app provides a family of algorithms that users can use, a tool to help decide which one to use, and several features including plotting the adjusted significance thresholds against a Bonferroni correction. Users are also able to download the results. If you do want to run a post-hoc analysis, please use the [Explore](https://mrc-bsu.shinyapps.io/onlineFDRShiny/) version as the [Stream](https://mrc-bsu.shinyapps.io/onlineFDRStream/) version is designed for sequential usage. 

## How to Use

Because the stream app is designed to be used as your dataset grows, you should have a CSV file of your parameters such that you can reuse them any time you want to run your algorithm of choice. More details about how to make your parameter CSV file can be found within the "How to upload your parameters" heading within the app.

![](https://github.com/latlio/onlineFDRstream/blob/master/www/user-diagram.png)

## Background behind algorithms
For more information, please visit our [vignette](https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html)!
