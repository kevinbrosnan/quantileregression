#####      Set up script file for FYP work in R     #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 19/02/2014                                      #
#                                                       #
# Description:                                          #
#  A simple set up file to load data, packages and      #
#  scripts.                                             #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

setwd("C:/Users/Kevin/SkyDrive/Final Year Project/R Code and Algorithms")

#### 1) Loading data ####

  thyroid <- read.csv("Data Files/Thyroid Data.csv")
  attach(thyroid,warn.conflicts=FALSE)

#### 2) Loading required packages ####

  library(quantreg, quietly=TRUE)
  library(ggplot2, quietly=TRUE)
  library(nortest, quietly=TRUE)
  library(MASS, quietly=TRUE)
  library(quantregGrowth, quietly=TRUE)

#### 3) Loading required scripts ####

  source("Created Functions/plot.QR.R")
  source("Created Functions/test.resid.R")
  source("Created Functions/QR.R")
  source("Created Functions/box.cox.R")
  source("Created Functions/distribution.R")
  source("Created Functions/summary.QR.R")
  source("Created Functions/bspline.R")
