##################
# Oldies But Goodies Final Project
# Using TensorFlow to build a package that performs simple shape image identification
##################

# Loading in Keras that can load in TensorFlow
install.packages("devtools", lib = "~/R_lib/")
library(devtools)
devtools::install_github("rstudio/keras", lib = "~/R_lib/")
library(keras)
install_keras()

# Loading in Fashion MNIST dataset
fashion_mnist <- dataset_fashion_mnist()


# Pre-processing data for SimpShape

# Loading and installing required packages for preprocessing

install.packages(c("rjson", "magick", "purrr", "tibble", "tidyr", "dplyr", "ggplot2", "stringr"))
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

annotations <- fromJSON(file = annot_file)
str(annotations, max.level = 1)