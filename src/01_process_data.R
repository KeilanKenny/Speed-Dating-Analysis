# ----------------------------------------
# Process Data
# ----------------------------------------
# This script processes raw data:
# 1. Loads data
# 2. Cleans the data
# ----------------------------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)
require(forcats)
library(ROCR)

# ----------------------------------------
# Step 1: Load Data
# ----------------------------------------

# Read in data
data <- read.csv('data/speed_dating_data.csv')

# ----------------------------------------
# Step 2: Clean Data
# ----------------------------------------

# Information about the data
str(data)
summary(data)

# Number of NA values per column.
colSums(is.na(data))

