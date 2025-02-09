# ----------------------------------------
# Master Script: Run All Steps
# ----------------------------------------
# This script runs all major stages of the data pipeline:
# 1. Data Processing
# 2. Data Analysis
# 3. Data Modelling
# ----------------------------------------

# ----------------------------------------
# Run Data Processing Scripts
# ----------------------------------------

source("src/01_process_data.R")

# ----------------------------------------
# Run Analysis Scripts
# ----------------------------------------

source("src/02_analyse_data.R")

# ----------------------------------------
# Run Modeling Scripts
# ----------------------------------------

source("src/03_model_data.R")

# ----------------------------------------
# Completion Message
# ----------------------------------------

print("All scripts executed successfully!")
