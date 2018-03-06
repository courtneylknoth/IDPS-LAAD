---
title: "Intro and Instructions"
author: "Capt Spencer Butt"
date: "February 21, 2018"

---

## Introduction

This shiny gadget is designed to construct an Autoencoder Neural Network (ANN) to detect anomalous data observations within a dataset. This analytic will test multiple ANN hyperparameters to determine the optimal settings supporting anomaly detection.

## Current Limitations

* Data Preparation
    + Load Data
        + No capability to load datasets for analysis
        + The base `r` `Fisher Iris` dataset is hard-coded for autoencoder analysis 
    + Select Features
        + No capability to select features from base `r` `Fisher Iris` dataset
        + The all 5-features within the dataset are used for autoencoder anomaly detection
    + Train/Test Split Data
        + Base `r` `Fisher Iris` dataset is automatically split into 3-subsets, 
        + Subsets are considered neural network hyperparameters
        + Each of the 3-subsets are automatically split into _training_ and _test_ datasets
    + Scale Data
        + Each subset is scaled into the three intervals 
            + [0,1]
            + [-0.5, 0.5]
            + [-1, 1]    
* Hyperparameter DOE
    + Load Custom Designed Experiment
        + No capability to load external designed experiment for analysis
    + Build Custom Designed Experiment
        + No capability to build experimental design withing gadget
    + Default Experimental Design
        + A 600-design point hyperparameter designed experiment is provided
* Test Hyperparameters
    + Two versions of the 600-design point hyperparameter designed experiment are executable
        + Run Short Default Experimental Design
            + The subset hyperparameter is removed from the test design
            + 200 design points
        + Run Full Default Experimental Design
            + The full 600-design point default experimental design is run 
* DOE Results
  + No limitations
* Identify Outliers
  + No limitations
  
## Instructions

1. Load the dataset for analysis 
    + Limited to base `r` `Fisher Iris` dataset use only
2. Select the appropriate features from the loaded dataset
    + All 5 base `r` `Fisher Iris` dataset features are utilized
    + The `Species` feature has been one-hot encoded to new 3-features 
3. Split into training and test datasets
    + Base `r` `Fisher Iris` is automatically split into three subsets and subsequently split into _test_ and _training_ datasets
4. Scale all subsets
    + Each _test_ and _training_ subset is automatically scaled into three intervals
5. Select hyperparameter experimental design
    + Only the default experimental design is available for hyperparameter testing
6. Test selected experimental design
    + Select either the short or full experimental design
        + Short experimental design: 200 test trials
        + Full experimental design: 600 test trials
7. Click `Run Autoencoder Designed Experiment`
    + Long run times are to be expected
8. Results of the designed experiment are displayed on the `DOE Results` tab
9. Graphical and a table of the top outliers are displayed in the `Identify Outliers` tab


