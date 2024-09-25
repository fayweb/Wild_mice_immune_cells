# ***********************************************************
# Title: Immune cells in wild / laboratory mice

# Purpose: This script defines all the settings and executes
#         all the code (.R, .md) to reproduce the analysis
#         of the project
#
# Authors: Fay Webster
# ID variables:
# ***********************************************************
# Part 1: Set standard settings & install packages            ----
# ***********************************************************
# Install packages/load libraries to keep R environment stable
# install
# pacman for simplified bulk pkg import
# renv for pkg consistency over time
#install.packages("pacman")
# increase maximum overlaps
options(ggrepel.max.overlaps = Inf)

library(pacman)
## Standard settings ----
# seed
set.seed(13102023)

# Use p_load to install (if not already) and load the packages
pacman::p_load(mice, stringr, gridExtra, dplyr, tidyverse, tidyr, janitor, 
               visdat, corrplot, RColorBrewer, ggplot2, VIM, limma, 
               latticeExtra, patchwork,FactoMineR, ggrepel, factoextra, 
               reshape2, sjPlot, stargazer, jtools,modelsummary, ggeffects, 
               pheatmap, ggpubr, ggridges, gt, caret, randomForest, rfUtilities,
               parasiteLoad, fitdistrplus, optimx, leaflet, magick, ggdist,
               ggbeeswarm, ggtext, kableExtra, webshot, broom)


# data
# building dynamic paths
# Get the user's profile directory on Windows
user_profile <- Sys.getenv("USERPROFILE")

# Append the specific path
one_drive <- file.path(user_profile, "OneDrive",
                       "Documents", "GitHub", "Wild_mice_immune_cells")


# ***********************************************************
# Part 2: Run Data cleaning                         ----
# ***********************************************************
# ***********************************************************
# Part 2: Run Data cleaning - lab                        ----
# ***********************************************************
#----------------------------------------------------------*
# 2.1: Import raw data & save as intermediate/processed
#----------------------------------------------------------*
if (1) source(file.path(clab, "lab_import.R"))
#----------------------------------------------------------*
# 2.2: Conduct cleaning (formatting) 
# Creates: Challenge
#----------------------------------------------------------*
if (1) source(file.path(clab, "lab_clean.R"))
#----------------------------------------------------------*
# 2.3: Visualize data
#----------------------------------------------------------*
if (1) source(file.path(clab, "lab_visualize.R"))
# Creates: Correlation matrix between laboratory gene expression values
# ***********************************************************
# Part 3: Run field infection data cleaning                      ----
# **********************************************************
#----------------------------------------------------------*
# 3.1: Import raw data & save as intermediate/processed
# Requires:
# Creates: field_cleaned_data.csv
#----------------------------------------------------------*
if (1) source(file.path(cfield, "field_import.R"))
#----------------------------------------------------------*
# 3.2: Conduct cleaning (formatting) w/o changing data
#----------------------------------------------------------*
if (1) source(file.path(cfield, "field_clean.R"))
#----------------------------------------------------------*
# 3.3: Visualize data
#----------------------------------------------------------*
if (1) source(file.path(cfield, "field_visualize.R"))
#----------------------------------------------------------*
# 3.4: Import amplicon infection intensities and join with field
#----------------------------------------------------------*
if (1) source(file.path(cfield, "field_import_amplicon_intensities.R"))


# ***********************************************************
# Part 4:  MNI: Merge normalize impute                           ----
# ***********************************************************
#----------------------------------------------------------*
# 4.1 Merging of field and laboratory data
# Creates output: merge_prior_imputation.csv
#----------------------------------------------------------*
if (1) source(file.path(nmi, "nmi_merge_long.R"))
# 4.2 Create a dataframe with the selection of the mice in the experiments
# Creates output: mice_selection.csv
#----------------------------------------------------------*
if (1) source(file.path(nmi, "nmi_merge_wide.R"))
# 4.3 Normalization of data 
# Creates output: genes
#----------------------------------------------------------*
if (1) source(file.path(nmi, "nmi_normalize.R"))
# 4.4 Imputation of missing data 
# Creates output: genes
#----------------------------------------------------------*
if (1) source(file.path(nmi, "nmi_impute.R"))
# 4.4 Imputation of missing data 
# Creates output: hm
#----------------------------------------------------------*
if (1) source(file.path(nmi, "Clean_after_impute.R"))
# 4.5 Cleaning after imputing  
# Correct the parasite labels for legend
# Add custom colours for parasites throughout the scripts
# Creates factor levels for parasite strains
#----------------------------------------------------------*


# ***********************************************************
# Part 5: Experimental design                           ----
# ***********************************************************
#----------------------------------------------------------*
# Show the primary results of our experimental design
# How many rodents, distributions, strains, and parasite information
if (0) source(file.path(cdesign, "design_experimental.R"))


# ***********************************************************
# Part 6: Analysis                           ----
# ***********************************************************
#----------------------------------------------------------*
# 6.1: PCA 
# performing a pca analysis on the laboratory immune gene data
# Requires: hm
# Creates: lab
# Plots: biplot, pca_variables,
#  contr_PC1, contr_PC2
#----------------------------------------------------------*
if (0) source(file.path(canalysis, "analysis_PCA_genes_lab.R"))
# 6.2: PCA 
# Regressions with pc axes 
# Plots: pc1_current_infection, pc2_current_infection, coefs5
#----------------------------------------------------------*
if (0) source(file.path(canalysis, "analysis_linear_regressions_PCA.R"))
#----------------------------------------------------------*
# 6.2: Heatmap lab genes
# Requires: previous script!!!
#----------------------------------------------------------*
if (0) source(file.path(canalysis, "heatmap_lab_genes.R"))
#----------------------------------------------------------*
# 6.3: Multiple multivariate regression of genes vs weight loss in the lab
# Requires: hm, lab
#----------------------------------------------------------*
if (0) source(file.path(canalysis, "analysis_multiple_multivariate_regression.R"))
#----------------------------------------------------------*
# 6.4: Multiple multivariate regression of genes vs weight loss in the field infections
# Requires: hm, field
#----------------------------------------------------------*
if (0) source(file.path(canalysis, "analysis_multiple_multivariate_regression_field.R"))


# ***********************************************************
# Part 7: Analysis                           ----
# ***********************************************************
#----------------------------------------------------------*