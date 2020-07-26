##### Libraries ######
library(zoo)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(nlme)

# Source different pre-processing functions
# Step 1: Read gazedata files
source("code/Read_Gazedata.R")

# Step 2: Rename event values for consistency
source("code/Rename_Values.R")

# Step 3: Adjust time scale (start at 0) 
source("code/Base_Line_Time_Scale.R")

# Step 4: Interpolate missing values per eye
source("code/Interpolate_Per_Eye.R")

# Step 5: Remove outliers based on extreme dilation speed 
source("code/Add_Prev_Data_Per_Eye.R")
source("code/Add_Next_Data_Per_Eye.R")
source("code/Remove_Slope_Outliers_Per_Eye.R")

# Step 6: Average both eyes 
source("code/Average.R")

# Step 7: Aggregate to 50ms 
source("code/Aggregate.R")

# Step 8: Shift, drop, and base line



######################################################################
## Functions to plot and analyze data after each preprocessing step
######################################################################

# Analyze data files in a directory
source("code/Analyze_Data_Quality_Dir.R")

# Basic trial analysis
source("code/Analyze_Data_Quality_Trial.R")

# Outlier trial analysis
source("code/Analyze_Data_Quality_Trial_Outlier.R")

# Average trial analysis
source("code/Analyze_Data_Quality_Trial_Average.R")

# Average only trial analysis
source("code/Analyze_Data_Quality_Trial_Average_Only.R")

# Average only trial analysis without event lines
source("code/Analyze_Data_Quality_Trial_Average_Only_No_Event.R")

# Average only2 trial analysis
source("code/Analyze_Data_Quality_Trial_Average_Only2.R")

# Average only2 trial analysis
source("code/Analyze_Data_Quality_Trial_Average_Only_Emotion.R")

##############################################################
# Execute data preprocessing
##############################################################

##############################################################
# 1st: read raw gazedata files
##############################################################

dir <- "Babylab_files/Raw"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
read_gazedata_files('Data/Gazedata')

##############################################################
# 2nd: rename data label names for consistency
##############################################################

dir <- "Babylab_files/Rename"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
rename_events("Babylab_files/Raw", "Babylab_files/Rename")

##############################################################
# 3rd: adjust time scale
##############################################################

dir <- "Babylab_files/Timescale"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
adjust_time_scale("Babylab_files/Rename", "Babylab_files/Timescale")

##############################################################
# 4th: Interpolate per eye
##############################################################

dir <- "Babylab_files/Interpolate"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
interpolate_per_eye("Babylab_files/Timescale", "Babylab_files/Interpolate", 60)

##############################################################
# Analysis: Interpolate 60
##############################################################

analyze_data_quality_dir("Babylab_files/Interpolate", "Interpolate 60", overall_plot = T)

##############################################################
# 5th: Identify slope outliers
##############################################################

dir <- "Babylab_files/Slopeoutliers1"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
add_prev_data_per_eye("Babylab_files/Interpolate", "Babylab_files/Slopeoutliers1")

dir <- "Babylab_files/Slopeoutliers2"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
add_next_data_per_eye("Babylab_files/Slopeoutliers1", "Babylab_files/Slopeoutliers2")

##############################################################
# Analysis: Outliers
##############################################################

analyze_data_quality_dir("Babylab_files/Slopeoutliers2", "Outliers - identified", overall_plot = T, outliers = T)
analyze_data_quality_dir("Babylab_files/Slopeoutliers2", "Outliers (single Trial) - identified", overall_plot = T, outliers = T,
                         height = 12.5, single = T)
analyze_data_quality_dir("Babylab_files/Slopeoutliers2", "Outliers example - identified", overall_plot = T, outliers = T, 
                         from = 8200, to = 8300, height = 12.5, single = T)

##############################################################
# 6th: Remove slope outliers
##############################################################

dir <- "Babylab_files/Slopeoutliers3"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
remove_slope_outliers_per_eye("Babylab_files/Slopeoutliers2", "Babylab_files/Slopeoutliers3")

##############################################################
# Analysis: Remove slope outliers
##############################################################

analyze_data_quality_dir("Babylab_files/Slopeoutliers3", "Outliers - removed", overall_plot = T, outliers = T)
analyze_data_quality_dir("Babylab_files/Slopeoutliers3", "Outliers example - removed", overall_plot = T, outliers = T, 
                         from = 8200, to = 8300, height = 12.5, single = T)
analyze_data_quality_dir("Babylab_files/Slopeoutliers3", "Outliers (Single Trial) - removed", overall_plot = T, outliers = T,
                         height = 12.5, single = T)

##############################################################
# 7th: Interpolate per eye
##############################################################

dir <- "Babylab_files/Interpolate2"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
interpolate_per_eye("Babylab_files/Slopeoutliers3", "Babylab_files/Interpolate2", 60)

##############################################################
# Analysis: "Outliers 2nd Interpolation step
##############################################################

analyze_data_quality_dir("Babylab_files/Interpolate2", "Outliers - removed and interpolated", overall_plot = T, outliers = T)
analyze_data_quality_dir("Babylab_files/Interpolate2", "Outliers example - removed and interpolated", overall_plot = T, outliers = T, 
                         from = 8200, to = 8300, height = 12.5, single = T)
analyze_data_quality_dir("Babylab_files/Interpolate2", "Outliers (Single Trial) - removed and interpolated", overall_plot = T, outliers = T,
                         height = 12.5, single = T)

##############################################################
# 8th: Average
##############################################################

dir <- "Babylab_files/Average"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
average_both_eyes("Babylab_files/Interpolate2", "Babylab_files/Average", 60)

##############################################################
# Analysis: Average
##############################################################

analyze_data_quality_dir("Babylab_files/Average", "Average", average_plot = T, overall_plot = T)
analyze_data_quality_dir("Babylab_files/Average", "Average example", average_plot = T, overall_plot = T,
                         from = 5550, to = 5600, height = 12.5, single = T)
analyze_data_quality_dir("Babylab_files/Average", "Average (Single Trial)", average_plot = T, overall_plot = T,
                         height = 12.5, single = T)
analyze_data_quality_dir("Babylab_files/Average", "Average only", average_plot = T, overall_plot = T, average_only = T)

##############################################################
# 9th: Aggregate 50 ms
##############################################################

dir <- "Babylab_files/Aggregate"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
aggregate.ms("Babylab_files/Average", "Babylab_files/Aggregate", 50)

##############################################################
# Analysis: Aggregate
##############################################################

analyze_data_quality_dir("Babylab_files/Aggregate", "Aggregate", average_plot = T, overall_plot = T, average_only = T)
analyze_data_quality_dir("Babylab_files/Aggregate", "Aggregate (single Trial)", average_plot = T, overall_plot = T,
                         height = 12.5, single = T, average_only = T)
analyze_data_quality_dir("Babylab_files/Aggregate", "Start of attention example", average_plot = T, overall_plot = T,
                         from = 4500, to = 6000, average_only = T, height = 12.5, single = T)

##############################################################
# 8th: Shift and baseline
##############################################################

dir <- "Babylab_files/Baseline"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
dir <- "Babylab_files/BaselineSingleModel"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
dir <- "Babylab_files/BaselineObject21Diff"
ifelse(!dir.exists(dir), dir.create(dir), FALSE)
shift_drop_baseline_events("Babylab_files/Aggregate", "Babylab_files/Baseline")

##############################################################
# Analysis: Shift and baseline
##############################################################

analyze_data_quality_dir("Babylab_files/BaselineObject21Diff", "Baseline (Object 1 and 2 Difference)", 
                         average_plot = T, overall_plot = T, 
                         average_only = T, no_event = T, emotion = T, common_legend = F)

analyze_data_quality_dir("Babylab_files/Baseline", "Baseline", average_plot = T, overall_plot = T, 
                         average_only = T, no_event = T)
analyze_data_quality_dir("Babylab_files/Baseline", "Baseline (Single Trial)", average_plot = T, overall_plot = T,
                        height = 12.5, single = T, average_only = T, no_event = T)
analyze_data_quality_dir("Babylab_files/Baseline", "Baseline example", average_plot = T, overall_plot = T,
                         from = 4500, to = 6000, average_only = T, height = 12.5, single = T, no_event = T)
analyze_data_quality_dir("Babylab_files/BaselineSingleModel", "Baseline (Single Model)", average_plot = T, overall_plot = T, 
                         from = 0, to = 1500, average_only = T, no_event = T, emotion = T, common_legend = F)

