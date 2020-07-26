##### Function: remove_slope_outliers_per_eye #####
### Remove too quick slope changes ###

remove_slope_outliers_per_eye <- function(path_name_in, path_name_out) {
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  for (i in 1:length(csv_files))
  {
    df <- read.csv(csv_files[i], header = T)
    
    left_mad <- mad(df$MaxLeftSlope, na.rm = T)
    left_mad <- ifelse(left_mad < 0.25, 0.25, left_mad)
    right_mad <- mad(df$MaxRightSlope, na.rm = T)
    right_mad <- ifelse(right_mad < 0.25, 0.25, right_mad)
    outlier_factor <- 8
    
    df$DiameterPupilLeftEye <- ifelse(df$MaxLeftSlope>outlier_factor*left_mad, NA, df$DiameterPupilLeftEye)
    df$DiameterPupilRightEye <- ifelse(df$MaxRightSlope>outlier_factor*left_mad, NA, df$DiameterPupilRightEye)
    
    new_filename <-
      paste(path_name_out, paste0(basename(csv_files[i])), sep = "/")
    write.csv(df, new_filename)
  }
}
