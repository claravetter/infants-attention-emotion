##### Function: interpolate_per_eye #####
### Interpolate missing values using the zoo package ###

interpolate_per_eye <- function(path_name_in, path_name_out, gap_value) {
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  for (i in 1:length(csv_files)) {
    df <- read.csv(csv_files[i], header = T)
    
    # linear interpolation
    # I use the na.approx of the zoo package for linear interpolation
    # if the gap of missing values is not longer than 500ms (=60 counts as the sampling rate was 120Hz), values are interplated
    df$DiameterPupilLeftEye <-
      na.approx(df$DiameterPupilLeftEye,
                na.rm = FALSE,
                maxgap = gap_value)
    df$DiameterPupilRightEye <-
      na.approx(df$DiameterPupilRightEye,
                na.rm = FALSE,
                maxgap = gap_value)
    
    new_filename <- paste(path_name_out, paste0(basename(csv_files[i])), sep="/")
    write.csv(df, new_filename)
  }
}
