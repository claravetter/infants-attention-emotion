##### Function: adjust_time_scale #####
### Adjust time scale (start at 0) ###
adjust_time_scale <- function(path_name_in, path_name_out) {
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  for (i in 1:length(csv_files)) {
    df <- read.csv(csv_files[i], header = T, stringsAsFactors = F)
    # the event value renaming is needed for my function:
    for (trial in 1:16)
    {
      df$BaseTime[df$TrialID == trial] <-
        df$MsTime[df$TrialID == trial] - min(df$MsTime[df$TrialID == trial])
      
    }
    
    df[, c("AverageDiameterPupils")] <- NA
    df[, c("PrevLeftBaseTime")] <- NA
    df[, c("PrevDiameterPupilLeftEye")] <- NA
    df[, c("PrevLeftSlope")] <- NA
    df[, c("PrevRightBaseTime")] <- NA
    df[, c("PrevDiameterPupilRightEye")] <- NA
    df[, c("PrevRightSlope")] <- NA
    df[, c("NextLeftBaseTime")] <- NA
    df[, c("NextDiameterPupilLeftEye")] <- NA
    df[, c("NextLeftSlope")] <- NA
    df[, c("NextRightBaseTime")] <- NA
    df[, c("NextDiameterPupilRightEye")] <- NA
    df[, c("NextRightSlope")] <- NA
    df[, c("MaxLeftSlope")] <- NA
    df[, c("MaxRightSlope")] <- NA
    df$OrigDiameterPupilLeftEye <- df$DiameterPupilLeftEye
    df$OrigDiameterPupilRightEye <- df$DiameterPupilRightEye
    
    df$BaseCount <- sequence(rle(df$TrialID)$lengths)
    
    new_filename <- paste(path_name_out, paste0(basename(csv_files[i])), sep="/")
    write.csv(df, new_filename)
  }
}