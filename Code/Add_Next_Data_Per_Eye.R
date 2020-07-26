##### Function: add_next_data_per_eye #####
### Add the next pupil diameter to the data set ###

add_next_data_per_eye <- function(path_name_in, path_name_out) {
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
    
    for (trialid in 1:16)
    {
      next_base_time <- NA
      next_val <- NA
      max_count <-
        max(df$BaseCount[df$TrialID == trialid])
      if (max_count >= 1)
      {
        for (c in max_count:1)
        {
          df$NextLeftBaseTime[df$TrialID == trialid &
                                df$BaseCount == c] <- next_base_time
          df$NextDiameterPupilLeftEye[df$TrialID == trialid &
                                        df$BaseCount == c] <- next_val
          
          if (!is.na(next_val))
          {
            df$NextLeftSlope[df$TrialID == trialid &
                               df$BaseCount == c] <-
              1000 * (df$DiameterPupilLeftEye[df$TrialID == trialid &
                                                df$BaseCount == c] - next_val) /
              (df$BaseTime[df$TrialID == trialid &
                             df$BaseCount == c] - next_base_time)
          }
          
          val <-
            df$DiameterPupilLeftEye[df$TrialID == trialid &
                                      df$BaseCount == c]
          base_time <-
            df$BaseTime[df$TrialID == trialid & df$BaseCount == c]
          if (!is.na(val))
          {
            next_val <- val
            next_base_time <- base_time
          }
        }
      }
    }
    
    for (trialid in 1:16)
    {
      next_base_time <- NA
      next_val <- NA
      max_count <-
        max(df$BaseCount[df$TrialID == trialid])
      if (max_count >= 1)
      {
        for (c in max_count:1)
        {
          df$NextRightBaseTime[df$TrialID == trialid &
                                 df$BaseCount == c] <- next_base_time
          df$NextDiameterPupilRightEye[df$TrialID == trialid &
                                         df$BaseCount == c] <-
            next_val
          
          if (!is.na(next_val))
          {
            df$NextRightSlope[df$TrialID == trialid &
                                df$BaseCount == c] <-
              1000 * (df$DiameterPupilRightEye[df$TrialID == trialid &
                                                 df$BaseCount == c] - next_val) /
              (df$BaseTime[df$TrialID == trialid &
                             df$BaseCount == c] - next_base_time)
          }
          
          val <-
            df$DiameterPupilRightEye[df$TrialID == trialid &
                                       df$BaseCount == c]
          base_time <-
            df$BaseTime[df$TrialID == trialid & df$BaseCount == c]
          if (!is.na(val))
          {
            next_val <- val
            next_base_time <- base_time
          }
        }
      }
    }
    
    df$MaxLeftSlope <- pmin(abs(df$PrevLeftSlope), abs(df$NextLeftSlope))
    df$MaxRightSlope <- pmin(abs(df$PrevRightSlope), abs(df$NextRightSlope))
    
    new_filename <-
      paste(path_name_out, paste0(basename(csv_files[i])), sep = "/")
    write.csv(df, new_filename)
  }
}
