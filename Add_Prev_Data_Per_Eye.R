##### Function: add_prev_data_per_eye #####
### Add the previous pupil diameter to the data set ###

add_prev_data_per_eye <- function(path_name_in, path_name_out) {
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
      prev_base_time <- NA
      prev_val <- NA
      max_count <-
        max(df$BaseCount[df$TrialID == trialid])
      if (max_count  >= 1)
      {
        for (c in 1:max_count)
        {
          df$PrevLeftBaseTime[df$TrialID == trialid &
                                df$BaseCount == c] <- prev_base_time
          df$PrevDiameterPupilLeftEye[df$TrialID == trialid &
                                        df$BaseCount == c] <- prev_val
          
          if (!is.na(prev_val))
          {
            df$PrevLeftSlope[df$TrialID == trialid &
                               df$BaseCount == c] <-
              1000 * (df$DiameterPupilLeftEye[df$TrialID == trialid &
                                                df$BaseCount == c] - prev_val) /
              (df$BaseTime[df$TrialID == trialid &
                             df$BaseCount == c] - prev_base_time)
          }
          
          val <-
            df$DiameterPupilLeftEye[df$TrialID == trialid &
                                      df$BaseCount == c]
          base_time <-
            df$BaseTime[df$TrialID == trialid & df$BaseCount == c]
          if (!is.na(val))
          {
            prev_val <- val
            prev_base_time <- base_time
          }
        }
      }
    }
    
    for (trialid in 1:16)
    {
      prev_base_time <- NA
      prev_val <- NA
      max_count <-
        max(df$BaseCount[df$TrialID == trialid])
      if (max_count >= 1)
      {
        for (c in 1:max_count)
        {
          df$PrevRightBaseTime[df$TrialID == trialid &
                                 df$BaseCount == c] <- prev_base_time
          df$PrevDiameterPupilRightEye[df$TrialID == trialid &
                                         df$BaseCount == c] <-
            prev_val
          
          if (!is.na(prev_val))
          {
            df$PrevRightSlope[df$TrialID == trialid &
                                df$BaseCount == c] <-
              1000 * (df$DiameterPupilRightEye[df$TrialID == trialid &
                                                 df$BaseCount == c] - prev_val) /
              (df$BaseTime[df$TrialID == trialid &
                             df$BaseCount == c] - prev_base_time)
          }
          
          val <-
            df$DiameterPupilRightEye[df$TrialID == trialid &
                                       df$BaseCount == c]
          base_time <-
            df$BaseTime[df$TrialID == trialid & df$BaseCount == c]
          if (!is.na(val))
          {
            prev_val <- val
            prev_base_time <- base_time
          }
        }
      }
    }
    new_filename <-
      paste(path_name_out, paste0(basename(csv_files[i])), sep = "/")
    write.csv(df, new_filename)
  }
}

