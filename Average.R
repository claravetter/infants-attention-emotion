##### Function: average_both_eyes #####
### Average left and right eye pupil data ###
average_both_eyes <- function(path_name_in, path_name_out, gap_value) {
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  for (i in 1:length(csv_files)) {
    df <- read.csv(csv_files[i], header = T)
    
    df$IntplDiameterPupilLeftEye <- 
      ((df$NextDiameterPupilLeftEye - df$PrevDiameterPupilLeftEye) / (df$NextLeftBaseTime - df$PrevLeftBaseTime)) *
      (df$BaseTime - df$PrevLeftBaseTime) + df$PrevDiameterPupilLeftEye
    
    df$IntplDiameterPupilRightEye <- 
      ((df$NextDiameterPupilRightEye - df$PrevDiameterPupilRightEye) / (df$NextRightBaseTime - df$PrevRightBaseTime)) *
      (df$BaseTime - df$PrevRightBaseTime) + df$PrevDiameterPupilRightEye
    
    df$IntplDiameterPupilLeftEye <- ifelse(is.na(df$DiameterPupilLeftEye) & !is.na(df$DiameterPupilRightEye), 
                                           df$IntplDiameterPupilLeftEye, 
                                           df$DiameterPupilLeftEye)
    df$IntplDiameterPupilRightEye <- ifelse(is.na(df$DiameterPupilRightEye) & !is.na(df$DiameterPupilLeftEye), 
                                            df$IntplDiameterPupilRightEye, 
                                            df$DiameterPupilRightEye)
    df$AverageDiameterPupils <- apply(df[c('IntplDiameterPupilLeftEye', 'IntplDiameterPupilRightEye')],
                                      1, mean, na.rm = TRUE)
    
    new_filename <- paste(path_name_out, paste0(basename(csv_files[i])), sep="/")
    write.csv(df, new_filename)
  }
}
