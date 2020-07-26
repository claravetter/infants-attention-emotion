##### Function: shift_drop_baseline_events #####
### Shift events ###
### Drop events ###
### Baseline events to first measurement###

shift_drop_baseline_events <- function(path_name_in, path_name_out){
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  min_points <- 3
  for (i in 1:length(csv_files)) {
    df <- read.csv(csv_files[i], header = T)
    df$BaseCount <- sequence(rle(df$TrialID)$lengths)
    
    for (trial in 1:16)
    {
      # ----------------------
      name <- 'Single_Model'
      # ----------------------
      emo <- as.character(unique (df$ModelEmo[df$TrialID == trial & df$Event == name]))
      
      single_model_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      single_model_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$SingleModelBaseTime[df$TrialID == trial &
                               df$Event == name &
                               !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - single_model_start
      df$SingleModelStartTime[df$TrialID == trial &
                                df$Event == name &
                                !is.na(df$AverageDiameterPupils)] <- single_model_start
      df$SingleModelEndTime[df$TrialID == trial &
                              df$Event == name &
                              !is.na(df$AverageDiameterPupils)] <- single_model_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$SingleModelBaseLine[df$TrialID == trial &
                               df$Event == name &
                               !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$SingleModelBaseLine[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)]
      
      # -----------------------------------------------------------------------
      if (nrow(df[df$TrialID == trial &
                  df$Event == name &
                  !is.na(df$AverageDiameterPupils),]) < min_points)
      {
        {
          df$AverageDiameterPupils[df$TrialID == trial &
                                     df$Event == name] <- NA
        }
      }
      
      single_model_start_count <- 
        min(df$BaseCount[df$TrialID == trial & df$Event == name &
                           !is.na(df$AverageDiameterPupils)])
      
      if (!is.na(single_model_start_count))
      {
        start_row <- which(df$BaseCount == single_model_start_count & df$TrialID == trial & df$Event == name)
        if (length(start_row) > 0)
        {
          if (is.na(df[start_row + 1, ]$AverageDiameterPupils))
          {
            message("Ignored ", name, " trial ", trial)
            df$SingleModelBaseTime[df$TrialID == trial &
                                     df$Event == name &
                                     !is.na(df$AverageDiameterPupils)] <- NA
          }
        }
        else
        {
          df$SingleModelBaseTime[df$TrialID == trial &
                                   df$Event == name] <- NA
        }
      }
      # -----------------------------------------------------------------------
      
      df1 <- subset(df, df$TrialID == trial & df$Event == name & !is.na(df$AverageDiameterPupils))
      
      if (nrow(df1) > 0)
      {
        lm_model <- lm(AverageDiameterPupils ~ BaseTime,
                       data = df1)
        df$SingleModelIntercept[df$TrialID == trial &
                                  df$Event == name &
                                  !is.na(df$AverageDiameterPupils)] <-
          coef(lm_model)["(Intercept)"]
        df$SingleModelSlope[df$TrialID == trial &
                              df$Event == name &
                              !is.na(df$AverageDiameterPupils)] <-
          coef(lm_model)["BaseTime"]
      }
      # ----------------------
      name <- 'Model'
      # ----------------------
      
      model_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      model_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$ModelBaseTime[df$TrialID == trial &
                         df$Event == name &
                         !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - model_start
      df$ModelStartTime[df$TrialID == trial &
                          df$Event == name &
                          !is.na(df$AverageDiameterPupils)] <- model_start
      df$ModelEndTime[df$TrialID == trial &
                        df$Event == name &
                        !is.na(df$AverageDiameterPupils)] <- model_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$ModelBaseLine[df$TrialID == trial &
                         df$Event == name &
                         !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$ModelBaseLine[df$TrialID == trial &
                           df$Event == name &
                           !is.na(df$AverageDiameterPupils)]
      
      # -----------------------------------------------------------------------
      if (nrow(df[df$TrialID == trial &
                  df$Event == name &
                  !is.na(df$AverageDiameterPupils),]) < min_points)
      {
        {
          df$AverageDiameterPupils[df$TrialID == trial &
                                     df$Event == name &
                                     !is.na(df$AverageDiameterPupils)] <- NA
        }
      }
      
      model_start_count <- 
        min(df$BaseCount[df$TrialID == trial & df$Event == name &
                           !is.na(df$AverageDiameterPupils)])
      
      if (!is.na(model_start_count))
      {
        start_row <- which(df$BaseCount == model_start_count & df$TrialID == trial & df$Event == name)
        if (length(start_row) > 0)
        {
          if (is.na(df[start_row + 1, ]$AverageDiameterPupils))
          {
            message("Ignored ", name, " trial ", trial)
            df$ModelBaseTime[df$TrialID == trial &
                               df$Event == name &
                               !is.na(df$AverageDiameterPupils)] <- NA
          }
        }
        else
        {
          df$ModelBaseTime[df$TrialID == trial &
                             df$Event == name] <- NA
        }
      }
      # -----------------------------------------------------------------------
      
      # ----------------------
      name <- 'Object1'
      # ----------------------
      
      object1_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      object1_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$Object1BaseTime[df$TrialID == trial &
                           df$Event == name &
                           !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - object1_start
      df$Object1StartTime[df$TrialID == trial &
                            df$Event == name &
                            !is.na(df$AverageDiameterPupils)] <- object1_start
      df$Object1EndTime[df$TrialID == trial &
                          df$Event == name &
                          !is.na(df$AverageDiameterPupils)] <- object1_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$Object1BaseLine[df$TrialID == trial &
                           df$Event == name &
                           !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$Object1BaseLine[df$TrialID == trial &
                             df$Event == name &
                             !is.na(df$AverageDiameterPupils)]
      
      # -----------------------------------------------------------------------
      if (nrow(df[df$TrialID == trial &
                  df$Event == name &
                  !is.na(df$AverageDiameterPupils),]) < min_points)
      {
        {
          df$AverageDiameterPupils[df$TrialID == trial &
                                     df$Event == name &
                                     !is.na(df$AverageDiameterPupils)] <- NA
        }
      }
      
      object1_model_start_count <- 
        min(df$BaseCount[df$TrialID == trial & df$Event == name &
                           !is.na(df$AverageDiameterPupils)])
      
      if (!is.na(object1_model_start_count))
      {
        start_row <- which(df$BaseCount == object1_model_start_count & df$TrialID == trial & df$Event == name)
        if (length(start_row) > 0)
        {
          if (is.na(df[start_row + 1, ]$AverageDiameterPupils))
          {
            message("Ignored ", name, " trial ", trial)
            df$Object1ModelBaseTime[df$TrialID == trial &
                                      df$Event == name &
                                      !is.na(df$AverageDiameterPupils)] <- NA
          }
        }
        else
        {
          df$Object1ModelBaseTime[df$TrialID == trial &
                                    df$Event == name] <- NA
        }
      }
      # -----------------------------------------------------------------------
      
      # ----------------------
      name <- 'Object2'
      # ----------------------
      
      object2_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      object2_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$Object1BaseTime[df$TrialID == trial &
                           df$Event == name &
                           !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - object2_start
      df$Object2StartTime[df$TrialID == trial &
                            df$Event == name &
                            !is.na(df$AverageDiameterPupils)] <- object2_start
      df$Object2EndTime[df$TrialID == trial &
                          df$Event == name &
                          !is.na(df$AverageDiameterPupils)] <- object2_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$Object2BaseLine[df$TrialID == trial &
                           df$Event == name &
                           !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$Object2BaseLine[df$TrialID == trial &
                             df$Event == name &
                             !is.na(df$AverageDiameterPupils)]
      
      # -----------------------------------------------------------------------
      if (nrow(df[df$TrialID == trial &
                  df$Event == name &
                  !is.na(df$AverageDiameterPupils),]) < min_points)
      {
        {
          df$AverageDiameterPupils[df$TrialID == trial &
                                     df$Event == name &
                                     !is.na(df$AverageDiameterPupils)] <- NA
        }
      }
      
      object2_model_start_count <- 
        min(df$BaseCount[df$TrialID == trial & df$Event == name &
                           !is.na(df$AverageDiameterPupils)])
      
      if (!is.na(object2_model_start_count))
      {
        start_row <- which(df$BaseCount == object2_model_start_count & df$TrialID == trial & df$Event == name)
        if (length(start_row) > 0)
        {
          if (is.na(df[start_row + 1, ]$AverageDiameterPupils))
          {
            message("Ignored ", name, " trial ", trial)
            df$Object2ModelBaseTime[df$TrialID == trial &
                                      df$Event == name &
                                      !is.na(df$AverageDiameterPupils)] <- NA
          }
        }
        else
        {
          df$Object2ModelBaseTime[df$TrialID == trial &
                                    df$Event == name] <- NA
        }
      }
      # -----------------------------------------------------------------------
      
      # ----------------------
      name <- 'Blank_Object1'
      # ----------------------
      
      blank_object1_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      blank_object1_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$BlankObject1BaseTime[df$TrialID == trial &
                                df$Event == name &
                                !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - blank_object1_start
      df$BlankObject1StartTime[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- blank_object1_start
      df$BlankObject1EndTime[df$TrialID == trial &
                               df$Event == name &
                               !is.na(df$AverageDiameterPupils)] <- blank_object1_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$BlankObject1BaseLine[df$TrialID == trial &
                                df$Event == name &
                                !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$BlankObject1BaseLine[df$TrialID == trial &
                                  df$Event == name &
                                  !is.na(df$AverageDiameterPupils)]
      
      # ----------------------
      name <- 'Blank_Object2'
      # ----------------------
      
      blank_object2_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      blank_object2_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$BlankObject1BaseTime[df$TrialID == trial &
                                df$Event == name &
                                !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - blank_object2_start
      df$BlankObject2StartTime[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- blank_object2_start
      df$BlankObject2EndTime[df$TrialID == trial &
                               df$Event == name &
                               !is.na(df$AverageDiameterPupils)] <- blank_object2_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$BlankObject2BaseLine[df$TrialID == trial &
                                df$Event == name &
                                !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$BlankObject2BaseLine[df$TrialID == trial &
                                  df$Event == name &
                                  !is.na(df$AverageDiameterPupils)]
      
      # ----------------------
      name <- 'Blank_Single_Model'
      # ----------------------
      
      blank_single_model_start <-
        min(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      blank_single_model_end <-
        max(df$BaseTime[df$TrialID == trial & df$Event == name &
                          !is.na(df$AverageDiameterPupils)])
      df$BlankSingleModelBaseTime[df$TrialID == trial &
                                    df$Event == name &
                                    !is.na(df$AverageDiameterPupils)] <-
        df$BaseTime[df$TrialID == trial &
                      df$Event == name &
                      !is.na(df$AverageDiameterPupils)] - blank_single_model_start
      df$BlankSingleModelStartTime[df$TrialID == trial &
                                     df$Event == name &
                                     !is.na(df$AverageDiameterPupils)] <- blank_single_model_start
      df$BlankSingleModelEndTime[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] <- blank_single_model_end
      start_count <- min(df$BaseCount[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils)])
      
      df$BlankSingleModelBaseLine[df$TrialID == trial &
                                    df$Event == name &
                                    !is.na(df$AverageDiameterPupils)] <-
        mean(df$AverageDiameterPupils[df$TrialID == trial &
                                        df$Event == name &
                                        !is.na(df$AverageDiameterPupils) & 
                                        df$BaseCount > start_count - 3 & 
                                        df$BaseCount < start_count + 3])
      
      df$AverageDiameterPupils[df$TrialID == trial &
                                 df$Event == name &
                                 !is.na(df$AverageDiameterPupils)] <- 
        df$AverageDiameterPupils[df$TrialID == trial &
                                   df$Event == name &
                                   !is.na(df$AverageDiameterPupils)] -
        df$BlankSingleModelBaseLine[df$TrialID == trial &
                                      df$Event == name &
                                      !is.na(df$AverageDiameterPupils)]
      
    }
    
    new_filename <- paste(path_name_out, paste0(basename(csv_files[i])), sep="/")
    write.csv(df, new_filename)
    
    df1 <- df
    df1<-df1[!is.na(df1$SingleModelBaseTime),]
    if (length(df1$SingleModelBaseTime) > 0)
    {
      df1$BaseTime <- round(df1$SingleModelBaseTime/50)*50
      for (r in 1:(nrow(df1)-1))
      {
        if (df1$BaseTime[r] == df1$BaseTime[r+1])
        {
          df1$BaseTime[r] <- NA
        }
      }
      df1<-df1[!is.na(df1$BaseTime),]
      new_filename <- paste(paste0(path_name_out, "SingleModel"), paste0(basename(csv_files[i])), sep="/")
      write.csv(df1, new_filename)
    }
    
    df1 <- df
    df1$Object21Diff <- NA
    df1<-df1[!is.na(df1$Object1BaseTime),]
    df1$BaseTime <- round(df1$Object1BaseTime/50)*50
    
    df2 <- df
    df2$Object21Diff <- NA
    df2<-df2[!is.na(df2$Object1BaseTime),]
    df2$BaseTime <- round(df2$Object1BaseTime/50)*50
    if (nrow(df2) > 0)
    {
      for (r in 1:(nrow(df2) - 1))
      {
        if (df2$BaseTime[r] == df2$BaseTime[r + 1])
        {
          df2$BaseTime[r] <- NA
        }
      }
    }
    df2<-df2[!is.na(df2$BaseTime),]
    
    df3 <- data.frame(matrix(ncol = ncol(df2), nrow=0))
    colnames(df3) <- colnames(df2)
    for (trial in 1:16)
    {
      for (t in 1:40)
      {
        row1 <- df1[df1$TrialID == trial & df1$Event == "Object1" & df1$BaseTime == (t-1)*50,]
        row2 <- df2[df2$TrialID == trial & df2$Event == "Object2" & df2$BaseTime == (t-1)*50,]
        if (nrow(row1) > 1)
        {
          row1 <- row1[1,]
        }
        if (nrow(row2) > 1)
        {
          row2 <- row2[1,]
        }
        if (nrow(row1) > 0 & nrow(row2) > 0 &
            nrow(df1[df1$TrialID == trial & df1$Event == "Model" & !is.na(df1$AverageDiameterPupils)]) >= 10)
        {
          row1$Object21Diff <- row2$AverageDiameterPupils - row1$AverageDiameterPupils
        }
        if (nrow(row1) > 0)
        {
          df3 <- rbind(df3,row1)
        }
      }
      nn <- nrow(df3[!is.na(df3$Object21Diff) & df3$TrialID == trial,])
      if (nn < 3 & nn > 0)
      {
        df3$Object21Diff[df3$TrialID == trial] <- NA
      }
    }
    
    df3<-df3[!is.na(df3$Object21Diff),]
    df3$AverageDiameterPupils <- df3$Object21Diff
    new_filename <- paste(paste0(path_name_out, "Object21Diff"), paste0(basename(csv_files[i])), sep="/")
    write.csv(df3, new_filename)
  }
}
