##### Function: aggregate.ms #####
### Aggregate pupil data of 50 ms(within one event) ###

aggregate.ms <- function(path_name_in, path_name_out, interval){
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  for (i in 1:length(csv_files)) {
    df <- read.csv(csv_files[i], header = T)
    
    df1 <- df[F,]
    last_trial <- NA
    last_event <- NA
    diam_sum <- 0
    cnt <- 0
    last_base_time_interval <- NA
    for (r in 1:nrow(df))
    {
      row <- df[r,]
      event <- row$Event
      trial <- row$TrialID
      base_time_interval <- floor(row$BaseTime/interval)
      
      if (is.na(trial) |
          is.na(last_event) | 
          is.na(last_base_time_interval) | 
          last_trial != trial |
          last_event != event |
          last_base_time_interval != base_time_interval)
      {
        if (cnt > 1)
        {
          avg <- diam_sum / cnt;
        }
        else
        {
          avg <- NA
        }
        row$AverageDiameterPupils <- avg
        df1 <- rbind(df1, data.frame(row)) 
        diam_sum <- 0
        cnt <- 0
      }
      else
      {
        if (!is.na(row$AverageDiameterPupils))
        {
          diam_sum <- diam_sum + row$AverageDiameterPupils
          cnt <- cnt +1
        }
      }
      
      last_trial <- trial
      last_event <- event
      last_base_time_interval <- base_time_interval
    }
    
    df <- df1
    
    new_filename <- paste(path_name_out, paste0(basename(csv_files[i])), sep="/")
    write.csv(df, new_filename)
  }
}
