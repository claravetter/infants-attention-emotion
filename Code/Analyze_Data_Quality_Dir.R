##### Function: analyze_data_quality_dir #####
### Analyze the files in the specified directory ###

analyze_data_quality_dir <- function(path_name, label, overall_plot = T, average_plot = F, 
                                     from = -Inf, to = Inf, width = 16, height = 25, single = F, 
                                     outliers = F, average_only = F, no_event = F, emotion = F, common_legend= T) {
  csv_files <-
    list.files(
      path = path_name,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  df_quality <-
    data.frame(
      No = integer(0),
      File = character(0),
      Trial = integer(0),
      Measurements = integer(0),
      DiameterPupilLeftEyeValids = integer(0),
      DiameterPupilLeftEyeValidsPercent = numeric(0),
      DiameterPupilRightEyeValids = integer(0),
      DiameterPupilRightEyeValidsPercent = numeric(0),
      DiameterPupilBothEyesValids = integer(0),
      DiameterPupilBothEyesValidsPercent = numeric(0)
    )
  for (i in 1:length(csv_files)) {
    current_file <- csv_files[i]
    df <- read.csv(current_file, header = T)
    
    no_plot_pages <- ifelse (single, 1, 8)
    no_plot_rows <- ifelse (single, 1, 3)
    no_plot_cols <- ifelse (single, 1, 2)
    
    if (overall_plot)
    {
      plot_pages <- 0
      plot_list <- list()
      
      for (trial in 1:16) {
        if (nrow(df[df$TrialID == trial,]) > 0)
        {
          if (average_only)
          {
            if (no_event)
            {
              if (emotion)
              {
                quality <-
                  analyze_data_quality_trial_average_only_emotion(df[which(df$TrialID == trial),], from = from, to = to)
              }
              else
              {
                quality <-
                  analyze_data_quality_trial_average_only_no_event(df[which(df$TrialID == trial),], from = from, to = to)
              }
            }
            else
            {
              quality <-
                analyze_data_quality_trial_average_only(df[which(df$TrialID == trial),], from = from, to = to)
            }
          }
          else
          {
            if (outliers)
            {
              quality <-
                analyze_data_quality_trial_outlier(df[which(df$TrialID == trial),], from = from, to = to)
            }
            else
            {
              if (average_plot)
              {
                quality <-
                  analyze_data_quality_trial_average(df[which(df$TrialID == trial),], from = from, to = to)
              }
              else
              {
                quality <-
                  analyze_data_quality_trial(df[which(df$TrialID == trial),], from = from, to = to)
              }
            }
          }
          plot_list <- append(plot_list, quality[8])
        }
        
        if (length(plot_list) > 0 & plot_pages < no_plot_pages &
            (length(plot_list) == no_plot_cols * no_plot_rows | trial == 16))
        {
          page <-
            ggarrange(
              plotlist = plot_list,
              ncol = no_plot_cols,
              nrow = no_plot_rows,
              common.legend = common_legend,
              legend = "bottom"
            )
          id <- paste(df$Subject[1], label)
          page <- annotate_figure(page,
                                  top = text_grob(
                                    id,
                                    color = "black",
                                    face = "bold",
                                    size = 14
                                  ))
          ggsave(
            paste0("plot_", plot_pages, "_", id, ".png"),
            plot = page,
            width = width,
            height = height,
            units = "cm",
            device = "png",
            dpi = 300
          )
          print(page)
          plot_pages <- plot_pages + 1
          plot_list <- list()
        }
        
        if (nrow(df_quality) > 0)
        {
          df_quality %>% rbind(
            data.frame(
              No = i,
              Trial = trial,
              Measurements = as.integer(quality[1]),
              DiameterPupilLeftEyeValids = as.integer(quality[2]),
              DiameterPupilLeftEyeValidsPercent = as.numeric(quality[3]),
              DiameterPupilRightEyeValids = as.integer(quality[4]),
              DiameterPupilRightEyeValidsPercent = as.numeric(quality[5]),
              DiameterPupilBothEyesValids = as.integer(quality[6]),
              DiameterPupilBothEyesValidsPercent = as.numeric(quality[7])
            )
          ) -> df_quality
        }
      }
    }
  }
  
  # Diagnostic output
  message("=====================")
  message(label)
  message("=====================")
  message("Number of trials: ", nrow(df_quality))
  for (i in seq(50, 90, 10)) {
    valids <- sum(na.omit(df_quality$DiameterPupilLeftEyeValidsPercent) >= i)
    valids_perc <- round(100*valids/nrow(df_quality),1)
    valids <- sum(na.omit(df_quality$DiameterPupilRightEyeValidsPercent) >= i)
    valids_perc <- round(100*valids/nrow(df_quality),1)
    valids <- sum(na.omit(df_quality$DiameterPupilBothEyesValidsPercent) >= i)
    valids_perc <- round(100*valids/nrow(df_quality),1)
  }
  df_quality
}
