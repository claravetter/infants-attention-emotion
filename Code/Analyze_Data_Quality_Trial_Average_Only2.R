##### Function: analyze_data_quality_trial_average_only2 #####
### Average only trial analysis (modifications)###

analyze_data_quality_trial_average_only2 <- function(df, from = -Inf, to = Inf) {
  # Count NAs
  message(
    "Baby ",
    df$Subject[1],
    " (trial ",
    df$TrialID[1],
    "): ",
    sum(
      !is.na(df$DiameterPupilLeftEye) &
        !is.na(df$DiameterPupilRightEye)
    ),
    " ",
    round(100 * sum(
      !is.na(df$DiameterPupilLeftEye) &
        !is.na(df$DiameterPupilRightEye)
    ) / nrow(df), 1),
    "% valids"
  )
  id <- paste0("Trial ", df$TrialID[1])
  # Plots
  df1 <- df[df$BaseTime >= from & df$BaseTime <= to,]
  plot <- 
    ggplot(df1) +
    geom_point(aes(
      x = BaseTime,
      y = AverageDiameterPupils,
      color = factor(Event)),
      shape = 19
    ) +
    geom_point(aes(x = BaseTime, y = EventNum/8+4)) +
    ggtitle(id) +
    theme(plot.title = element_text(lineheight = .4, face = "bold", color = "black"))
  print(plot)
  list(
    nrow(df),
    sum(!is.na(df$DiameterPupilLeftEye)),
    100 * sum(!is.na(df$DiameterPupilLeftEye)) / nrow(df),
    sum(!is.na(df$DiameterPupilRightEye)),
    100 * sum(!is.na(df$DiameterPupilRightEye)) / nrow(df),
    sum(
      !is.na(df$DiameterPupilLeftEye) & !is.na(df$DiameterPupilRightEye)
    ),
    100 * sum(
      !is.na(df$DiameterPupilLeftEye) &
        !is.na(df$DiameterPupilRightEye)
    ) / nrow(df),
    plot
  )
}
