##### Function: analyze_data_quality_trial #####
### Basic trial analysis ###

analyze_data_quality_trial <- function(df, from = -Inf, to = Inf) {
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
      y = DiameterPupilLeftEye,
      color = "black"),
      shape = 2
    ) +
    geom_point(aes(
      x = BaseTime,
      y = DiameterPupilRightEye,
      color = "black"),
      shape = 6
    ) +
    geom_point(aes(x = BaseTime, y = EventNum/8+4)) +
    scale_color_manual(labels = c("Diameter"), 
                       values = c("black")) +
    labs(color = "Values") +
    ylab("Pupil Diameter") +
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
