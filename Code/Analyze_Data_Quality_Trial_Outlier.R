##### Function: analyze_data_quality_trial_outlier #####
### Outlier trial analysis ###

analyze_data_quality_trial_outlier <- function(df, from = -Inf, to = Inf) {
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
  message(id)
  left_mad <- mad(df$MaxLeftSlope, na.rm = T)
  left_mad <- ifelse(left_mad < 0.25, 0.25, left_mad)
  right_mad <- mad(df$MaxRightSlope, na.rm = T)
  right_mad <- ifelse(right_mad < 0.25, 0.25, right_mad)
  outlier_factor <- 8
  df1 <- df[df$BaseTime >= from & df$BaseTime <= to,]
  plot <- 
    ggplot(df1) +
    geom_point(aes(
      x = BaseTime,
      y = DiameterPupilLeftEye,
      color = ifelse(is.na(ifelse(MaxLeftSlope>outlier_factor*left_mad, "black", "red")), 
                     "black", ifelse(MaxLeftSlope>outlier_factor*left_mad, "black", "red"))),
      shape = ifelse(is.na(ifelse(df1$MaxLeftSlope>outlier_factor*left_mad, 24, 2)), 
                     2, ifelse(df1$MaxLeftSlope>outlier_factor*left_mad, 24, 2))
    ) +
    geom_point(aes(
      x = BaseTime,
      y = DiameterPupilRightEye,
      color = ifelse(is.na(ifelse(MaxRightSlope>outlier_factor*left_mad, "black", "red")), 
                     "black", ifelse(MaxRightSlope>outlier_factor*left_mad, "black", "red"))),
      shape = ifelse(is.na(ifelse(df1$MaxRightSlope>outlier_factor*left_mad, 25, 6)), 
                     6, ifelse(df1$MaxRightSlope>outlier_factor*left_mad, 25, 6))
    ) +
    geom_point(aes(x = BaseTime, y = EventNum/8+4)) +
    scale_color_manual(labels = c("Outlier", "Valid"), 
                       values = c("red", "black")) +
    labs(color = "Values") +
    ylab("Pupil Diameter") +
    ggtitle(id) +
    theme(plot.title = element_text(lineheight = .4, face = "bold", color = "black"))
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
