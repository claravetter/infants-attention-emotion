##### Function: analyze_data_quality_trial_average_only_emotion #####
### Average only trial analysis with emotion onlys###

analyze_data_quality_trial_average_only_emotion <- function(df, from = -Inf, to = Inf) {
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
  
  #  lm_model <- lm(AverageDiameterPupils ~ BaseTime, data = df1)
  #  df2 <- data.frame(avg_pred = predict(lm_model, df1), BaseTime=df1$BaseTime, 
  #                    ModelEmo=df1$ModelEmo, 
  #                    AverageDiameterPupils=df1$AverageDiameterPupils)
  #  slope <- coef(lm_model)["BaseTime"]
  df2 <- df1
  plot <- 
    ggplot(df2) +
    geom_point(aes(
      x = BaseTime,
      y = AverageDiameterPupils,
      color = factor(ModelEmo)),
      shape = 19,
    ) +
    ylim(-.5, .5) +
    xlim(0,1000) +
    #    geom_line(color='red', aes(x=BaseTime, y=avg_pred)) +
    ggtitle(paste(id, 
                  as.character(unique(df$ModelEmo)), as.character(unique(as.character(df2$ObjectPos))==as.character(df2$ModelLook))
                  #                  , floor(100000*slope)
    )) +
    theme(plot.title = element_text(lineheight = .4, face = "bold", color = "black")) +
    theme(legend.position = "none") +
    theme(legend.title = element_blank())
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
