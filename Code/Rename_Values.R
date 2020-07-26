##### Function: rename_events #####
### Rename event values for consistency ###

rename_events <- function(path_name_in, path_name_out) {
  csv_files <-
    list.files(
      path = path_name_in,
      pattern = "*.csv",
      full.names = T,
      recursive = F
    )
  for (i in 1:length(csv_files)) {
    df <- read.csv(csv_files[i], header = T, stringsAsFactors = F)
    # the event value renaming is needed for my function:
    df$Event[df$Event == "Blank_Model"] <- "Blank_Single_Model"
    df$Event[df$Event == "Blank_Obj1"] <- "Blank_Object1"
    df$Event[df$Event == "Blank_Obj2"] <- "Blank_Object2"
    
    new_filename <- paste(path_name_out, paste0(basename(csv_files[i])), sep="/")
    write.csv(df, new_filename)
  }
}
