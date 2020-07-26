##### Function: read_gazedata_files #####
### Transforming .gazedate (txt) files into .csv ###

read_gazedata_files <- function(path_name) {
  files <- list.files(
    path = path_name,
    #    pattern = '.*SRF.*.gazedata',
    pattern = '.*.gazedata',
    full.names = TRUE,
    recursive = FALSE
  )
  for (i in 1:length(files)) {
    current_file <- files[i]
    
    # remove path from filename
    filename <- basename(current_file)
    
    # extract infantNo and condition from filename
    split_filename <- strsplit(filename, '_')
    infant_no <- split_filename[[1]][1] # e.g. SRF201
    condition <- split_filename[[1]][2] # e.g. InfantEmoToolsFaces
    
    df <- read.delim(current_file)
    
    # missing data = - 1
    # in the original files, missing values are indicated by a value of -1
    df[df == -1] <- NA
    
    # add variable with condition
    df$Condition <- condition
    
    new_filename <-
      paste0("Babylab_files/Raw/", infant_no, "_", condition, ".csv")
    write.csv(df, new_filename, row.names = F)
  }
}
