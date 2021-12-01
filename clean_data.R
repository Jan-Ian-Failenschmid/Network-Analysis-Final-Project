clean_data <- function(inp) {
  
  # Loads the data
  raw_df <- read_spss(inp)
  
  # Adds an unique identifier for each person
  raw_df$ID <- c(1:2000)
  
  # Splits up the data set into separate data sets for years 2006, 2008, 2010
  df2006 <- select(raw_df, c(ID, ends_with("_1")))
  df2008 <- select(raw_df, c(ID, ends_with("_2"))) %>%
    select(!c(intrace_2, indus_2, panstat_2))
  df2010 <- select(raw_df, c(ID, ends_with("_3"))) %>%
    select(!c(intrace_3, panstat_3))
  
  # Adds a year identifier to each seperate data file
  df2006$yearID <- 2006
  df2008$yearID <- 2008
  df2010$yearID <- 2010
  
  # Removes the waive identifier from the names
  names(df2006)[names(df2006) != "ID" & names(df2006) != "yearID"] <- sub("_1", "", names(df2006)[names(df2006) != "ID" & names(df2006) != "yearID"])
  names(df2008)[names(df2008) != "ID" & names(df2008) != "yearID"] <- sub("_2", "", names(df2008)[names(df2008) != "ID" & names(df2008) != "yearID"])
  names(df2010)[names(df2010) != "ID" & names(df2010) != "yearID"] <- sub("_3", "", names(df2010)[names(df2010) != "ID" & names(df2010) != "yearID"])
  
  # Combine the three waves
  df <- bind_rows(df2006, df2008, df2010)
  
  # Selects relevant variables
  df_clean <- select(df, c(
    ID, yearID,
    age, sex, wrkstat, degree, race,
    eqwlth, marblk, marwht, marasian, marhisp, marhomo, 
    socrel, socommun, socfrend, parsol, kidssol, goodlife,
    fechld, fepresch, fefam, punsin, blkwhite, rotapple, permoral,
    finrela, polviews, happy, news, relpersn, sprtprsn
  ))
  
  # Removes variables which have all Na's in one year
  df_clean <- select(df_clean, !unique(c(
    names(colSums(!is.na(df_clean[df_clean$yearID == 2006, ]))[colSums(!is.na(df_clean[df_clean$yearID == 2006, ])) == 0]),
    names(colSums(!is.na(df_clean[df_clean$yearID == 2008, ]))[colSums(!is.na(df_clean[df_clean$yearID == 2008, ])) == 0]),
    names(colSums(!is.na(df_clean[df_clean$yearID == 2010, ]))[colSums(!is.na(df_clean[df_clean$yearID == 2010, ])) == 0])
  )))
  
  
  # Removes all cases which were not assessed in all waives (works for now, should be moved up before variable selection might otherwice cause problems later)
  names_avar <- names(df_clean)[!names(df_clean) %in% c("ID", "yearID", "age", "sex", "wrkstat", "degree", "race")]
  exclude <- unique(df_clean$ID[rowSums(is.na(df_clean[names(df_clean) %in% names_avar])) == ncol(df_clean[names(df_clean) %in% names_avar])])
  df_clean <- filter(df_clean, !df_clean$ID %in% exclude)
  
  # Exclude people outside the working range
  ind_age <- df_clean[which(df_clean$yearID == 2010 & df_clean$age < 67),]
  data_age <- df_clean[which(df_clean$ID %in% ind_age$ID),]
  
  ##drop rows with too much missing data
  df_rows <- data_age[which(rowMeans(!is.na(data_age[ ,8:32])) > 0.50), ]
  
  return(df_rows)
}