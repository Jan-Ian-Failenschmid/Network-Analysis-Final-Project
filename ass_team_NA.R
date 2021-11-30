library("plyr")
new_data <- read.csv(file.choose(), stringsAsFactors=FALSE)

new_data_clean1 <- new_data[,which(!is.na(new_data[1,]))]
ColumnNames <- new_data_clean1[1,]

new_data_clean1 <- new_data_clean1[(new_data_clean1$GSS.YEAR.FOR.THIS.RESPONDENT > 2005 & new_data_clean1$GSS.YEAR.FOR.THIS.RESPONDENT < 2011),]

respondents <- count(new_data_clean1, 'RESPONDNT.ID.NUMBER')
panels <- respondents[respondents$freq > 2,]

new_data_clean2 <- new_data_clean1[which(new_data_clean1$RESPONDNT.ID.NUMBER %in% panels$RESPONDNT.ID.NUMBER),]

#list of variables
vars <- c('id', 'year','dateintv',  'age', 'sex', 'eqwlth','wrkstat','partyid',
          'posslq','degree','educ','born', 'sample',
          'liveblks', 'livewhts','marwht', 'marblk', 'marasian', 'marhisp',
          'wrkwayup', 'nextgen', 'toofast', 'advfront', 'socrel', 'socommun',
          'socfrend', 'satjob', 'parsol', 'kidssol', 'goodlife', 'premarsx',
          'teensex', 'xmarsex', 'homosex', 'pillok', 'marhomo', 'spanking',
          'fechld', 'fepresch', 'fefam', 'meovrwrk', 'punsin', 'blkwhite',
          'rotapple', 'permoral', 'incom16', 'finrela', 'polviews', 'happy',
          'joblose', 'news','relpersn', 'sprtprsn', 'nanotech')

ind_vars <- which(ColumnNames %in% vars)

new_data_clean3 <- new_data_clean2[,ind_vars]
ColumnNames1 <- ColumnNames[ind_vars]
colnames(new_data_clean3) <- ColumnNames1

write.csv(new_data_clean3, "clean_data.csv")
