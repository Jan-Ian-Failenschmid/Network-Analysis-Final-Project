return_model <- function(df) {
  df <- df_clean
  #Extracts Variable Names
  var_names <- names(df[8:30])
  
  #Split up Data by year
  df2006 <- df[df$yearID == 2006, c("ID", var_names)]
  df2008 <- df[df$yearID == 2008, c("ID", var_names)]
  df2010 <- df[df$yearID == 2010, c("ID", var_names)]
  
  #Add Year Identifier to variable names
  names(df2006) <- paste(names(df2006), "2006", sep = "_")
  names(df2008) <- paste(names(df2008), "2008", sep = "_")
  names(df2010) <- paste(names(df2010), "2010", sep = "_")
  
  #Create a wide data set by ID
  df_overall <- full_join(df2006, df2008, by = c("ID_2006" = "ID_2008")) %>%
    full_join(df2010, by = c("ID_2006" = "ID_2010"))
  
  #Remove ID
  df_overall <- df_overall[, which(names(df_overall) != "ID_2006")]
  
  #Calculate and defines model specifications (provisional)
  covMat <- cov(df_overall, use = "pairwise.complete.obs")
  nobs <- nrow(df_overall)
  varMat <- matrix(names(df_overall), nrow = 23, ncol = 3, 
                   dimnames = list(var_names, c("V1", "V2", "V3")))
  lambda <- matrix(c(
    1,0,0,0,0,0,0,0,0,0,0,
    1,0,0,0,0,0,0,0,0,0,0,
    1,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,1
  ), ncol = 11, nrow = 23, byrow = T)
  
  latents <- c("Racial Attitudes", "Gay Attitudes", "Social Behaviour",
               "Living Standard", "Working Mothers", "Morals", 
               "Religiounsess", "Equal Wealth", 
               "Political Views", "News Consumption", "Happiness")
  
  #Defines Model
  model <- dlvm1(
    covs = covMat, # Covariance matrix to use
    nobs = nobs, # Number of observations to use
    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
    lambda = lambda, # Measurement model
    within_latent = "ggm", # Model within-subject contemporaneous as GGM
    within_residual = "chol", # Model within-subject residuals as Cholesky
    between_latent = "ggm", # Model between-subject latent as GGM
    between_residual = "chol", # Model between-subject residuals as Cholesky
    latents = latents # Names of the latent variables to use
  )
  
  #Returns Model
  return(model)
}

