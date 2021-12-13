model_search_algorithm <- function(model) {
  # Estimation algorithm set-up:
  alpha <- 0.05
  adjust <- "none"
  searchstrategy <- "modelsearch" # Can also be "none" or "stepup"
  
  # Estimation algorithm (prune step):
  model_prune <- model %>% 
    runmodel %>% 
    prune(alpha = alpha, adjust = adjust, recursive = FALSE)
  
  # Search strategy
  # if (searchstrategy == "stepup"){
  #   model_prune <- model_prune %>%  stepup(alpha = alpha, criterion = "bic")
  # } else if (searchstrategy == "modelsearch"){
  #   model_prune <- model_prune %>%  modelsearch(prunealpha = alpha, addalpha = alpha)
  # } 
  
  #Returns optimized model
  return(model_prune)
}
