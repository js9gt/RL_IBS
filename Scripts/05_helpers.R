
library(randomForestSRC)
library(dplyr)



weights_claims <- function(data, weight.formula, weight.cens, nstages) {
  # For all of our stages, we want to create a propensity score formula
  ## A.stage ~ age + sex + prevStagetime + cumulative time + current time + # prev trts in prior stage,, etc
  
  # ------- Jane code ------ #
  
  # Initialize a list to store the models
  models_list <- list()
  
  # Loop over stages 1 to 25
  for (stage in 1:nstages) {
    
    # Use as.formula to convert the string into a formula
    input_form <- weight.formula[[stage]]
    
    # Fit the model dynamically and store in the list
    model <- glm(
      formula = input_form, 
      data = data %>% filter(!is.na(get(paste0('A_', stage)))), 
      family = binomial
    )
    
    # Save the model in the list, dynamically naming it
    models_list[[paste0('prop', stage, '.model')]] <- model
  }
  
  # Initialize a matrix to store cumulative propensity scores for each stage
  cumulative_propensity_matrix <- matrix(NA, nrow = nrow(data), ncol = nstages)
  colnames(cumulative_propensity_matrix) <- paste0('cumulative_prop_stage_', 1:nstages)
  
  # Initialize a vector to keep track of the cumulative product of propensity scores
  cumulative_propensity <- rep(1, nrow(data))
  
  # Loop over stages to calculate and store cumulative propensity scores
  for (stage in 1:nstages) {
    
    # Dynamically get the model
    model <- models_list[[paste0('prop', stage, '.model')]]
    
    # Predict the propensity score for the current stage
    propensity_stage <- predict(model, type = "response")
    
    # Dynamically reference the 'A.stage' column and apply the ifelse transformations
    A_stage_col <- paste0('A_', stage)
    
    # Ensure the column exists before processing
    if (A_stage_col %in% names(data)) {
      propensity_stage <- ifelse(is.na(data[[A_stage_col]]), 
                                 1, 
                                 ifelse(data[[A_stage_col]] == 1, 
                                        propensity_stage, 
                                        1 - propensity_stage))
    } else {
      stop(paste("Column", A_stage_col, "does not exist in the data"))
    }
    
    # Update the cumulative product of propensity scores
    cumulative_propensity <- cumulative_propensity * propensity_stage
    
    # Store the cumulative propensity scores in the matrix
    cumulative_propensity_matrix[, stage] <- cumulative_propensity
  }
  
  
  ## now, we move onto IPCW
  ## we estimate the survival function for each patient in the dataset using random survival forest
  
  # Initialize a list to store survival estimates for each stage
  Sc.hat_stages <- list()
  
  # Loop over stages 1 to 25
  for (stage in 1:nstages) {
    

    # Convert the string into a formula
    formula <- weight.cens[[stage]]
    
    # Fit the Random Survival Forest model for the current stage
    rfsrc_model <- rfsrc(formula, data = data)
    
    ## this should only be applied to patients who are present in the current stage
    navail <- nrow(data %>% filter(!is.na(!!sym(sprintf("visit.length_%d", stage)))))
    
    # Estimate survival probability for each patient at a given evaluation time using linear interpolation 
    ## returns the specific survival rpobability at a given time: t.eval which is the observed time of the stage
    survival_prob <- sapply(1:navail, function(i) 
      ## rfsrc_model$survival gives output survival curves for each patient: each row is a dif patient's survival func
      ## time.interest gives us the time points at each evaluation of the survival curve
      St2(rfsrc_model$survival[i, ], rfsrc_model$time.interest, t.eval = na.omit(data[[paste0('visit.length_', stage)]])[i]))
    
    # Store the survival estimates in the list
    Sc.hat_stages[[stage]] <- survival_prob
  }
  
  ## initialize Sc.hat with only survival probabilities from the first stage
  Sc.hat <- Sc.hat_stages[[1]]
  
  # Second loop: Combine survival probabilities from all stages to reflect overall survival for patients who have made it to stage k
  for (stage in 2:nstages) {
    
    # Identify which patients have valid data for the current stage (non-missing V.stage)
    available_idx <- !is.na(data[[paste0('visit.length_', stage)]])
    
    # Multiply the survival probabilities from the previous stages with those from the current stage
    Sc.hat[available_idx] <- Sc.hat[available_idx] * Sc.hat_stages[[stage]][available_idx]
  }
  
  # clipping
  ## survival probabilities are clipped to lie within the range [0.05, 0.95]
  ## to prevent extreme weights < 0.05 and negligible weights > 0.95
  Sc.hat <- Sc.hat %>% pmax(0.05) %>% pmin(0.95)
  
  ## censoring weights calculated by dividing the censoring indicator (data$delta) by the clipped survival probabilities
  ## this uses an overall delta of whether or not a patient was censored
  weight.censor = data$overall.delta/Sc.hat
  
  # p1 <<- propensity1
  # pd1 <<- propensity1.DW
  # c1 <<- Sc.hat1
  
  return(list(propensity = cumulative_propensity_matrix,  weight.censor = weight.censor))
  
}




St2 <- function(surv, time, t.eval, tau = Inf, exponential.tail = TRUE) {
  # tau is a placeholder for compatibility
  time = c(0, time)
  surv =  c(1, surv)
  max.t = max(time)
  min.s = min(surv)
  if (t.eval > max.t) {
    if (exponential.tail) {
      S.t = 1 - (1-min.s)^(t.eval / max.t)
    } else {
      S.t = min.s
    }
  } else {
    S.t = approxfun(time, surv)(t.eval)
  }
  return(S.t)
}



#If all elements in x are TRUE (ignoring NAs), it returns TRUE.
#If there's at least one FALSE in x, it returns FALSE.
#If all elements in x are NA, it returns NA.

all2 = function(x) {
  # if everything is NA, return NA. Otherwise, TRUE only if all is TRUE.
  # The naive all() returns NA even if there are only TRUEs except NAs.
  na.index = is.na(x)
  if (all(na.index)) return(NA)
  all(x[!na.index])
}



#If criterion[1] == "mean":
#  The function calculates the mean of the weighted, truncated values of testY using tau as the truncation threshold.
#The truncation is performed by pmin(tau, testY), which limits the values in testY to tau.
# The result is normalized by dividing by the mean of the weights.

evaluate <- function(testY, weight, criterion, tau) {
  if (criterion[1] == "mean") {
    mean(pmin(tau, testY) * weight, na.rm = T)/ mean(weight, na.rm = T)
  } else {
    mean(as.numeric(testY >= as.numeric(criterion[2])) * weight)/ mean(weight)
  }
}

## computes the weighted value of a treatment policy based on how well the estimated treatment matches the actual treatment
## considering censoring weights and propensities.
getValue <- function(test, actual, estimated, propensity, weight.censor, criterion, tau) {
  # weight = apply(actual == estimated, 1, all, na.rm = TRUE) / propensity
  
  ## first compares the actual treatments to the estimated treatments for each row using apply(actual == estimated, 1, all2).
  weight = 
    apply(actual == estimated, 1, all2) %>%   
    # When there is at least one NA,
    # 1. all() does not return TRUE                     all(c(NA, NA, T)) = NA; all(c(T, T, T)) = TRUE
    # 2. all() returns NA if there is no FALSE          all(c(NA, NA, F, T)) = FALSE;  all(c(NA, NA)) = NA
    # 3. all() returns FALSE if there is at least one FALSE
    # When the second stage is not available in test set, the NA-match cases should still be counted. NA => 1.
    
    ## result of the comparison is passed through ifelse(is.na(.), 1, as.numeric(.)), which replaces NA with 1. 
    ## accounts for cases where a stage of treatment is not applicable (e.g., missing data), effectively treating them as a match.
    {ifelse(is.na(.), 1, as.numeric(.))} %>% 
    
    ## comparison results are divided by the propensity scores to create the initial weights.
    "/" (propensity)
  
  ## weights are then multiplied by weight.censor to account for censoring
  weight = weight * weight.censor
  
  ## ## returns the weighted mean of the outcome 
  evaluate (test[, "overall.time"], weight = weight, criterion = criterion, tau = tau)
}






