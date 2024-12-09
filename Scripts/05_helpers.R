
library(randomForestSRC)
library(dplyr)

## Create a simple dataset with 5 rows and 3 stages (you can expand to 25 stages for full testing)
#set.seed(123)
## Step 1: Create a dataset for 100 patients with data for all stages (1, 2, and 3)
#data <- data.frame(
#  age = sample(20:70, 100, replace = TRUE),
#  cyto = as.factor(sample(c("positive", "negative"), 100, replace = TRUE)),
#  
#  # Stage 1 data
#  A.1 = sample(0:1, 100, replace = TRUE),
#  prevTime.1 = runif(100, 0, 10),
#  prevA.1 = sample(0:1, 100, replace = TRUE),
#  response.1 = sample(0:1, 100, replace = TRUE),
#  V.1 = rnorm(n = 100, mean = 100, sd = 20),
#  d.1 = sample(0:1, 100, replace = TRUE),
#  
#  # Stage 2 data (for 90 patients)
#  A.2 = c(sample(0:1, 90, replace = TRUE), rep(NA, 10)),
#  prevTime.2 = c(runif(90, 0, 10), rep(NA, 10)),
#  prevA.2 = c(sample(0:1, 90, replace = TRUE), rep(NA, 10)),
#  response.2 = c(sample(0:1, 90, replace = TRUE), rep(NA, 10)),
#  V.2 = c(rnorm(n = 90, mean = 100, sd = 20), rep(NA, 10)),
#  d.2 = c(sample(0:1, 90, replace = TRUE), rep(NA, 10)),
#  
#  # Stage 3 data (for 80 patients)
#  A.3 = c(sample(0:1, 80, replace = TRUE), rep(NA, 20)),
#  prevTime.3 = c(runif(80, 0, 10), rep(NA, 20)),
#  prevA.3 = c(sample(0:1, 80, replace = TRUE), rep(NA, 20)),
#  response.3 = c(sample(0:1, 80, replace = TRUE), rep(NA, 20)),
#  V.3 = c(rnorm(n = 80, mean = 100, sd = 20), rep(NA, 20)),
#  d.3 = c(sample(0:1, 80, replace = TRUE), rep(NA, 20))
#)
#
## Step 2: Set 10 patients (rows 91-100) to have NA for both stage 2 and stage 3
#data[91:100, c("A.2", "prevTime.2", "prevA.2", "response.2", "V.2", "d.2",
#               "A.3", "prevTime.3", "prevA.3", "response.3", "V.3", "d.3")] <- NA
#
## Step 3: Set another 10 patients (rows 81-90) to have NA for only stage 3
#data[81:90, c("A.3", "prevTime.3", "prevA.3", "response.3", "V.3", "d.3")] <- NA



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





## ---------------------- for propensity score portion with 20+ stages --------------------------- ##

#weights_claims <- function(data, weight.formula, weight.cens, nstages) {
#  # For all of our stages, we want to create a propensity score formula
#  ## A.stage ~ age + sex + prevStagetime + cumulative time + current time + # prev trts in prior stage,, etc
#  
#  # ------- Jane code ------ #
#  
#  # Extract variable names from the data frame and remove "subj.id" and "rep.id" if they exist in the setdiff()
#  ## then, extract all the unique portions before the underscore to be used as the common variable name
#  variables <- unique(sub("_(\\d+)", "", setdiff(colnames(data), c("subj.id", "rep.id", "overall.delta", "overall.time"))))
#  
#  long_data <-  data %>%
#    pivot_longer(cols = starts_with(variables),
#                 names_sep = "_",
#                 names_to = c(".value", "stage"),
#                 names_prefix = "",
#                 values_to = "value",
#                 values_drop_na = FALSE) %>%
#    mutate(stage = as.integer(stage)) %>%
#    mutate(A.original = A) %>%
#    arrange(subj.id, stage)
#  
#  
#  # Fit the model dynamically and store in the list
#  model <- glm(
#    formula = weight.formula, 
#    data = long_data, 
#    family = binomial
#  )
#  
#  
#  # Initialize a matrix to store cumulative propensity scores for each stage
#  cumulative_propensity_matrix <- matrix(NA, nrow = nrow(data), ncol = nstages)
#  colnames(cumulative_propensity_matrix) <- paste0('cumulative_prop_stage_', 1:nstages)
#  
#  # Predict the propensity score for the current stage from the fitted model
#  propensity_stage <- matrix(predict(model, newdata = long_data, type = "response"), nrow = nrow(data), ncol = nstages, byrow = T)
#  
#  # Loop over stages to calculate and store predicted propensity scores
#  for (stage in 1:nstages) {
#    
#    
#    # Dynamically reference the 'A.stage' column and apply ifelse transformations
#    A_stage_col <- paste0('A_', stage)
#    
#    # Ensure the column exists before processing
#    if (A_stage_col %in% names(data)) {
#      propensity_stage[,stage] <- ifelse(is.na(data[[A_stage_col]]), 
#                                         1, 
#                                         ifelse(data[[A_stage_col]] == 1, 
#                                                propensity_stage[, stage], 
#                                                1 - propensity_stage[, stage]))
#    } else {
#      stop(paste("Column", A_stage_col, "does not exist in the data"))
#    }
#    
#    # Store the predicted propensity scores in the matrix for each stage
#    cumulative_propensity_matrix[, stage] <- propensity_stage[,stage]
#  }
#  
#  ## now, we move onto IPCW
#  ## we estimate the survival function for each patient in the dataset using random survival forest
#  
#  # Initialize a list to store survival estimates for each stage
#  Sc.hat_stages <- list()
#  
#  weight.cens <- weight.cens
#  
#  # Loop over stages 1 to 25
#  #  for (stage in 1:nstages) {
#  #    
#  #
#  #    # Convert the string into a formula
#  #    formula <- weight.cens[[stage]]
#  #    
#  #    # Fit the Random Survival Forest model for the current stage
#  #    rfsrc_model <- rfsrc(formula, data = data)
#  #    
#  #    ## this should only be applied to patients who are present in the current stage
#  #    navail <- nrow(data %>% filter(!is.na(!!sym(sprintf("visit.length_%d", stage)))))
#  #    
#  #    # Estimate survival probability for each patient at a given evaluation time using linear interpolation 
#  #    ## returns the specific survival rpobability at a given time: t.eval which is the observed time of the stage
#  #    survival_prob <- sapply(1:navail, function(i) 
#  #      ## rfsrc_model$survival gives output survival curves for each patient: each row is a dif patient's survival func
#  #      ## time.interest gives us the time points at each evaluation of the survival curve
#  #      St2(rfsrc_model$survival[i, ], rfsrc_model$time.interest, t.eval = na.omit(data[[paste0('visit.length_', stage)]])[i]))
#  #    
#  #    # Store the survival estimates in the list
#  #    Sc.hat_stages[[stage]] <- survival_prob
#  #  }
#  #  
#  #  ## initialize Sc.hat with only survival probabilities from the first stage
#  #  Sc.hat <- Sc.hat_stages[[1]]
#  #  
#  #  # Second loop: Combine survival probabilities from all stages to reflect overall survival for patients who have made it to stage k
#  #  for (stage in 2:nstages) {
#  #    
#  #    # Identify which patients have valid data for the current stage (non-missing V.stage)
#  #    available_idx <- !is.na(data[[paste0('visit.length_', stage)]])
#  #    
#  #    # Multiply the survival probabilities from the previous stages with those from the current stage
#  #    Sc.hat[available_idx] <- Sc.hat[available_idx] * Sc.hat_stages[[stage]][available_idx]
#  #  }
#  #  
#  #  # clipping
#  #  ## survival probabilities are clipped to lie within the range [0.05, 0.95]
#  #  ## to prevent extreme weights < 0.05 and negligible weights > 0.95
#  #  Sc.hat <- Sc.hat %>% pmax(0.05) %>% pmin(0.95)
#  #  
#  #  ## censoring weights calculated by dividing the censoring indicator (data$delta) by the clipped survival probabilities
#  #  ## this uses an overall delta of whether or not a patient was censored
#  #  weight.censor = data$overall.delta/Sc.hat
#  #  
#  #  # p1 <<- propensity1
#  #  # pd1 <<- propensity1.DW
#  #  # c1 <<- Sc.hat1
#  
#  return(list(propensity = cumulative_propensity_matrix  ))
#  #weight.censor = weight.censor))
#  
#}




#getProp <- function(actual, estimated, propensity, nstages) {
#  ## first compares the actual treatments to the estimated treatments for each row using apply(actual == estimated, 1, all2).
#  
#  ## we need to repeat the propensity score for the number of stages the patient has
#  ## each row in actual corresponds to a patient, each non-NA column corresponds to a visit time
#  
#  
#  ## we need to transform the actual into the same form as the estimated by combining all the rows together into one and getting rid of NA
#  actual_num = as.numeric(as.vector(t(actual)))
#  
#  prop_vector = as.numeric(as.vector(t(propensity)))
#  
#  # Insert values from estimated into the non-NA positions
#  expanded_estimated <- as.numeric(as.vector(t(estimated)))
#  
#  
#  message("actual_num class", class(actual_num))
#  message("expanded_estimate class", class(expanded_estimated))
#  
#  #  weight =
#  #    ifelse(actual_num == expanded_estimated, 1, 0) %>%
#  #    # When there is at least one NA,
#  #    # 1. all() does not return TRUE                     all(c(NA, NA, T)) = NA; all(c(T, T, T)) = TRUE
#  #    # 2. all() returns NA if there is no FALSE          all(c(NA, NA, F, T)) = FALSE;  all(c(NA, NA)) = NA
#  #    # 3. all() returns FALSE if there is at least one FALSE
#  #    # When the second stage is not available in test set, the NA-match cases should still be counted. NA => 1.
#  #    
#  #    ## result of the comparison is passed through ifelse(is.na(.), 1, as.numeric(.)), which replaces NA with 1.
#  #    ## accounts for cases where a stage of treatment is not applicable (e.g., missing data), effectively treating them as a match.
#  #    {
#  #      ifelse(is.na(.), 0, as.numeric(.))
#  #    } %>%
#  #    
#  #    ## comparison results are multiplied by the propensity scores to create the initial weights.
#  #    "/" (prop_vector)
#  #  
#  #  return(weight)
#  
#  ## Calculate the weight, treating NA comparisons as matches (0)
#  weight <- ifelse(is.na(actual_num) | is.na(expanded_estimated), 0,
#                   ifelse(actual_num == expanded_estimated, 1, 0))
#  
#  ## Normalize by propensity score, avoiding division by zero
#  weight <- weight / prop_vector
#  
#  ## Normalize the weights so that they sum to 1
#  weight_total <- sum(weight, na.rm = TRUE)  # Total sum of weights, ignoring NAs
#  weight <- weight / weight_total  # Normalize each weight by the total sum
#  
#  
#  return(weight)
#}


## computes the weighted value of a treatment policy based on how well the estimated treatment matches the actual treatment
## considering censoring weights and propensities.
#getValue <- function(test, actual, estimated, propensity, weight.censor, criterion, tau) {
#  # weight = apply(actual == estimated, 1, all, na.rm = TRUE) / propensity
#  
#  ## convert into vector 
#  
#  ## first compares the actual treatments to the estimated treatments for each row using apply(actual == estimated, 1, all2).
#  weight = 
#    ifelse(actual == estimated, 1, 0) %>%   
#    # When there is at least one NA,
#    # 1. all() does not return TRUE                     all(c(NA, NA, T)) = NA; all(c(T, T, T)) = TRUE
#    # 2. all() returns NA if there is no FALSE          all(c(NA, NA, F, T)) = FALSE;  all(c(NA, NA)) = NA
#    # 3. all() returns FALSE if there is at least one FALSE
#    # When the second stage is not available in test set, the NA-match cases should still be counted. NA => 1.
#    
#    ## result of the comparison is passed through ifelse(is.na(.), 1, as.numeric(.)), which replaces NA with 1. 
#    ## accounts for cases where a stage of treatment is not applicable (e.g., missing data), effectively treating them as a match.
#    {ifelse(is.na(.), 1, as.numeric(.))} %>% 
#    
#    ## comparison results are divided by the propensity scores to create the initial weights.
#    "/" (propensity)
#  
#  ## weights are then multiplied by weight.censor to account for censoring
#  ### we skip this portion for now
#  #weight = weight * weight.censor
#  
#  ## ## returns the weighted mean of the outcome 
#  evaluate (test[, "overall.time"], weight = weight, criterion = criterion, tau = tau)
#}



### this function is used to find the area under the curve to track the integral of the survival functions for patients to see if they are converging
### this is used in IH.dtrSurv.R for the first pooling and last pooling
### also used in IH.pool1.R for each complete step of the pooling algorithm

# Define a function to calculate the area under the curve using trapezoidal rule
#area_under_curve <- function(surv_prob, time_points) {
#  # Sort time points and corresponding survival probabilities
#  sorted_indices <- order(time_points)
#  sorted_time_points <- time_points[sorted_indices]
#  sorted_surv_prob <- surv_prob[sorted_indices]
#  
#  # Calculate the width of each trapezoid
#  widths <- diff(sorted_time_points)
#  
#  # Calculate the average height of each trapezoid (average of adjacent survival probabilities)
#  heights <- (sorted_surv_prob[-1] + sorted_surv_prob[-length(sorted_surv_prob)]) / 2
#  
#  # Calculate the area of each trapezoid
#  areas <- widths * heights
#  
#  # Sum up the areas to get the total area under the curve
#  total_area <- sum(areas)
#  
#  return(total_area)
#}


