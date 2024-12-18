

library(caret); library(purrr); library(dplyr); library(survival)
library(DTRreg); library(randomForestSRC); library(IHsurvrf)

setwd("~/survrf/Scripts/Data Simulations")
source("F00.generic.R")


setwd("~/RL_IBS")
##Add source file reading in data
source('Scripts/04_preprocessing_trtcateg.R')

setwd("~/RL_IBS")
source("Scripts/05_helpers.R")

value.criterion = "mean"

tau = 1000
timepoints = seq(0, sqrt(tau), length.out = 300)^2

## tuning parameters

nodesize = 5
mindeath = round(sqrt(c(nodesize)), 0)
rule =  "mean" 
ert = TRUE; rs = 0.2 # randomSplit = 0.2


## parameters
nstages = 3

# this is the number of cross validation folds we use (simple train/test split will use 1)
K = 10

# Define the range of number of trees to assess
tree_sizes <- seq(100, 900, by = 100)

# Create a data structure to store results for each number of trees
results <- list()

# Loop over the number of trees
for (n_tree in tree_sizes) {
  cat("Evaluating with", n_tree, "trees...\n")
  
  # Update the number of trees for the current iteration
  Ntree <- n_tree

# skeleton for results
values <- matrix(NA, K, 5, 
                 dimnames = list(1:K, c("IHsurvRF", "observed", "ZOM", "ns.IHsurvRF", "ns.ZOM")))


## ----------------------------------------- ##
##     train/test split for 10 fold CV       ##
## ----------------------------------------- ##

set.seed(5, kind="Mersenne-Twister")
cv.insample  = createDataPartition(analysis_dat$A_1, p = .8, list = FALSE, times = K)
cv.outsample = map_dfc(1:K, ~which(!(1:nrow(analysis_dat) %in% cv.insample[, .]))) %>% as.matrix


## for each cross validated sample
# 1:K
for (cv in 1:K){
  
  cat(cv, "th cv.\n")
  set.seed(cv)
  in.cv = cv.insample[, cv]   # insample index
  out.cv = cv.outsample[, cv] # outsample index
  train = analysis_dat[in.cv, ]
  test  = analysis_dat[out.cv,]

## we also need to create a report of the average amount of censoring for each stage
censoring <- matrix(NA, K, nstages, 
                    dimnames = list(1:K, paste0("cens_", 1:nstages)))


## for each cross validated sample, we estimate the censoring in each stage
for (stage in 1:nstages) {
  # Construct the dynamic column name for each stage
  column_name <- paste0("cens_", stage)
  
  # Calculate the mean for each stage and update the matrix
  censoring[cv, column_name] <- mean(train[[paste0("delta_", stage)]] == 0, na.rm = T)*100
}



## ------------------------------------------- ##
##            Calculating weights              ##
## NOTE: this is only used in value estimation ##
## for estimation based on IPCW
## ------------------------------------------  ##

## first, our formula for the weights

## covariates:
### previous visit length (for each stage)
### number of previous stages
### days since last hospitalization (state 1)
### number of previous hospitalizations (state 2)
### number of times patient did NOT receive drugs (action 0 count)
### number of times patient received drugs (action 1 count)
### age at first resection (baseline)
### sex (baseline)
## cumulative time so far
## Action


### we build a separate propensity score model for each stage

prform <- "factor(A_%d) ~ prev.visit.length_%d + nstageprev_%d + A1.count_%d + A0.count_%d + age.first.resc_%d + der.sex_%d + cumulative.time_%d"



form.weight <- list()

# Loop over stages 1 to 25
for (stage in 1:nstages) {
  
  # Use as.formula to convert the string into a formula
  formula <- sprintf(
    prform, 
    stage, stage, stage, stage, stage, stage, stage, stage
  ) %>% as.formula()
  
  # Save the model in the list, dynamically naming it
  form.weight[[paste0('prop', stage, '.formula')]] <- formula
}



## we also create our survival model using random forests for censoring weights

# excluded: days.since.last.hosp.cum_%d 

# Dynamically create the formula with the appropriate stage number
csform <- "Surv(visit.length_%d, delta_%d) ~ prev.visit.length_%d + nstageprev_%d + A1.count_%d + A0.count_%d + age.first.resc_%d + der.sex_%d + A_%d + cumulative.time_%d"

form.cs <- list()

# Loop over stages 1 to 25
for (stage in 1:nstages) {
  
  # Use as.formula to convert the string into a formula
  formula <- sprintf(
    csform, 
    stage, stage, stage, stage, stage, stage, stage, stage, stage, stage
  ) %>% as.formula()
  
  # Save the model in the list, dynamically naming it
  form.cs[[paste0('prop', stage, '.formula')]] <- formula
}


## ----------------------------------------- ##
##     Rule Estimation on training data      ##
## ----------------------------------------- ##


args.IHsurvrf <- list(data = train %>% select(- c("overall.delta", "overall.time")), 
                 txName = paste("A", 1:nstages, sep = "_"),
                 models = Surv(visit.length, delta) ~ A + prev.visit.length + nstageprev + 
                   #days.since.last.hosp.cum +   ### pausing this for now since some patients have never had one
                   A1.count + A0.count + age.first.resc + der.sex + cumulative.time,
                 #usePrevTime = FALSE, 
                 tau = tau, timePoints = timepoints,
                 nTimes = length(timepoints),
                 tieMethod = "random",
                 criticalValue = value.criterion, evalTime = NULL, 
                 splitRule = "mean",
                 ERT = ert, uniformSplit = ert, replace = !ert,
                 randomSplit = rs, nTree = Ntree, mTry = rep(6, nstages),
                 stratifiedSplit = FALSE, 
                 stageLabel = "_", nstrata = 1,
                 sampleSize = 1,
                 windowsize = 10)

values[cv, "ns.IHsurvRF"] = nodesize


# actual fitting
set.seed(cv)

IHsurvrf.policy <- 
  try(do.call(IHsurvrf, c(args.IHsurvrf, list(nodeSize = nodesize, minEvent = mindeath))))
err.IHsurvrf = class(IHsurvrf.policy)[1] == "try-error"



## ----------------------------------------- ##
##        Applying rules to test set         ##
## ----------------------------------------- ##

# here, the goal is to use the trained forests to output predictions for the test set

 #--- predicted treatment --- #
opt.rule.IHsurvrf = setNames(data.frame(matrix(NA, nrow = nrow(test), ncol = nstages)),
                             paste0("A_", 1:nstages))




for (q in 1:nstages) {
  if (!err.IHsurvrf) {
    
    
    updated_formula <- form.cs[[q]]
    
    models = Surv(visit.length, delta) ~ A + prev.visit.length + nstageprev + 
      #days.since.last.hosp.cum +   ### pausing this for now since some patients have never had one
      A1.count + A0.count + age.first.resc + der.sex + cumulative.time
    
    x = get_all_vars(updated_formula, test %>% filter(!is.na(!!sym(paste0(as.character(attr(terms(models), "variables")[[2]][[2]]),"_",(q) )))))
    
    elig =  !is.na(test[[paste0("visit.length_", q)]])
    
    
    ## remove stage suffixto use in prediction
    new_col_names <- gsub(paste0("_", (q), "$"), "", colnames(x))
    colnames(x) <- new_col_names
    
    
    opt.IHsurvrf = PredDTRSurvStep(object = IHsurvrf.policy@Forest1@FinalForest,
                    newdata = x,
                    params = IHsurvrf.policy@params, findOptimal = T)
    
    
    opt.rule.IHsurvrf[elig, q] = opt.IHsurvrf$optimal@optimalTx # %>% as.numeric()
    
  }
}


rm(IHsurvrf.policy)
gc()

## ----------------------------------------------------- ##
##      Comparator: training ZOM + predicting            ##
## ----------------------------------------------------- ##


# actual fitting
set.seed(cv)

### A5. zero-order model
zom.policy <- 
  try(do.call(IHsurvrf, c(args.IHsurvrf, list(nodeSize = 1e+9, minEvent = 1e+9))))
err.zom = class(zom.policy)[1] == "try-error"

values[cv, "ns.ZOM"] = 1e+9


#--- predicted treatment --- #
opt.rule.ZOM = setNames(data.frame(matrix(NA, nrow = nrow(test), ncol = nstages)),
                             paste0("A_", 1:nstages))




for (q in 1:nstages) {
  if (!err.zom) {
    
    
    updated_formula <- form.cs[[q]]
    
    models = Surv(visit.length, delta) ~ A + prev.visit.length + nstageprev + 
      #days.since.last.hosp.cum +   ### pausing this for now since some patients have never had one
      A1.count + A0.count + age.first.resc + der.sex + cumulative.time
    
    x = get_all_vars(updated_formula, test %>% filter(!is.na(!!sym(paste0(as.character(attr(terms(models), "variables")[[2]][[2]]),"_",(q) )))))
    
    elig =  !is.na(test[[paste0("visit.length_", q)]])
    
    
    ## remove stage suffixto use in prediction
    new_col_names <- gsub(paste0("_", (q), "$"), "", colnames(x))
    colnames(x) <- new_col_names
    
    
    opt.ZOM = PredDTRSurvStep(object = zom.policy@Forest1@FinalForest,
                                   newdata = x,
                                   params = zom.policy@params, findOptimal = T)
    
    
    opt.rule.ZOM[elig, q] = opt.ZOM$optimal@optimalTx # %>% as.numeric()
    
  }
}


rm(zom.policy)
gc()


## ----------------------------------------- ##
##             Value Estimation              ##
## ----------------------------------------- ##


## testing propensity score & weight generation
weight = weights_claims(data = test, weight.formula = form.weight, weight.cens = form.cs, nstage = nstages)

## at each stage, we extract the actual treatments that the patient received 
test.tmp = test %>% select(paste0("A_", 1:nstages)) 


arg.val = list(test = test, actual = test.tmp, propensity = weight$propensity,
               weight.censor = weight$weight.censor, criterion = value.criterion,
               tau = tau)

## our method
if (!err.IHsurvrf)  values[cv, "IHsurvRF"] = do.call(getValue, c(arg.val, list(estimated = opt.rule.IHsurvrf)))

## observed
if (!err.IHsurvrf) values[cv, "observed"] = do.call(getValue, c(arg.val, list(estimated = test.tmp)))

if (!err.zom)  values[cv, "ZOM"] = do.call(getValue, c(arg.val, list(estimated = opt.rule.ZOM)))

}

# Store results for this tree size
results[[as.character(n_tree)]] <- values
}

save(results, file = "RDA_forest_results_1strata.RData")



#write.csv(values, "/nas/longleaf/home/js9gt/survrf/Outputs/RDA_res_10", row.names=FALSE)


# Analyze and compare results across tree sizes
results_summary <- lapply(results, function(res) colMeans(res, na.rm = TRUE))
results_summary


mean_value_IHsurvrf <- sapply(results_summary, function(res) res["IHsurvRF"])
mean_value_observed <- sapply(results_summary, function(res) res["observed"])


# Combine into a data frame
plot_data <- data.frame(
  Trees = tree_sizes,
  MeanIHsurvRF = mean_value_IHsurvrf,
  meanobserved = mean_value_observed
)

# Create the line plot
ggplot(plot_data) +
  # Plot IHsurvRF values
  geom_line(aes(x = Trees, y = MeanIHsurvRF), color = "cornflowerblue", size = 1) +
  geom_point(aes(x = Trees, y = MeanIHsurvRF), color = "bisque4", size = 3) +
  # Plot observed values
  geom_line(aes(x = Trees, y = meanobserved), color = "grey", size = 1, linetype = "dashed") +
  geom_point(aes(x = Trees, y = meanobserved), color = "grey", size = 3) +
  # Labels and titles
  labs(
    x = "Number of Trees",
    y = "Value"  # Update y-axis label
  ) +
  # Blank background theme
  theme_void(base_size = 14) +
  theme(
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text = element_text(color = "black"),  # Add axis text
    axis.title.x = element_text(color = "black"),  # Ensure x-axis title styling remains default
    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, color = "black"),  # Vertical y-axis label
    plot.margin = margin(t = 10, r = 10, b = 40, l = 40)  # Add white space: top, right, bottom, left
  )


### Now, plotting our RDA results

observed <- results[[6]][, 2]
IHsurvrf <- results[[6]][, 1]
ZOM <- results[[6]][, 3]

## Combine data into a dataframe
data <- data.frame(
  Group = c(rep("observed", length(observed)), rep("IHsurvrf", length(IHsurvrf))),
  Value = c(observed, IHsurvrf)
)

ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(alpha = 0, outlier.shape = NA, aes(color = Group)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2), aes(color = Group), size = 3, alpha = 0.6) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 14),  # Adjust x-axis label size here
        axis.ticks.y = element_line(),
        axis.title.x = element_blank()) +
  theme(legend.position = "none") +
  geom_text(data = data.frame(Group = c("observed", "IHsurvrf"),
                              Value = c(mean(observed), mean(IHsurvrf))),
            aes(x = Group, y = Value, label = round(Value, 2)), vjust = 5, hjust = 0, color = "black", size = 4) +
  geom_point(data = data.frame(Group = c("observed", "IHsurvrf"),
                               Value = c(mean(observed), mean(IHsurvrf))),
             aes(x = Group, y = Value), color = "black", size = 3)






