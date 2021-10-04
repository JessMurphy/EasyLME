
########## DEFINE FUNCTIONS ###########

# function for producing a summary table of the input data
# inputs: data - data frame containing the variables of interest
#         y - response variable name (character)
#         time - time variable name (character)
#         group - grouping variable name (character)
#         re - random effect variable name (character)
#         covariates - optional covariate variable names (character)
# output: a data frame

summary_table <- function(data, y, time, group, re, covariates=""){
  
  # extract the levels of the random effect variable
  donors = levels(data[[re]])
  
  status = time_out = y_out = c()
  for (i in 1:length(donors)){ # loop through the random effect levels
    
    # subset the data by random effect level
    temp = data[data[[paste(re)]] == donors[i],]
    
    # store information about the grouping status of the subsetted data
    status = rbind(status, as.character(temp[[group]][1]))
    
    # calculate summary statistics for the numeric variables (time & response)
    time_sum = data.frame(nrow(temp), min(temp[[time]]), median(temp[[time]]), max(temp[[time]]), mean(temp[[time]]), sd(temp[[time]]))
    y_sum = data.frame(min(temp[[y]]), median(temp[[y]]), max(temp[[y]]), mean(temp[[y]]), sd(temp[[y]]))
    
    # combine information across all levels
    time_out = rbind(time_out, time_sum)
    y_out = rbind(y_out, y_sum)
  }
  
  colnames(time_out) = c("count", "min", "median", "max", "mean", "sd")
  colnames(y_out) = c("min", "median", "max", "mean", "sd")
  
  # calculate the percentage of observations per random effect level, format the numeric values, & combine the
  # mean/sd into one variable, the min/median/max into another variable, and the count/percentage into another
  time_out2 = time_out %>% mutate(percent=(count/sum(count))*100, across(min:percent, ~formatC(.x, format="f", digits=1))) %>%
    mutate(mean=paste0(mean, " (", sd, ")"), median=paste0(median, " [", min, ", ", max, "]"),
           count=paste0(count, " (", percent, "%)")) %>% select(-c(min, max, sd, percent))
  
  # format the numeric values & combine the mean/sd into one variable and the min/median/max into another variable
  y_out2 = y_out %>% mutate(across(min:sd, ~formatC(.x, format="f", digits=1))) %>%
    mutate(mean=paste0(mean, " (", sd, ")"), median=paste0(median, " [", min, ", ", max, "]")) %>% 
    select(-c(min, max, sd))
  
  # combine all the information into a single data frame
  out = data.frame(donors, status, time_out2, y_out2)
  
  # rename the columns
  colnames(out) = c(re, group, "Count", "Median [Min, Max]", "Mean (SD)", "Median [Min, Max]", "Mean (SD)")
  
  return(out)
}


# functions for fitting mixed effects models to nested data
# inputs: data - data frame containing the variables of interest
#         y - response variable name (character)
#         time - time variable name (character)
#         group - grouping variable name (character)
#         donor - clustered random effect variable name (character)
#         mouse - nested random effect variable name (character)
#         covariates - optional covariate variable names (character)
# output: model object

# create a model with random slopes and random intercepts for both donor and mouse
slopes_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  slopes_name = paste(donor, "Intercept/Slope +", mouse, "Intercept/Slope")
  
  #specify fixed and random effects
  fixed = paste(y, "~", group, "*", time)
  slope_donor = paste("(", time, "|", donor, ")")
  slope_mouse = paste("(", time, "|", donor, ":", mouse, ")")
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    slopes_formula = as.formula(paste(fixed, covars, slope_donor, slope_mouse, sep="+"))
  } else {
    slopes_formula = as.formula(paste(fixed, slope_donor, slope_mouse, sep="+"))
  }
  #fit model
  slopes = lmerTest::lmer(slopes_formula, data)
  
  #output list with model and model name
  return(list(slopes_name, slopes))
}

# create a model with a random slope and intercept for mouse and a random intercept for donor
mouse_slope_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  mouse_slope_name = paste(donor, "Intercept +", mouse, "Intercept/Slope")
  
  #specify fixed and random effects
  fixed = paste(y, "~", group, "*", time)
  int_donor = paste("(", 1, "|", donor, ")")
  slope_mouse = paste("(", time, "|", donor, ":", mouse, ")")
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    mouse_slope_formula = as.formula(paste(fixed, covars, int_donor, slope_mouse, sep="+"))
  } else {
    mouse_slope_formula = as.formula(paste(fixed, int_donor, slope_mouse, sep="+"))
  }
  #fit model
  mouse_slope = lmerTest::lmer(mouse_slope_formula, data)
  
  #output list with model and model name
  return(list(mouse_slope_name, mouse_slope))
}

# create a model with a random slope and intercept for mouse
mouse_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  mouse_name = paste(mouse, "Intercept/Slope")
  
  #specify fixed and random effects
  fixed = paste(y, "~", group, "*", time)
  slope_mouse = paste("(", time, "|", donor, ":", mouse, ")")
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    mouse_formula = as.formula(paste(fixed, covars, slope_mouse, sep="+"))
  } else {
    mouse_formula = as.formula(paste(fixed, slope_mouse, sep="+"))
  }
  #fit model
  mouse_ = lmerTest::lmer(mouse_formula, data)
  
  #output list with model and model name
  return(list(mouse_name, mouse_))
}

# create a model with just a random intercept for mouse
mouse_int_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  mouse_int_name = paste(mouse, "Intercept")
  
  #specify fixed and random effects
  fixed = paste(y, "~", group, "*", time)
  int_mouse = paste("(", 1, "|", donor, ":", mouse, ")")
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    int_formula = as.formula(paste(fixed, covars, int_mouse, sep="+"))
  } else {
    int_formula = as.formula(paste(fixed, int_mouse, sep="+"))
  }
  #fit model
  mouse_int = lmerTest::lmer(int_formula, data)
  
  #output list with model and model name
  return(list(mouse_int_name, mouse_int))
}

# create a model with no random effects
noRE_model <- function(data, y, time, group, covariates=""){
  
  #specify model name
  noRE_name = "No Random Effects"
  
  #specify fixed effects
  fixed = paste(y, "~", group, "*", time)
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    formula = as.formula(paste(fixed, covars, sep="+"))
  } else {
    formula = as.formula(fixed)
  }
  #fit model
  noRE_mod = stats::lm(formula, data)
  
  #output list with model and model name
  return(list(noRE_name, noRE_mod))
}

# create a model with a random slope and intercept for a non-nested random effect
REslope_model <- function(data, y, time, group, re, covariates=""){
  
  #specify model name
  REslope_name = paste(re, "Intercept/Slope")
  
  #specify fixed and random effects
  fixed = paste(y, "~", group, "*", time)
  REslope = paste("(", time, "|", re, ")")
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    REslope_formula = as.formula(paste(fixed, covars, REslope, sep="+"))
  } else {
    REslope_formula = as.formula(paste(fixed, REslope, sep="+"))
  }
  #fit model
  REslope_mod = lmerTest::lmer(REslope_formula, data)
  
  #output list with model and model name
  return(list(REslope_name, REslope_mod))
}

# create a model with just a random intercept for a non-nested random effect
REint_model <- function(data, y, time, group, re, covariates=""){
  
  #specify model name
  REint_name = paste(re, "Intercept")
  
  #specify fixed and random effects
  fixed = paste(y, "~", group, "*", time)
  REint = paste("(", 1, "|", re, ")")
  
  #add covariates if applicable & create formula for model
  if (isTruthy(paste(covariates))){
    covars = paste(covariates, collapse = "+", sep = "")
    REint_formula = as.formula(paste(fixed, covars, REint, sep="+"))
  } else {
    REint_formula = as.formula(paste(fixed, REint, sep="+"))
  }
  #fit model
  REint_mod = lmerTest::lmer(REint_formula, data)
  
  #output list with model and model name
  return(list(REint_name, REint_mod))
}

# function for results table
# inputs: a list of models, a vector of pvalues, and a list of model names
# output: a data frame of the results

results_table <- function(models, pvalues, names){
  
  out = p = c()
  for (i in 1:length(models)){
    
    # store the summary of the coefficients for each model 
    mod.sum = summary(models[[i]])$coefficients
    
    # extract the desired information from the summary
    est = round(mod.sum[2:4, "Estimate"], 2)
    se = round(mod.sum[2:4, "Std. Error"], 2)
    pval = mod.sum[2:4, "Pr(>|t|)"]
    log = round(logLik(models[[i]]), 2)
    
    temp = c(paste0(est, " (", se, ")"), log)
    out = rbind(out, temp)
    p = rbind(p, pval)
  }
  
  results = data.frame(names, out, pvalues)
  colnames(results) = c("Model", "Group*", "Time*", "Interaction*", "Likelihood**", "Pvalue***")
  rownames(results) = NULL
  
  results2 = data.frame(names, p)
  colnames(results2) = c("Model", "Group*", "Time*", "Interaction*")
  rownames(results2) = NULL
  
  return(list(results, results2))
}


# function for coefficient data
# inputs: a list of models and a list of model names
# output: a data frame of the coefficient estimates, standard errors, and 
#         confidence intervals for each fixed effect term per model

coef_data <- function(models, names){
  
  n = length(models)
  terms = c("Group", "Time", "Interaction")
  
  out = c()
  for (i in 1:n){ # loop through the models
    
    # extract the coefficient estimates and standard errors from the model summary
    sum = summary(models[[i]])$coefficients[2:4, 1:2]
    
    # compute the confidence intervals for the coefficient estimates
    if (i!=n) { # mixed model
      ci = confint(models[[i]], parm="beta_", method="Wald")[-1,]
    } else { # linear model
      ci = confint(models[[i]])[-1,]
    }
  
    out = rbind(out, cbind(sum, ci))
  }
  
  # add the names of the terms and models
  data = as.data.frame(out) %>% remove_rownames() %>%
    mutate(Term=rep(terms, times=n), Model=rep(names, each=3)) %>% 
    rename(ci.low=`2.5 %`, ci.high=`97.5 %`)
  
  # specify the factor variables
  data$Model = factor(data$Model, levels=names, labels=names)
  data$Term = factor(data$Term, levels=terms, labels=terms)
  
  return(data)
}
  

# function to recreate the ggplot2 color palette 
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# function for donor fitted lines
# inputs: a model object as well as a data frame and variable names
# output: a scatterplot of response vs time with fitted lines

# plot donor fitted lines based on specified model using ggplot
donor_lines = function(model, data, y, time, group, donor){
  
  # determine if model is an LME or not (LMEs are S4 objects whereas lms are not)
  if (isS4(model)) {
    
    # extract donor coefficients / names for LME models
    donor_coef = coef(model)[[paste(donor)]] 
    donors = rownames(donor_coef)
    
  } else {
    
    # extract donor coefficients / names for lm models
    donor_coef = as.data.frame(t(coef(model)))
    donor_coef[2:8,] =  donor_coef[1,]
    donors = names(summary(data[,paste(donor)]))
  }
  
  # create empty vectors to use in loop
  intercepts = c()
  slopes = c()
  Group = c()
  
  # determine the intercept, slope, and donor status for each donor
  for(i in 1:length(donors)){
    Intercept = donor_coef[i,1] + (data[data[[paste(donor)]] == donors[i],][[paste(group)]] == levels(data[[paste(group)]])[2])[1]*donor_coef[i,2]
    Slope = donor_coef[i,3] + (data[data[[paste(donor)]] == donors[i],][[paste(group)]] == levels(data[[paste(group)]])[2])[1]*donor_coef[i,4]
    Status = ifelse((data[data[[paste(donor)]] == donors[i],][[paste(group)]] == levels(data[[paste(group)]])[1])[1], levels(data[[paste(group)]])[1], levels(data[[paste(group)]])[2])
    
    intercepts = c(intercepts, Intercept)
    slopes = c(slopes, Slope)
    Group = c(Group, Status)
  }
  
  # create data frame with info needed for fitted lines
  donor_data = data.frame(donors, Group, intercepts=as.numeric(as.character(intercepts)), slopes=as.numeric(as.character(slopes)))
  donor_data$donors = factor(donor_data$donors, levels=levels(data[[paste(donor)]]))
  
  colnames(donor_data)[1:2] = c(donor, group)
  
  # specify color vector (Donor) and title for plot
  data[[paste(donor)]] = data[[paste(donor)]]
  title = paste(donor, "Fitted Lines", sep=" ")
  
  # create scatterplot with fitted lines (color based on donor, type based on group)
  p = ggplot() + 
    geom_point(data=data, aes(x=.data[[paste(time)]], y=.data[[paste(y)]], color=.data[[paste(donor)]]), show.legend=F) +
    geom_abline(data=donor_data, aes(intercept=intercepts, slope=slopes, color=.data[[paste(donor)]], linetype=.data[[paste(group)]])) +
    theme_pubr(border=T) + labs(x=paste(time), y=paste(y)) + guides(color="none") +
    theme(legend.title=element_blank())
  
  return(p)
}

# function for mouse fitted lines
# inputs: a model object as well as a data frame and variable names
# output: a scatterplot of response vs time with fitted lines faceted by donor

# coef(model)[[`donor:mouse`]] only includes the mouse random effects
# need to add ranef(model)[[donor]] + ranef(model)[[`donor:mouse`]] to the fixed effects instead

# plot mouse fitted lines based on specified model using ggplot
mouse_lines = function(model, data, y, time, group, donor, mouse){
  
  # extract mice and donor names from data
  mice = names(summary(data[,paste(mouse)]))
  donors = sub("^(.*)[.].*", "\\1", mice)
  name = paste0(donor, ":", mouse)
  
  # define default values for mouse & donor effects
  donor_int = rep(0, length(donors))
  donor_slope = rep(0, length(donors))
  mouse_int = rep(0, length(mice))
  mouse_slope = rep(0, length(mice))
  
  # determine if model is an LME or not (LMEs are S4 objects whereas lms are not)
  if (isS4(model)) {
    
    # extract fixed effects for LME models
    fix_effects = fixef(model)
    
    # determine if model contains mouse effects  
    if (!is.null(ranef(model)[[paste(name)]])) { 
      
      # extract mouse random effects
      mouse_effects = ranef(model)[[paste(name)]] 
      
      # determine if the mouse effects contain a slope & intercept or just intercept
      if (length(mouse_effects)==2){
        mouse_int = mouse_effects[,1]
        mouse_slope = mouse_effects[,2]
      } else {
        mouse_int = mouse_effects[,1]
      }
    }
    
    # determine if model contains donor effects  
    if (!is.null(ranef(model)[[paste(donor)]])) { 
      
      # extract donor random effects
      donor_effects = ranef(model)[[paste(donor)]] 
      
      # store donor effects per mouse
      donor_effects2 = c()
      for (i in 1:length(donors)){
        row = which(rownames(donor_effects)==donors[i])
        effect_donor = donor_effects[row,]
        donor_effects2 = rbind(donor_effects2, effect_donor)
      }
      # determine if donor effects contain a slope & intercept or just intercept
      if (length(donor_effects2)==2){
        donor_int = donor_effects2[,1]
        donor_slope = donor_effects2[,2]
      } else {
        donor_int = donor_effects2[,1]
      }
    }
  } else {
    
    # extract fixed effects for lm models
    fix_effects = coef(model)
  }
  
  # create empty vectors to use in loop
  intercepts = c()
  slopes = c()
  Group = c()
  
  # determine the intercept, slope, and donor status for each mouse
  for(i in 1:length(mice)){
    Intercept = fix_effects[1] + (data[data[[paste(mouse)]] == mice[i],][[paste(group)]] == levels(data[[paste(group)]])[2])[1]*fix_effects[2] +
      mouse_int[i] + donor_int[i]
    Slope = fix_effects[3] + (data[data[[paste(mouse)]] == mice[i],][[paste(group)]] == levels(data[[paste(group)]])[2])[1]*fix_effects[4] +
      mouse_slope[i] + donor_slope[i]
    Status = ifelse((data[data[[paste(mouse)]] == mice[i],][[paste(group)]] == levels(data[[paste(group)]])[1])[1], levels(data[[paste(group)]])[1], levels(data[[paste(group)]])[2])
    
    intercepts = c(intercepts, Intercept)
    slopes = c(slopes, Slope)
    Group = c(Group, Status)
  }
  
  # create data frame with info needed for fitted lines
  type_data = data.frame(mice, donors, Group, intercepts=as.numeric(as.character(intercepts)), slopes=as.numeric(as.character(slopes)))
  type_data$donors = factor(type_data$donors, levels=levels(data[[paste(donor)]]))
  type_data$mice = factor(type_data$mice, levels=levels(data[[paste(mouse)]]))
  
  colnames(type_data)[1:3] = c(mouse, donor, group)
  
  # specify color vector (Donor) and title for plot
  data[[paste(donor)]] = as.factor(data[[paste(donor)]]) 
  title = paste(mouse, "Fitted Lines", sep=" ")
  
  # create scatterplot with fitted lines (color based on donor, type based on group)
  p = ggplot() + 
    geom_point(data=data, aes(x=.data[[paste(time)]], y=.data[[paste(y)]], color=.data[[paste(group)]]), show.legend=FALSE) +
    geom_abline(data=type_data, aes(intercept=intercepts, slope=slopes, color=.data[[paste(group)]])) +
    theme_pubr(border=T) + labs(x=paste(time), y=paste(y)) + guides(color="none") +
    theme(legend.title=element_blank()) + facet_wrap(~.data[[paste(donor)]])
  
  return(p)
}
