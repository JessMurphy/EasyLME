
##### DEFINE FUNCTIONS #####

# functions for fitting mixed effects models to nested data
# inputs: data - data frame containing the variables of interest
#         y - variable name (character)
#         time - variable name (character)
#         group - variable name (character)
#         donor - variable name (character)
#         mouse - variable name (character)
# output: model object

# create a model with random slopes and random intercepts for both donor and mouse
slopes_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  slopes_name = paste(donor, "Slope/Intercept +", mouse, "Slope/Intercept")
  
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
  slopes = lme4::lmer(slopes_formula, data)
  
  #output list with model and model name
  return(list(slopes_name, slopes))
}

# create a model with a random slope and intercept for mouse and a random intercept for donor
mouse_slope_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  mouse_slope_name = paste(donor, "Intercept +", mouse, "Slope/Intercept")
  
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
  mouse_slope = lme4::lmer(mouse_slope_formula, data)
  
  #output list with model and model name
  return(list(mouse_slope_name, mouse_slope))
}

# create a model with a random slope and intercept for mouse
mouse_model <- function(data, y, time, group, donor, mouse, covariates=""){
  
  #specify model name
  mouse_name = paste(mouse, "Slope/Intercept")
  
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
  mouse_ = lme4::lmer(mouse_formula, data)
  
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
  mouse_int = lme4::lmer(int_formula, data)
  
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
  REslope_name = paste(re, "Slope/Intercept")
  
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
  REslope_mod = lme4::lmer(REslope_formula, data)
  
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
  REint_mod = lme4::lmer(REint_formula, data)
  
  #output list with model and model name
  return(list(REint_name, REint_mod))
}


# function for donor fitted lines
# inputs: a model object as well as a data frame and variable names
# output: a plot of response vs time with fitted lines

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
    Intercept = donor_coef[i,1] + (data[data[[donor]] == donors[i],][[group]] == levels(data[[group]])[2])[1]*donor_coef[i,2]
    Slope = donor_coef[i,3] + (data[data[[donor]] == donors[i],][[group]] == levels(data[[group]])[2])[1]*donor_coef[i,4]
    Status = ifelse((data[data[[donor]] == donors[i],][[group]] == levels(data[[group]])[1])[1], levels(data[[group]])[1], levels(data[[group]])[2])
    
    intercepts = c(intercepts, Intercept)
    slopes = c(slopes, Slope)
    Group = c(Group, Status)
  }
  
  # create data frame with info needed for fitted lines
  donor_data = as.data.frame(cbind(donors, Group, as.numeric(as.character(intercepts)), as.numeric(as.character(slopes))))
  
  # specify color vector (Donor) and title for plot
  Donor = data[[paste(donor)]]
  title = paste(donor, "Fitted Lines", sep=" ")
  
  # create scatterplot with fitted lines (color based on donor, type based on group)
  p = ggplot() + 
    geom_point(aes(x=data[[time]], y=data[[y]], color=Donor), size=2.1) +
    geom_abline(data=donor_data, aes(intercept=intercepts, slope=slopes, color=donors, linetype=Group), size=1) +
    theme_bw(base_size=17) + labs(x=time, y=y, title=title, color=paste(donor))
  
  return(p)
}

# function for mouse fitted lines
# inputs: a model object as well as a data frame and variable names
# output: a plot of response vs time with fittled lines

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
    Intercept = fix_effects[1] + (data[data[[mouse]] == mice[i],][[group]] == levels(data[[group]])[2])[1]*fix_effects[2] +
      mouse_int[i] + donor_int[i]
    Slope = fix_effects[3] + (data[data[[mouse]] == mice[i],][[group]] == levels(data[[group]])[2])[1]*fix_effects[4] +
      mouse_slope[i] + donor_slope[i]
    Status = ifelse((data[data[[mouse]] == mice[i],][[group]] == levels(data[[group]])[1])[1], levels(data[[group]])[1], levels(data[[group]])[2])
    
    intercepts = c(intercepts, Intercept)
    slopes = c(slopes, Slope)
    Group = c(Group, Status)
  }
  
  # create data frame with info needed for fitted lines
  type_data = as.data.frame(cbind(mice, donors, Group, as.numeric(as.character(intercepts)), as.numeric(as.character(slopes))))
  
  # specify color vector (Donor) and title for plot
  Donor = as.factor(data[[paste(donor)]]) 
  title = paste(mouse, "Fitted Lines", sep=" ")
  
  # create scatterplot with fitted lines (color based on donor, type based on group)
  p = ggplot() + 
    geom_point(aes(x=data[[time]], y=data[[y]], color=Donor), size=2.1) +
    geom_abline(data=type_data, aes(intercept=intercepts, slope=slopes, color=donors, linetype=Group), size=1) +
    theme_bw(base_size=17) + labs(x=time, y=y, title=title, color=paste(donor))
  return(p)
}
