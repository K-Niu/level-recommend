library(dplyr)
library(stats4)
library(boot)
library(numDeriv)

#Estimate student ability using maximum likelihood (MLE)
estimate.ability <- function(play.data, level.difficulties, fail = "fail", pass = "pass") {
  processed.data = play.data %>%
    filter(result %in% c(fail, pass))
  #If all the student's plays are passes, set ability to 5 to avoid numerical computation problems
  if(sum(processed.data$result == pass) == length(processed.data$result)) {
    return(5)
  }
  #If all the student's plays are fails, set ability to -5 to avoid numerical computation problems
  if(sum(processed.data$result == fail) == length(processed.data$result)) {
    return(-5)
  }
  processed.data = processed.data %>%
    arrange(timestamp) %>%
    mutate(result = ifelse(result == pass, 1, 0)) %>%
    group_by(level) %>%
    summarize(n = n(), y = sum(result)) %>%
    left_join(level.difficulties)
  
  negLL = function(eta) {
    return(sum(sapply(1:length(processed.data$level), function(i) {
      processed.data$n[i]*log(1 + exp(eta + processed.data$estimate[i])) - processed.data$y[i]*(eta + processed.data$estimate[i])
    })))
  }
  return(mle(negLL, list(eta = 0))@coef)
}

#Estimate standard error of MLE using numeric method
estimate.error.numeric <- function(play.data, level.difficulties, estimate, fail = "fail", pass = "pass") {
  processed.data = play.data %>%
    filter(result %in% c(fail, pass)) %>%
    arrange(timestamp) %>%
    mutate(result = ifelse(result == pass, 1, 0)) %>%
    group_by(level) %>%
    summarize(n = n(), y = sum(result)) %>%
    left_join(level.difficulties)
  
  likelihood = function(eta) {
    first.part = log(prod(sapply(1:length(processed.data$level), function(i) {
      return(choose(processed.data$n[i], processed.data$y[i])*(inv.logit(eta + processed.data$estimate[i])^processed.data$y[i])*((1 - inv.logit(eta + processed.data$estimate[i]))^(processed.data$n[i] - processed.data$y[i])))
    })))
  }
  return(1/sqrt(-as.numeric(hessian(likelihood, estimate))))
}


#Iteratively estimate student ability
estimate.ability.iterative <- function(play.data, level.difficulties, fail = "fail", pass = "pass") {
  return(sapply(1:length(play.data$level), function(i) {
    return(estimate.ability(play.data[1:i,], level.difficulties, fail, pass))
  }))
}

#Iteratively estimate student ability error
estimate.error.iterative <- function(play.data, level.difficulties, estimates, fail = "fail", pass = "pass") {
  return(sapply(1:length(play.data$level), function(i) {
    return(estimate.error.numeric(play.data[1:i,], level.difficulties, estimates[i], fail, pass))
  }))
}
