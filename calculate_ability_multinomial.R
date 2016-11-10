library(dplyr)
library(stats4)
library(boot)
library(numDeriv)

#Estimate student ability using maximum likelihood (MLE)
estimate.ability <- function(play.data, level.difficulties, tau, fail = "fail", pass = "pass", excel = "excel") {
  processed.data = play.data %>%
    filter(result %in% c(fail, pass, excel))
  #If all the student's plays are excels, set ability to 5 to avoid numerical computation problems
  if(sum(processed.data$result == excel) == length(processed.data$result)) {
    return(5)
  }
  #If all the student's plays are fails, set ability to -5 to avoid numerical computation problems
  if(sum(processed.data$result == fail) == length(processed.data$result)) {
    return(-5)
  }
  processed.data = processed.data %>%
    arrange(timestamp) %>%
    group_by(level) %>%
    summarize(y0 = sum(result == fail), y1 = sum(result == pass), y2 = sum(result == excel)) %>%
    left_join(level.difficulties)
  
  negLL = function(eta) {
    return(sum(sapply(1:length(processed.data$level), function(i) {
      -processed.data$y0[i]*log(inv.logit(-(eta + processed.data$estimate[i]))) - processed.data$y1[i]*log(inv.logit(tau - (eta + processed.data$estimate[i])) - inv.logit(-(eta + processed.data$estimate[i]))) - processed.data$y2[i]*log(1 - inv.logit(tau - (eta + processed.data$estimate[i])))
    })))
  }
  return(mle(negLL, list(eta = 0))@coef)
}

#Estimate standard error of MLE using numeric method
estimate.error.numeric <- function(play.data, level.difficulties, tau, estimate, fail = "fail", pass = "pass", excel = "excel") {
  processed.data = play.data %>%
    filter(result %in% c(fail, pass, excel)) %>%
    arrange(timestamp) %>%
    group_by(level) %>%
    summarize(y0 = sum(result == fail), y1 = sum(result == pass), y2 = sum(result == excel)) %>%
    left_join(level.difficulties)
  
  likelihood = function(eta) {
    first.part = log(prod(sapply(1:length(processed.data$level), function(i) {
      return((inv.logit(-(eta + processed.data$estimate[i]))^processed.data$y0[i])*((inv.logit(tau - (eta + processed.data$estimate[i])) - inv.logit(-(eta + processed.data$estimate[i])))^processed.data$y1[i])*((1 - inv.logit(tau - (eta + processed.data$estimate[i])))^processed.data$y2[i]))
    })))
  }
  return(1/sqrt(-as.numeric(hessian(likelihood, estimate))))
}

#Iteratively estimate student ability
estimate.ability.iterative <- function(play.data, level.difficulties, tau, fail = "fail", pass = "pass", excel = "excel") {
  return(sapply(1:length(play.data$level), function(i) {
    return(estimate.ability(play.data[1:i,], level.difficulties, tau, fail, pass, excel))
  }))
}

#Iteratively estimate student ability error
estimate.error.iterative <- function(play.data, level.difficulties, tau, estimates, fail = "fail", pass = "pass", excel = "excel") {
  return(sapply(1:length(play.data$level), function(i) {
    return(estimate.error.numeric(play.data[1:i,], level.difficulties, tau, estimates[i], fail, pass, excel))
  }))
}
