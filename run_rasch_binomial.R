library(dplyr)

source("binomial_model.R")

#Runs binomial (fail/pass) Rasch model on game data
#Returns a list with level (gamma) and student (eta) posterior estimates
run_rasch_binomial <- function(game.data, fail = "fail", pass = "pass") {
  #Create input matrix to JAGS
  input.matrix <- data.frame(game.data) %>%
    filter(result %in% c(fail, pass)) %>%
    select(result, student, level) %>%
    group_by(student, level) %>%
    summarize(count = n(), success = sum(result == pass))
  
  input.matrix$student.num <- as.numeric(factor(input.matrix$student))
  input.matrix$question.num <- as.numeric(factor(input.matrix$level))
  
  #Create input matrix labels
  student.labels <- data.frame(table(input.matrix$student, input.matrix$student.num)) %>%
    filter(Freq > 0) %>%
    rename(student = Var1, student.num = Var2) %>%
    arrange(student.num)
  question.labels <- data.frame(table(input.matrix$level, input.matrix$question.num)) %>%
    filter(Freq > 0) %>%
    rename(level = Var1, question.num = Var2) %>%
    arrange(question.num)
  
  #Run Rasch model
  model.result <- bi_jags(data.frame(input.matrix))
  
  ##Process output
  #Levels
  gamma.mat <- matrix(numeric(model.result$mod_list$L*dim(model.result$post_mat)[1]), ncol = model.result$mod_list$L)
  for(i in 1:model.result$mod_list$L){
    gamma.mat[,i] <- model.result$post_mat[,paste0("gamma[",i,"]")]
  }
  colnames(gamma.mat) = question.labels$level
  #Students
  eta.mat <- matrix(numeric(model.result$mod_list$S*dim(model.result$post_mat)[1]),ncol = model.result$mod_list$S)
  for(i in 1:model.result$mod_list$S){
    eta.mat[,i] <- model.result$post_mat[,paste0("eta[",i,"]")]
  }
  colnames(eta.mat) = student.labels$student
  
  return(list(gamma.mat = gamma.mat, eta.mat = eta.mat))
}
