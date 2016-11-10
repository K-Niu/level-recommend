library(dplyr)

source("multinomial_model.R")

#Runs multinomial (fail/pass/excel) Rasch model on game data
#Returns a list with level (gamma) and student (eta) posterior estimates
run_rasch_multinomial <- function(game.data, fail = "fail", pass = "pass", excel = "excel") {
  #Create input matrix to JAGS
  input.matrix <- data.frame(game.data) %>%
    filter(result %in% c(fail, pass, excel)) %>%
    select(result, student, level) %>%
    group_by(student, level) %>%
    summarize(count = n(), fail = sum(result == fail), pass = sum(result == pass), excel = sum(result == excel))
  
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
  input.matrix = as.data.frame(input.matrix)
  outcomes <- as.matrix(input.matrix[, c("fail", "pass", "excel")])
  result <- mult_jags(outcomes, input.matrix)
  
  ##Process output
  #Levels
  gamma.mat <- matrix(numeric(result$mod_list$L*dim(result$post_mat)[1]), ncol = result$mod_list$L)
  for(i in 1:result$mod_list$L){
    gamma.mat[,i] <- result$post_mat[,paste0("gamma[",i,"]")]
  }
  colnames(gamma.mat) = question.labels$level
  #Students
  eta.mat <- matrix(numeric(result$mod_list$S*dim(result$post_mat)[1]),ncol = result$mod_list$S)
  for(i in 1:result$mod_list$S){
    eta.mat[,i] <- result$post_mat[,paste0("eta[",i,"]")]
  }
  colnames(eta.mat) = student.labels$student
  #Category cutoff
  tau = mean(result$post_mat[,"tau"])
  
  return(list(gamma.mat = gamma.mat, eta.mat = eta.mat, tau = tau))
}
