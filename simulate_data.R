library(boot)

#Simulates sample (fail/pass) game play data for a game with num.levels levels and num.students students
simulate_data_binomial <- function(num.levels, num.students) {
  #Create level difficulties in increasing order
  level.difficulties = data.frame(level = 1:num.levels, difficulty = seq(4, -4, length.out = num.levels))
  #Draw student abilities from a normal distribution
  student.abilities = data.frame(student = 1:num.students, ability = rnorm(num.students))
  #Write out true level difficulties and student abilities
  write.csv(level.difficulties, 'levelDifficultiesTrue.csv')
  write.csv(student.abilities, 'studentAbilitiesTrue.csv')
  
  #Create data frame to store simulated plays
  plays = data.frame(student = character(), level = character(), result = character(), timestamp = character())
  timestamp = 1;
  #Function to simulate plays by a student on a level
  simulate.play = function(s, l, reps) {
    for(i in 1:reps) {
      ability = student.abilities$ability[which(student.abilities$student == s)]
      difficulty = level.difficulties$difficulty[which(level.difficulties$level == l)]
      prob = inv.logit(ability + difficulty)
      result = rbinom(1, 1, prob)
      plays <<- rbind(plays, list(student = s, level = l, result = result, timestamp = timestamp))
      timestamp <<- timestamp + 1
    }
  }
  
  #For each student, simulate randomly between 1 and 50 games on random levels
  for(i in 1:num.students) {
    for(j in 1:round(runif(1, 1, 50))) {
      simulate.play(i, round(runif(1, 1, num.levels)), 1)
    }
  }
  
  return(plays)
}

#Simulates sample (fail/pass/excel) game play data for a game with num.levels levels and num.students students
simulate_data_multinomial <- function(num.levels, num.students) {
  #Create level difficulties in increasing order
  level.difficulties = data.frame(level = 1:num.levels, difficulty = seq(4, -4, length.out = num.levels))
  #Draw student abilities from a normal distribution
  student.abilities = data.frame(student = 1:num.students, ability = rnorm(num.students))
  #Write out true level difficulties and student abilities
  write.csv(level.difficulties, 'levelDifficultiesTrue.csv')
  write.csv(student.abilities, 'studentAbilitiesTrue.csv')
  #Set cutoff
  tau = 3.5
  
  #Create data frame to store simulated plays
  plays = data.frame(student = character(), level = character(), result = character(), timestamp = character())
  timestamp = 1;
  #Function to simulate plays by a student on a level
  simulate.play = function(s, l, reps) {
    for(i in 1:reps) {
      ability = student.abilities$ability[which(student.abilities$student == s)]
      difficulty = level.difficulties$difficulty[which(level.difficulties$level == l)]
      prob.lose = inv.logit(-(ability + difficulty))
      prob.excel = 1 - inv.logit(tau - (ability + difficulty))
      prob.win = 1 - prob.lose - prob.excel
      result = which(rmultinom(1, 1, c(prob.lose, prob.win, prob.excel)) == 1)
      plays <<- rbind(plays, list(student = s, level = l, result = result, timestamp = timestamp))
      timestamp <<- timestamp + 1
    }
  }
  
  #For each student, simulate randomly between 1 and 50 games on random levels
  for(i in 1:num.students) {
    for(j in 1:round(runif(1, 1, 50))) {
      simulate.play(i, round(runif(1, 1, num.levels)), 1)
    }
  }
  
  return(plays)
}