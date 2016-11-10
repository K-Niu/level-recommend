library(ggplot2)
library(boot)
library(dplyr)

source("calculate_ability_binomial.R")

#Plots recommended levels (between lower.prob and upper.prob chance of passing) for a student based on play data
plot_level_recommendations <- function(play.data, level.difficulties, fail = "fail", pass = "pass", lower.prob, upper.prob) {
  student.data = play.data
  #Iteratively estimate student ability using frequentist MLE based on level plays
  student.data$ability <- estimate.ability.iterative(student.data, levelDiff, fail = fail, pass = pass)
  #Function to calculate the win probability given a student ability and level
  win.prob <- function(student.ability, level) {
    level.difficulty = level.difficulties$estimate[which(level.difficulties$level == level)]
    return(inv.logit(student.ability + level.difficulty))
  }
  #Calculate which levels have passing probabilities falling within the desired range
  sets <- data.frame(xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric(), ability = numeric())
  for(i in 1:length(student.data$ability)) {
    level.set = c();
    for(j in level.difficulties$level) {
      level.prob = win.prob(student.data$ability[i], j)
      if(length(level.prob) != 0) {
        if((level.prob < upper.prob) & (level.prob > lower.prob)) {
          level.set = c(level.set, j)
        }
      }
    }
    sets = rbind(sets, data.frame(xmin = rep(i - 0.5, length(level.set)), xmax = rep(i + 0.5, length(level.set)), ymin = level.set - 0.5, ymax = level.set + 0.5, ability = rep(student.data$ability[i], length(level.set))))
  }
  
  #Create recommendation plot
  values.color = c("#F8766D", "#00BA38")
  names(values.color) = c(fail, pass)
  values.shape = c(4, 15)
  names(values.shape) = c(fail, pass)
  ggplot() + geom_rect(data = sets, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ifelse(ability > 3, 3, ability))) + geom_point(data = student.data, aes(x = 1:length(student), y = level, color = as.factor(result), shape = as.factor(result))) + scale_color_manual(values = values.color) + scale_shape_manual(values = values.shape) + labs(title = "Student recommendations", x = "play order", y = "level number", fill = "estimated ability", color = "level result", shape = "level result")
}