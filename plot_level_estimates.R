library(dplyr)
library(reshape2)
library(ggplot2)

#Plot the level difficulty estimates
plot_level_estimates <- function(gamma.mat) {
  mat = data.frame(gamma.mat)
  colnames(mat) <- colnames(gamma.mat)
  
  #Add a column for row names so data frame can be melted
  mat$rowNum = 1:dim(mat)[1]
  
  #Melt each column and row bind
  mat.melted <- data.frame(rowNum = integer(), variable = character(), value = numeric())
  for(i in 1:(ncol(mat) - 1)) {
    iterator = melt(mat, id = "rowNum", measure = i)
    mat.melted = rbind(mat.melted, iterator)
  }
  colnames(mat.melted) <- c("rowNum", "level", "value")

  #Write out level difficulties
  level.difficulties <- mat.melted %>%
    group_by(level) %>%
    summarize(estimate = mean(value))
  write.csv(level.difficulties, "levelDifficulties.csv")
  
  #Plot level difficulties
  ggplot(mat.melted) +
    geom_boxplot(aes(x = level, y = value)) +
    facet_grid(. ~ level, scales = "free") +
    theme(axis.text.x = element_blank()) +
    labs(title = "Level difficulty estimates")
}
