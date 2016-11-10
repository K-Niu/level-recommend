library(R2jags)
library(R2WinBUGS)

bi_jags <- function(dataset,
                    HC_precision = 0.1,
                    student_colname = "student.num",
                    level_colname = "question.num",
                    success_colname = "success",
                    plays_colname = "count") {
  data_list <- list(student = dataset[,student_colname],
                    Y = dataset[,success_colname],
                    level = dataset[,level_colname],
                    plays = dataset[,plays_colname],
                    n = dim(dataset)[1],
                    L = length(unique(dataset[,level_colname])),
                    S = length(unique(dataset[,student_colname])),
                    HC_precision = HC_precision)
  model.bi <- "
  model {
  ##
  #Likelihood
  ##
  for(i in 1:n) {
  logit(p[i]) <- eta[student[i]] + gamma[level[i]]
  Y[i] ~ dbinom(p[i], plays[i])
  }
  
  ##
  #Hyper priors for random effects
  ##
  prec <- HC_precision
  
  ##
  #Student priors
  #
  sd.s ~ dt(0, prec, 1) T(0,)
  phi.s <- pow(sd.s, -2)
  for(s in 1:S) {
  eta[s] ~ dnorm(0, phi.s)
  }
  
  ##
  #Level priors
  ##
  sd.l ~ dt(0, prec, 1) T(0,)
  phi.l <- pow(sd.l, -2)
  for(l in 1:L) {
  gamma[l] ~ dnorm(0, phi.l)
  }
  
  }
  "
  
  inits <- function() {
    eta <- rep(0, data_list$S)
    gamma <- rep(0, data_list$L)
    sd.l <- 0.1
    sd.s <- 0.1
    return(list(eta = eta, gamma = gamma, sd.l = sd.l, sd.s = sd.s))
  }
  
  parameters <- c("eta", "gamma", "sd.l", "sd.s")
  
  ptm <- proc.time()
  game.sim <- jags(data_list, inits = inits, parameters, model.file = textConnection(model.bi), n.iter = 5000)
  game.bugs <- as.mcmc(game.sim$BUGSoutput$sims.matrix)
  ptm_end <- proc.time() - ptm
  
  return(list(jags_obj = game.sim, post_mat = game.bugs, time = ptm_end['elapsed'], mod_list = data_list))
}