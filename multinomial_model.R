library(R2jags)
library(R2WinBUGS)

mult_jags <- function(outcomes, dataset, 
                      HC_precision = 0.1,
                      student_colname = "student.num",
                      level_colname = "question.num",
                      plays_colname = "count") {
  data_list <- list(student = dataset[,student_colname],
                    Y = outcomes,
                    plays = dataset[,plays_colname],
                    level = dataset[,level_colname],
                    n = dim(dataset)[1],
                    L = length(unique(dataset[,level_colname])),
                    S = length(unique(dataset[,student_colname])),
                    HC_precision = HC_precision)
  
  model.mult <- "
  model {
  ##
  #Likelihood
  ##
  for(i in 1:n) {
  p0[i] <- ilogit(-eta[student[i]] - gamma[level[i]])
  p1[i] <- 1 - p0[i] - p2[i]
  p2[i] <- 1 - ilogit(tau - eta[student[i]] - gamma[level[i]])
  Y[i,] ~ dmulti(c(p0[i], p1[i], p2[i]), plays[i])
  }
  
  ##
  #Prior on cutoff
  #Uniform from 0 to infinity
  ##
  tau ~ dunif(0, 1000)
  
  prec <- HC_precision
  
  ##
  #Student priors
  ##
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
    tau <- 2
    sd.l <- 0.1
    sd.s <- 0.1
    return(list(eta = eta, gamma = gamma, tau = tau, sd.l = sd.l, sd.s = sd.s))
  }
  
  parameters <- c("eta", "gamma", "tau", "sd.l", "sd.s")
  
  ptm <- proc.time()
  HF.sim <- jags(data_list, inits = inits, parameters, model.file = textConnection(model.mult), n.chains = 5, n.iter = 5000)
  HF.bugs <- as.mcmc(HF.sim$BUGSoutput$sims.matrix)
  ptm_end <- proc.time() - ptm
  
  return(list(jags_obj = HF.sim, post_mat = HF.bugs, time = ptm_end['elapsed'], mod_list = data_list))
}


#########Version that works on the Duke server
model.mult <- "
model {
##
#Likelihood
##
for(i in 1:n) {
p0[i] <- ilogit(-eta[student[i]] - gamma[level[i]])
p1[i] <- ilogit(tau - eta[student[i]] - gamma[level[i]]) - ilogit(-eta[student[i]] - gamma[level[i]])
p2[i] <- 1 - ilogit(tau - eta[student[i]] - gamma[level[i]])
p[i,1] <- p0[i]
p[i,2] <- p1[i]
p[i,3] <- p2[i]
Y[i,] ~ dmulti(p[i,], plays[i])
}

##
#Prior on cutoff
#Uniform from 0 to infinity
##
tau ~ dunif(0, 1000)

prec <- HC_precision

##
#Student priors
##
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