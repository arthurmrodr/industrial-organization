library(ggridges)
library(hrbrthemes)
library(viridis)
library(tidyverse)

#### Questão 3 ####

# Lendo base de dados

db_tire <- read_table("data/tiredealers.txt", col_names = FALSE) %>% 
  select(id = X1, n = X2, tpop = X3, ngrw = X4, pgrw = X5, 
         octy = X6, opop = X7, landv = X8, eld = X9, 
         ffrac = X10, pinc = X11, lnhdd = X12)

# Criando coluna que identifica 5 ou mais vendedores

db_tire <- db_tire %>% mutate(n_agg = ifelse(n<=5, n, 5))

## (a) ##

# Gráfico de densidade

ggplot(db_tire, aes(x = tpop, y = as.factor(n_agg), fill = as.factor(n_agg))) +
  geom_density_ridges(alpha=0.6,  stat="binline", bins = 30) +
  geom_density_ridges(alpha = 0.3)+
  theme_ridges()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Populacao") +
  ylab("Numero de negociantes")

# Boxplot



## (b) ##


# Escrevendo a função de verossimilhança da questão (b)

likelihood_b <- function(params, data) {
  
  # parameters to be chosen
  lambda1 <- params[1]
  lambda2 <- params[2]
  lambda3 <- params[3]
  lambda4 <- params[4]
  
  alpha1 <- params[5]
  alpha2 <- params[6]
  alpha3 <- params[7]
  alpha4 <- 0
  alpha5 <- params[8]
  
  beta2 <- params[9]
  beta3 <- params[10]
  beta4 <- params[11]
  beta7 <- params[12]
  
  gamma1 <- params[13]
  gamma2 <- params[14]
  gamma3 <- params[15]
  gamma4 <- params[16]
  gamma5 <- params[17]
  gammaL <- params[18]
  
  # market size
  S <- data$tpop + lambda1*data$opop + lambda2*data$ngrw + lambda3*data$pgrw + lambda4*data$octy
  
  # defining independent variables
  XBeta <- beta2*data$eld + beta3*data$pinc + beta4*data$lnhdd + beta7*data$ffrac
  W <- data$landv
  
  # building profit function
  Pi_bar1 <- S * (alpha1 + XBeta) - (gamma1 + gammaL*W)
  
  Pi_bar2 <- S * (alpha1 + XBeta - alpha2) - (gamma1 + gammaL*W + gamma2)
  
  Pi_bar3 <- S * (alpha1 + XBeta - alpha2 - alpha3) - (gamma1 + gammaL*W + gamma2 + gamma3)
  
  Pi_bar4 <- S * (alpha1 + XBeta - alpha2 - alpha3 - alpha4) - (gamma1 + gammaL*W + gamma2 + gamma3 + gamma4)
  
  Pi_bar5 <- S * (alpha1 + XBeta - alpha2 - alpha3 - alpha4 - alpha5) - (gamma1 + gammaL*W + gamma2 + gamma3 + gamma4 + gamma5)
  
  
  # building likelihood function
  ml <- rep(0, nrow(data))
  N <- data$n_agg
  
  ml[N == 0] <- 1 - pnorm(Pi_bar1[N == 0], mean = 0, sd = 1)
  ml[N == 1] <- pnorm(Pi_bar1[N == 1], mean = 0, sd = 1) - pnorm(Pi_bar2[N == 1], mean = 0, sd = 1)
  ml[N == 2] <- pnorm(Pi_bar2[N == 2], mean = 0, sd = 1) - pnorm(Pi_bar3[N == 2], mean = 0, sd = 1)
  ml[N == 3] <- pnorm(Pi_bar3[N == 3], mean = 0, sd = 1) - pnorm(Pi_bar4[N == 3], mean = 0, sd = 1)
  ml[N == 4] <- pnorm(Pi_bar4[N == 4], mean = 0, sd = 1) - pnorm(Pi_bar5[N == 4], mean = 0, sd = 1)
  ml[N == 5] <- pnorm(Pi_bar5[N == 5], mean = 0, sd = 1)
  
  l = sum(log(ml))
  
  return(-l)
  
}

initial_pars_b <- rep(0.01, 18)

sol_b <- optim(initial_pars_b, likelihood_b, data = db_tire, method = "BFGS", hessian = TRUE)
se_b <- sqrt(diag(solve(sol_b$hessian)))


res_b <- cbind(sol_b$par, se_b)

## (c) ##

# Escrevendo a função de verossimilhança da questão (c)


likelihood <- function(params, .data) {
  
  # parameters to be chosen
  lambda1 <- params[1]
  lambda2 <- params[2]
  lambda3 <- params[3]
  lambda4 <- params[4]
  
  alpha1 <- params[5]
  alpha2 <- params[6]
  alpha3 <- params[7]
  alpha4 <- params[8]
  alpha5 <- params[9]
  
  beta2 <- params[10]
  beta3 <- params[11]
  beta4 <- params[12]
  beta7 <- params[13]
  
  gamma1 <- params[14]
  gamma2 <- params[15]
  gamma3 <- params[16]
  gamma4 <- params[17]
  gamma5 <- params[18]
  gammaL <- params[19]
  
  # market size
  S <- .data$tpop + lambda1*.data$opop + lambda2*.data$ngrw + lambda3*.data$pgrw + lambda4*.data$octy
  
  # defining independent variables
  XBeta <- beta2*.data$eld + beta3*.data$pinc + beta4*.data$lnhdd + beta7*.data$ffrac
  W <- .data$landv
  
  # building profit function
  Pi_bar1 <- S * (alpha1 + XBeta) - (gamma1 + gammaL*W)
  
  Pi_bar2 <- S * (alpha1 + XBeta - alpha2) - (gamma1 + gammaL*W + gamma2)
  
  Pi_bar3 <- S * (alpha1 + XBeta - alpha2 - alpha3) - (gamma1 + gammaL*W + gamma2 + gamma3)
  
  Pi_bar4 <- S * (alpha1 + XBeta - alpha2 - alpha3 - alpha4) - (gamma1 + gammaL*W + gamma2 + gamma3 + gamma4)
  
  Pi_bar5 <- S * (alpha1 + XBeta - alpha2 - alpha3 - alpha4 - alpha5) - (gamma1 + gammaL*W + gamma2 + gamma3 + gamma4 + gamma5)
  
  
  # building likelihood function
  ml <- rep(0, nrow(.data))
  N <- .data$n
  
  ml[N == 0] <- 1 - pnorm(Pi_bar1[N == 0], mean = 0, sd = 1)
  ml[N == 1] <- pnorm(Pi_bar1[N == 1], mean = 0, sd = 1) - pnorm(Pi_bar2[N == 1], mean = 0, sd = 1)
  ml[N == 2] <- pnorm(Pi_bar2[N == 2], mean = 0, sd = 1) - pnorm(Pi_bar3[N == 2], mean = 0, sd = 1)
  ml[N == 3] <- pnorm(Pi_bar3[N == 3], mean = 0, sd = 1) - pnorm(Pi_bar4[N == 3], mean = 0, sd = 1)
  ml[N == 4] <- pnorm(Pi_bar4[N == 4], mean = 0, sd = 1) - pnorm(Pi_bar5[N == 4], mean = 0, sd = 1)
  ml[N == 5] <- pnorm(Pi_bar5[N == 5], mean = 0, sd = 1)
  
  l = sum(log(ml))
  
  return(-l)
  
}

initial_pars_c <- rep(0.01, 19)
sol <- optim(initial_pars_c, likelihood, .data = db_tire, method = "BFGS", hessian = TRUE)
se <- sqrt(diag(solve(sol$hessian)))

likelihood(data = data, initial_pars_c)


## (d) ##

# Criando colunas com especificações de número potencial de entrantes

db_tire <- db_tire %>% mutate(F_50 = 50, F_2x = n * 2)

# Retirando mercados com 0 firmas

data <- db_tire %>% filter(n != 0)

# Likelihood especificação com F=50

likelihood_seim_50 <-function(params, data){
  beta1 <- params[1]
  beta2 <- params[2]
  beta3 <- params[3]
  beta4 <- params[4]
  
  gamma <- params[5]
  
  mu <- params[6]
  sigma <- params[7]
  
  
  Pi_bar <- beta1*data$eld + beta2*data$pinc + beta3*data$lnhdd + beta4*data$ffrac # Parte não estocástica do lucro
  
  p_entry <- (data$n / data$F_50) # Probabilidade de entrada de uma dada firma
  
  xi <- log(data$n) - log(data$F_50 - data$n) - Pi_bar - gamma*(((data$n - 1)*p_entry)+1) # Vetor xi calculado a partir do logit
  
  normal_density <- dnorm(xi, mean = mu, sd = sigma) # Densidade da normal
  
  p_dm <- (p_entry^data$n)*((1-p_entry)^(data$F_50-data$n)) # Probabilidade de observarmos o vetor de decisões de entrada no mercado
  
  like <- p_dm * normal_density
  
  l <- sum(log(like))
  return(-l)
}

initial_par_seim <- c(-0.5, -0.01, 0.01, -0.02, #betas
                      1, #gamma
                      0, 1) #mu, sigma

sol_d <- optim(initial_par_seim, likelihood_seim_50, data = data, method = "Nelder-Mead", hessian = T)

se_d <- sqrt(diag(solve(sol_d$hessian)))

likelihood_seim(data = data, params = sol_d$par)

# Likelihood especificação com F=2n

likelihood_seim_2n <-function(params, data){
  beta1 <- params[1]
  beta2 <- params[2]
  beta3 <- params[3]
  beta4 <- params[4]
  
  gamma <- params[5]
  
  mu <- params[6]
  sigma <- params[7]
  
  
  Pi_bar <- beta1*data$eld + beta2*data$pinc + beta3*data$lnhdd + beta4*data$ffrac # Parte não estocástica do lucro
  
  p_entry <- (data$n / data$F_50) # Probabilidade de entrada de uma dada firma
  
  xi <- log(data$n) - log(data$F_2x - data$n) - Pi_bar - gamma*(((data$n1)*p_entry)+1) # Vetor xi calculado a partir do logit
  
  normal_density <- dnorm(xi, mean = mu, sd = sigma) # Densidade da normal
  
  p_dm <- (p_entry^data$n)*((1-p_entry)^(data$F_50-data$n)) # Probabil - idade de observarmos o vetor de decisões de entrada no mercado
  
  like <- p_dm * normal_density
  
  l <- sum(log(like))
  return(-l)
}
  

sol_d <- optim(initial_par_seim, likelihood_seim_2n, data = data, method = "Nelder-Mead", hessian = T)

se_d <- sqrt(diag(solve(sol_d$hessian)))

likelihood_seim(data = data, params = sol_d$par)

  


