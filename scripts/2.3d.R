likelihood_seim <-function(params, data){
  beta1 <- params[1]
  beta2 <- params[2]
  beta3 <- params[3]
  beta4 <- params[4]
  
  gamma <- params[5]
  
  mu <- params[6]
  sigma <- params[7]
  
  
  Pi_bar <- beta1*data$eld + beta2*data$pinc + beta3*data$lnhdd + beta4*data$ffrac # Parte não estocástica do lucro
  
  xi <- log(data$n) - log(data$F_50 - data$n) - Pi_bar - gamma*(((data$n - 1)*p_entry)+1) # Vetor xi calculado a partir do logit
  
  normal_density <- dnorm(xi, mean = mu, sd = sigma) # Densidade da normal
  
  p_entry <- (data$n / data$F_50) # Probabilidade de entrada de uma dada firma
  
  p_dm <- (p_entry^data$n)*((1-p_entry)^(data$F_50-data$n)) # Probabilidade de observarmos o vetor de decisões de entrada no mercado
  
  like <- p_dm * normal_density
  
  l <- sum(log(like))
  return(-l)
}

initial_par_seim <- c(-0.5, -0.01, 0.01, -0.02, #betas
                      1, #gamma
                      0, 1) #mu, sigma

sol_d <- optim(initial_par_seim, likelihood_seim, data = data, method = "Nelder-Mead", hessian = T)

se_d <- sqrt(diag(solve(sol_d$hessian)))

likelihood_seim(data = data, params = sol_d$par)



likelihood_seim_2x <-function(params, data){
  beta1 <- params[1]
  beta2 <- params[2]
  beta3 <- params[3]
  beta4 <- params[4]
  
  gamma <- params[5]
  
  mu <- params[6]
  sigma <- params[7]
  
  
  Pi_bar <- beta1*data$eld + beta2*data$pinc + beta3*data$lnhdd + beta4*data$ffrac # Parte não estocástica do lucro
  
  xi <- log(data$n) - log(data$F_2x - data$n) - Pi_bar - gamma*(((data$n - 1)*p_entry)+1) # Vetor xi calculado a partir do logit
  
  normal_density <- dnorm(xi, mean = mu, sd = sigma) # Densidade da normal
  
  p_entry <- (data$n / data$F_2x) # Probabilidade de entrada de uma dada firma
  
  p_dm <- (p_entry^data$n)*((1-p_entry)^(1-data$n)) # Probabilidade de observarmos o vetor de decisões de entrada no mercado
  
  like <- p_dm * normal_density
  
  l <- sum(log(like))
  return(-l)
}

sol_d_2x <- optim(initial_par_seim, likelihood_seim_2x, data = data, method = "Nelder-Mead", hessian = T)

se_d <- sqrt(diag(solve(sol_d$hessian)))




##################################################


exp(xi + Pi_bar) / (1 + exp(xi + Pi_bar))
