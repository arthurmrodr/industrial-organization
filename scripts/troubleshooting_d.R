params <- rep(2, 7)
params <- c(-0.5, -0.01, 0.01, -0.02, #betas
            1, #gamma
            0, 1) #mu, sigma

beta1 <- params[1]
beta2 <- params[2]
beta3 <- params[3]
beta4 <- params[4]

gamma <- params[5]

mu <- params[6]
sigma <- params[7]


Pi_bar <- beta1*data$eld + beta2*data$pinc + beta3*data$lnhdd + beta4*data$ffrac # Parte não estocástica do lucro

xi <- log(data$n) - log(data$F_50 - data$n) - Pi_bar - gamma*(((data$n - 1)*p_entry)+1) # Vetor xi calculado a partir do logit

normal_density <- pnorm(xi, mean = mu, sd = sigma) # Densidade da normal

p_entry <- exp(xi + Pi_bar) / (1 + exp(xi + Pi_bar)) # Probabilidade de entrada de uma dada firma

#p_dm <- pbinom(data$n, size = data$F_50, prob = p_entry) # Probabilidade de observarmos o vetor de decisões de entrada no mercado
p_dm <- (p_entry^data$n) * ((1-p_entry)^(data$F_50-data$n))

like <- p_dm * normal_density

l <- sum(log(like))





like <- (exp(xi + Pi_bar) / (1 + exp(xi + Pi_bar))) * normal_density

like <- (exp(xi + Pi_bar) / (1 + exp(xi + Pi_bar))) * xi

pr_dm <- choose(data$F_50, data$n) * (p_entry^data$n) *(1 - p_entry)^(F_50 - data$n)

like_amost <- prod(like)