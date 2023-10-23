library(tidyverse)
library(ggplot2)
library(kableExtra)
library()

quibble <- function(x, q = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}


data <- read_table("data/tiredealers.txt", col_names = FALSE) %>% 
  select(id = X1, n = X2, tpop = X3, ngrw = X4, pgrw = X5, 
         octy = X6, opop = X7, landv = X8, eld = X9, 
         ffrac = X10, pinc = X11, lnhdd = X12)


data %>% 
  mutate(n = ifelse(n >= 5, "5+", n)) %>% 
  ggplot(aes(x = factor(n), y = tpop)) +
  geom_boxplot() + 
  xlab("Nmero de Revendedores") +
  ylab("Popula") +
  theme_bw()


data %>% 
  mutate(n = ifelse(n >= 5, "5+", n)) %>% 
  group_by(n) %>% 
  summarise(quibble(tpop)) %>% 
  mutate(tpop_q = as.character(tpop_q)) %>% 
  rbind(data %>% mutate(n = ifelse(n >= 5, "5+", n)) %>% 
          group_by(n) %>% 
          summarise(tpop = mean(tpop), tpop_q = "Média")) %>% 
  pivot_wider(id_cols = tpop_q,
              values_from = tpop,
              names_from = n) %>% 
  rename("Percentil" = tpop_q) %>% 
  kbl(caption = "Distribuição da População pelo Número de Revendedores na Cidade",
      booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = "HOLD_position")


likelihood_gpt <- function(params, .data) {
  # Extract parameters
  lambdas <- params[1:4]
  alphas <- c(params[5:7], 0, params[8])
  betas <- params[9:12]
  gammas <- params[13:18]
  
  # Calculate the market size
  S <- .data$tpop + sum(lambdas * c(.data$opop, .data$ngrw, .data$pgrw, .data$octy))
  
  # Calculate XBeta and W
  XBeta <- sum(betas * c(.data$eld, .data$pinc, .data$lnhdd, .data$ffrac))
  W <- .data$landv
  
  # Calculate Pi_bar values
  Pi_bar <- numeric(5)
  Pi_bar[1] <- S * (alphas[1] + XBeta) - (gammas[1] + gammas[6] * W)
  for (i in 2:5) {
    Pi_bar[i] <- S * (alphas[1] - sum(alphas[2:i]) + XBeta) - (sum(gammas[1:i]) + gammas[6] * W)
  }
  
  # Calculate the likelihood
  N <- .data$n
  ml <- rep(0, nrow(.data))
  for (i in 1:5) {
    ml[N == (i - 1)] <- diff(pnorm(Pi_bar[i:(i + 1)], mean = 0, sd = 1))
  }
  
  l <- sum(log(ml))
  return(-l)
}

df$n <- pmin(df$n, 5)  # Cap n at 5

initial_pars <- c(-0.5, 2, 0.1, 0.1, 0.8, 0.01, 0.15, 0.08, -0.5, -0.01, 0.01, -0.02, 0.5, 0.8, 0.4, 0.6, 0.3, -0.7)

sol <- optim(initial_pars, likelihood_gpt, .data = df, method = "BFGS", hessian = TRUE)
sol <- mle(minuslogl = likelihood, start = rep(0,times = 19))
se <- sqrt(diag(solve(sol$hessian)))


conditional_decile_table <- db_tire %>%
  group_by(n_agg) %>%             # Group by the 'n_agg' variable
  mutate(decile = ntile(tpop, 10)) %>%  # Calculate deciles for 'tpop'
  group_by(n_agg, decile) %>%    # Group by 'n_agg' and decile
  summarise(
    decile_start = min(tpop),    # Calculate the min tpop in the decile
    decile_end = max(tpop),      # Calculate the max tpop in the decile
    decile_count = n()           # Calculate the count in the decile
  ) %>%
  ungroup()


conditional_decile_table <- db_tire %>%
  group_by(n_agg) %>%
  mutate(decile = ntile(tpop, 10)) %>%
  ungroup()
conditional_decile_table <- conditional_decile_table %>%
  pivot_wider(names_from = decile, values_from = tpop)
colnames(conditional_decile_table) <- c("n_agg", paste0("dec", 1:10))
print(conditional_decile_table)


db_tire %>%
  group_by(n_agg) %>%
  mutate(decile = ntile(tpop, 10)) %>%
  ungroup()

result_table <- db_tire %>%
  group_by(n_agg) %>%
  summarize(
    decile_10 = quantile(tpop, 0.1),
    decile_20 = quantile(tpop, 0.2),
    decile_30 = quantile(tpop, 0.3),
    decile_40 = quantile(tpop, 0.4),
    decile_50 = quantile(tpop, 0.5),
    decile_60 = quantile(tpop, 0.6),
    decile_70 = quantile(tpop, 0.7),
    decile_80 = quantile(tpop, 0.8),
    decile_90 = quantile(tpop, 0.9)
  ) %>%
  ungroup()


db_tire %>% 
  mutate(n = ifelse(n >= 8, "8+", n)) %>% 
  ggplot(aes(x = factor(n), y = tpop)) +
  geom_boxplot() + 
  xlab("Número de Revendedores") +
  ylab("População") +
  theme_bw()
