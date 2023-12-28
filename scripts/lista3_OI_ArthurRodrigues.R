library(tidyverse)
library(evd)
library(progress)
library(kableExtra)
library(fixest)
library(dgof)

#### Importanto e tratando base ####

# Lendo e renomeando base

db_estoque <- read.delim("data/Estoques.txt", sep = ",", header = T)

db_estoque <- db_estoque %>% rename(id = X1, t = X1.1, I_t = X0, x_t = X0.1)

# Retirando observações com 0 em todos os períodos

db_estoque <- db_estoque %>% filter(id != 1 & id != 9)

# Calculando a quantidade vendida n*_t

db_estoque <- db_estoque %>% group_by(id) %>% mutate(n_t = I_t + x_t - lead(I_t, default = NA))

#### A demanda depende do estoque? É justo supor iid? ####

ggplot(db_estoque, aes(x=n_t)) +  # Histograma n_t
  geom_histogram(binwidth = 1)+
  theme_bw()+
  labs(title = "Histograma de n*",
       x = "n*",
       y = "Número de observações")

db_estoque %>% ungroup() %>% # Contagem de observações por n_t < = > I_t
  summarise(
    lessthan = sum(n_t < I_t, na.rm = T),
    equal = sum(n_t == I_t, na.rm = T),
    greater = sum(n_t > I_t, na.rm = T)
  )

db_estoque <- db_estoque %>% mutate(I_expan = I_t + x_t) # Criando variável de estoque expandido (I_t + x_t)

db_estoque_restrito <- db_estoque %>%  filter(n_t != I_expan & n_t != I_t)# Criando base restrita esem n* igual ao estoque ou estoque expandido

reg1 <- feols(n_t ~ I_t, data = db_estoque) # Regressão x_t, I_t com base completa
reg2 <- feols(n_t ~ I_t, data = db_estoque_restrito)# Regressão x_t, I_t com base restrita

etable(list(reg1, reg2), tex=TRUE)#Tabela de resultados

db_estoque <- db_estoque %>% group_by(id) %>%  mutate(n_tp1 = dplyr::lead(n_t, default = NA))# Criando a variável de qtdd vendida em t+1


reg3 <- feols(n_tp1 ~ n_t, data = db_estoque %>% filter(n_t < I_expan, n_t != I_t))# Regressão de n*t e n*t+1

etable(reg3, tex=TRUE)#Tabela do resultado

# Realizando teste de Goodness of Fit na base completa

db_estoque_semNA <- db_estoque %>% filter(!is.na(n_t))
observed <- db_estoque_semNA %>% group_by(n_t) %>% summarise(p_t = n() / nrow(db_estoque_semNA))
expected <- rep(1/8, 8)
test1 <- chisq.test(observed, p =expected)

# Realizando teste de Goodness of Fit na base completa

db_estoquerest_semNA <- db_estoque_restrito %>% filter(!is.na(n_t))
observedrest <- db_estoquerest_semNA %>% group_by(n_t) %>% summarise(p_t = n() / nrow(db_estoquerest_semNA))
test2 <- chisq.test(observedrest, p =expected)

# Printando resultados

results1 <- data.frame(
  Test_Statistic = test1$statistic,
  Degrees_of_Freedom = test1$parameter,
  P_Value = test1$p.value
)
kable(results1, format = "latex")

results2 <- data.frame(
  Test_Statistic = test2$statistic,
  Degrees_of_Freedom = test2$parameter,
  P_Value = test2$p.value
)
kable(results2 , format = "latex")


#### What: Estimando x(I_t) e P(n_t+1 | n_t, x_t) ####

# Transition matrix não paramétrico

P <- array(1/13, dim = c(13, 13))

# Gerando matriz de probabilidades condicionais

db_estoque$x_t <- factor(db_estoque$x_t, levels = c(0,1,2,3,4,5,6,7,8,9,10))
contingency_table <- table(db_estoque$I_t, db_estoque$x_t)
observed_conditional_probabilities <- prop.table(contingency_table, margin = 1)

# Plotando heatmap de probabilidades observadas

obs_prob_df <- as.data.frame(as.table(observed_conditional_probabilities))
colnames(obs_prob_df) <- c("Row", "Column", "Probability")

ggplot(obs_prob_df, aes(x = Row, y = Column, fill = Probability)) +
  geom_tile() +
  scale_fill_viridis_c() +  # You can choose a different color palette
  labs(title = "Mapa de Calor de Probabilidades Observadas",
       x = "Estoque (I)",
       y = "Encomendas (x)") +
  theme_minimal()+
  geom_text(size = 3, color = "black", vjust = 1, label = round(obs_prob_df$Probability, 2))

# Usando additive smoothing pra extrapolar a matriz de probabilidades


laplace_contingency_table <- contingency_table+0.1
conditional_probabilities <- prop.table(laplace_contingency_table, margin = 1)
print(conditional_probabilities)

# Definindo policies para até I = 100

original_rows <- nrow(conditional_probabilities)
desired_rows <- 100
new_rows <- conditional_probabilities[rep(original_rows, desired_rows - original_rows), ]
extended_matrix <- rbind(conditional_probabilities, new_rows)


# logit gerando diferenca de valor entre escolhas

v <- log(extended_matrix) - log(extended_matrix[,1])

# Gerando mapa de calor de funções valor

obs_values <- as.data.frame(as.table(head(v,14)))
colnames(obs_values) <- c("Row", "Column", "Value")

obs_values <- reshape2::melt(head(v,14), id.vars = c("row", "col"))


ggplot(obs_values, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mapa de Calor da Função Valor Estimada",
       x = "Estoque (I)",
       y = "Encomendas (x)") +
  theme_minimal()+
  geom_text(size = 3, color = "black", vjust = 1, label = round(obs_values$value, 2))

policy <- function(I, nu, mat){ # Função escolha. Toma um valor de estoque e um choque nu e retorna um valor para x.
  choice <- which.max(as.vector(mat[I+1,]-nu))-1
  return(choice)
}

#### Why: Estimando delta, c e E ####

En <- function(I){ # Valor Esperado de min(n,I) como função
  ifelse(I==0,0,ifelse(I<=8,(I+1)/2,9/2))
}

psi <- function(I, nu, mat){ # Vetor de funções base, do lucro, aproveitando a linearidade
  vec <- c(En(I), - policy(I, nu, mat),- 1*(policy(I, nu, mat)>0), - I^2)
  return(vec)
}

W <- function(I_0, mat) { # Função calculando o W para 100 períodos
  I <- I_0
  beta <- 0.95
  result <- matrix(0, nrow = 100, ncol = 4)  
  
  for (l in 1:100) {# t = 100
    nu <- rgumbel(11)
    result[l, ] <- beta^l * psi(I, nu, mat)
    I <- I + policy(I, nu, mat) - min(sample(1:8, 1),I)
  }
  
  return(colSums(result))  
}

# Criando matrizes v alternativas

add_random_shocks <- function(matrix, magnitude) { # Função que gera choques uniformes aleatórios aleatórios em uma matriz
  rows <- nrow(matrix)
  cols <- ncol(matrix)
  
  random_shocks <- matrix(runif(rows * cols, -magnitude, magnitude), nrow = rows, ncol = cols)
  matrix_with_shocks <- matrix + random_shocks
  
  return(matrix_with_shocks)
}

for (i in 1:10) { # Criando matrizes V de políticas alternativas
  assign(paste0("v_", i), add_random_shocks(v, 2))
}

# Estimando o W da política correta

W_estimado <- function(){# Função que faz o draw dos nus e gera matriz W pros parâmetros verdadeiros
  temp <- matrix(0, nrow = 12, ncol = 4)
  for (i in 0:11) { # Armazenando os Ws estimados
    temp[i+1,] <- W(i, v)
  }
  return(temp)
}

paths_W <- replicate(100, W_estimado(), simplify = FALSE) # Gerando 100 paths (N=100)
avg_W_est <- Reduce("+", paths_W) / length(paths_W) # Tirando a média entre os 100 paths

# Estimando os W's de políticas alternativas

W_prime_estimado <- function(){# Função que faz o draw dos nus e gera 10 matrizes W_prime pros desvios simulados
  matrices <- list()
  
  for (i in 1:10) {
    W_i <- matrix(0, nrow = 12, ncol = 4)
    
    for (j in 0:11) {
      W_i[j+1,] <- W(j, get(paste0("v_", i)))
    }
    
    matrices[[i]] <- W_i
  }
  
  return(matrices)
}


paths_W_prime <- replicate(100, W_prime_estimado(), simplify = FALSE) # Gerando 100 paths

avg_W_prime_est <- list() # Criando lista pra calcular a média
for (i in 1:10) {
  average_matrix <- matrix(0, nrow = 12, ncol = 4) # Matriz de 0s pra armazenar o resultado
  
  for (j in 1:12) {
    for (k in 1:4) {
      average_matrix[j,k] <- mean(sapply(paths_W_prime, function(x) x[[i]][j,k])) # média célula a célula de cada matriz estimada
    }
  }
  
  avg_W_prime_est[[i]] <- average_matrix #adicionando a matriz na lista
}

for (i in 1:10) { # Printa os resultados
  cat(paste0("Average matrix for W_", i, ":\n"))
  print(avg_W_prime_est[[i]])
}



# Criando as 120 condições de equilíbrio

W_final <- do.call(rbind, replicate(10, avg_W_est, simplify = FALSE)) # combinando 10 vezes a política correta
W_prime_final <- do.call(rbind, avg_W_prime_est) # Combinando as políticas alternativas
W_minus_Wprime <- W_final - W_prime_final # subtração célula a célula

# Criando função objetivo para estimação

objective <- function(params){
  ml <- rep(0,120)
  for (i in 1:120) {
    ml[i] <- min(c(10,params) %*% W_minus_Wprime[i,],0)^2
  }
  l = sum(ml)
  return(l)
}

# Estimando os parâmetros e gerando tabela

solution <- optim(c(2,10,1), objective, method = "BFGS")
print(solution$par)

#### Value Function Iteration ####

# Definindo parâmetros novos e antigos para teste

old_pars <- c(10, 1.73, 19, 0.12)
new_pars <- c(11, 1.73, 19, 0.12)

# Espaços de controle e estado

I_space <- 0:13
x_space <- 0:10

# Criando vetor de chutes iniciais para a V(I)

initial_V <- rep(1,14)
V <- initial_V
V_p10 <- initial_V
beta <- 0.95

# Criando a função lucro

profit <- function(params, I, x){
  params[1]*En(I) - params[2]*x - params[3]*1*(x>0) - params[4]*I^2
}

# Criando o valor esperado em t+1

EV <- function(I, x, V){
  (1/8)*V[min(I+x-min(1,I),12)+1] + (1/8)*V[min(I+x-min(2,I),12)+1]  +
    (1/8)*V[min(I+x-min(3,I),12)+1]  + (1/8)*V[min(I+x-min(4,I),12)+1]  +
    (1/8)*V[min(I+x-min(5,I),12)+1]  + (1/8)*V[min(I+x-min(6,I),12)+1]  +
    (1/8)*V[min(I+x-min(7,I),12)+1]  + (1/8)*V[min(I+x-min(8,I),12)+1] 
}

# Equação de Bellman

Bellman <- function(params, I, V){
  Q <- rep(0, length(x_space))
  for (j in 1:length(x_space)) {
    Q[j] <- profit(params, I, j-1) + beta*EV(I,j-1,V)
  }
  return(which.max(Q))
}

# Definindo parâmetros pra convergência

tolerance <- 1e-04
diff <- Inf

# Testando convergência com p=11

while (diff > tolerance) {
  V_new <- rep(0, nrow = length(I_space))
  for (i in 1:length(I_space)) {
    V_new[i] <- profit(new_pars, i, Bellman(new_pars, i, V)-1) + beta*EV(i, Bellman(new_pars, i, V)-1, V)
  }
  diff <- max(abs(V_new - V))
  V <- V_new
}

for (k in 0:12) {
  print(Bellman(new_pars, k, V)-1) 
}

# Testando convergência com p=10

diff <- Inf
V_p10
while (diff > tolerance) {
  V10_new <- rep(0, nrow = length(I_space))
  for (i in 1:length(I_space)) {
    V10_new[i] <- profit(old_pars, i, Bellman(old_pars, i, V_p10)-1) + beta*EV(i, Bellman(old_pars, i, V_p10)-1, V_p10)
  }
  diff <- max(abs(V10_new - V_p10))
  V_p10 <- V10_new
}


for (k in 0:12) {
  print(Bellman(old_pars, k, V_p10)-1) 
}
