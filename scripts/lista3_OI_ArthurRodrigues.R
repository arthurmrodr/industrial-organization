library(tidyverse)
library(evd)
set.seed(1)

#### Análise Exploratória ####

# Lendo e renomeando base

db_estoque <- read.delim("data/Estoques.txt", sep = ",", header = T)

db_estoque <- db_estoque %>% rename(id = X1, t = X1.1, I_t = X0, x_t = X0.1)

# Retirando observações com 0 em todos os períodos

db_estoque <- db_estoque %>% filter(id != 1 & id != 9)

# Calculando a quantidade vendida n*_t

db_estoque <- db_estoque %>% group_by(id) %>% mutate(n_t = I_t + x_t - lead(I_t, default = 0))

# A demanda depende do estoque?

ggplot(db_estoque, aes(x=n_t, y=I_t)) + 
  geom_point()

lm(n_t ~ I_t, data = db_estoque)

ggplot(db_estoque, aes(x=n_t, y=lead(n_t))) + 
  geom_point()

# As orders dependem do estoque?

ggplot(db_estoque, aes(x=I_t, y=x_t)) + 
  geom_point()


#### What: Estimando x(I_t) e P(n_t+1 | n_t, x_t) ####

# Transition matrix não paramétrico

P <- array(1/13, dim = c(13, 13))

# Gerando matriz de probabilidades condicionais

db_estoque$x_t <- factor(db_estoque$x_t, levels = c(0,1,2,3,4,5,6,7,8,9,10))
contingency_table <- table(db_estoque$I_t, db_estoque$x_t) 
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

# Gerando vetor de choques Gumbel

nus <- rgumbel(ncol(v))

# Criando função de escolha

policy <- function(I, nu){
  choice <- as.numeric(names(which.max(v[I,]-nu)))
  return(choice)
}


#### Why: estimando os parâmtros c, E e delta ####

# Criando psi

psi <- function(I, nu){
  vec <- c(min(I, 4.7), - policy(I, nu),- 1*(policy(I, nu)>0), - I^2)
  return(vec)
}

# Criando W para 100 períodos

beta <- 0.95


W <- function(I_0) {
  I <- I_0
  beta <- 0.95
  result <- matrix(0, nrow = 100, ncol = 4)  # Initialize a matrix with four columns
  
  for (l in 1:100) {
    nu <- rgumbel(11)
    result[l, ] <- beta^l * psi(I, nu)
    I <- I + policy(I, nu) - sample(1:13, 1)
  }
  
  return(colSums(result))  # Return a vector by summing across columns
}

# Criando função que adiciona choques na matriz

add_random_shocks <- function(matrix, magnitude) {
  # Get the dimensions of the matrix
  rows <- nrow(matrix)
  cols <- ncol(matrix)
  
  # Generate random shocks and add them to the original matrix
  random_shocks <- matrix(runif(rows * cols, -magnitude, magnitude), nrow = rows, ncol = cols)
  matrix_with_shocks <- matrix + random_shocks
  
  return(matrix_with_shocks)
}

# Criando políticas alternativas

W_prime <- function(I_0, magnitude){
  alternative_v <- add_random_shocks(v, magnitude)
  
  # Criando função de escolha alternativa
  
  alternative_policy <- function(I, nu){
    choice <- as.numeric(names(which.max(alternative_v[I,]-nu)))
    return(choice)
  }
  
  # Criando psi alternativo
  
  alternative_psi <- function(I, nu){
    vec <- c(min(I, 4.7), - alternative_policy(I, nu),- 1*(alternative_policy(I, nu)>0), - I^2)
    return(vec)
  }
  
  I <- I_0
  beta <- 0.95
  result <- matrix(0, nrow = 100, ncol = 4)  # Initialize a matrix with four columns
  
  for (k in 1:100) {
    nu <- rgumbel(11)
    result[k, ] <- beta^k * alternative_psi(I, nu)
    I <- I + alternative_policy(I, nu) - sample(1:13, 1)
  }
  
  return(colSums(result))  # Return a vector by summing across columns
  
}

W_prime(50,0.5)

# Criando "verossimilhança" para estimação

like <- function(params){
  
  ml <- matrix(0, nrow = 13, ncol = 10)
  
  for (j in 1:10) {
      for (i in 1:13) {
        ml[i,j] <-(min(c(1,params) %*% (W(i) - W_prime(i, j/5)),0))^2
      }    
  }

  l = sum(ml)
  return(l)
}

like(c(1,1,1))

solution <- optim(c(1,1,1), like, method = "BFGS", hessian = TRUE)
se <- sqrt(diag(solve(solution$hessian)))
res <- cbind(solution$par, se)

