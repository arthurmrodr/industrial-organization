library(tidyverse)
library(AER)
library(BLPestimatoR)

#### Base de Dados ####

# Baixando Dados

db_fipe <- read.csv("data/fipefenabrave.csv", header = F)
db_domic <- read.csv("data/domicilios.csv", sep = ";")

# Renomeando variáveis

db_fipe <- db_fipe %>%  
  rename(ano_ref = V1,   
         marca = V2,
         modelo = V3,
         descmodelo = V4,
         preco = V5,
         quantidade = V6,
         combustivel = V7,
         cc = V8,
         ipi = V9)

# Criando dummies combustível

db_fipe <- db_fipe %>% mutate(flex = ifelse(combustivel == "Flex", 1, 0), 
                              gasolina = ifelse(combustivel == "Gasolina", 1, 0),
                              alcool = ifelse(combustivel == "Álcool", 1, 0),
                              diesel = ifelse(combustivel == "Diesel", 1, 0))

# Agregação

db_fipeagreg <- db_fipe %>% group_by(ano_ref, modelo) %>% summarise(preco_media = mean(preco),
                                                           qtdd_total = sum(quantidade),
                                                           cc_media = mean(cc),
                                                           cc_max = max(cc),
                                                           cc_min = min(cc),
                                                           alcool = max(alcool),
                                                           diesel = max(diesel),
                                                           gasolina = max(gasolina),
                                                           flex = max(flex),
                                                           ipi = mean(ipi),
                                                           combustivel = mode(combustivel))

# Adicionando modelos

lista_modelos <- db_fipe %>% select(modelo, marca) %>% distinct(modelo, marca)

db_fipeagreg <- left_join(db_fipeagreg, lista_modelos)

# Criando o market share

db_fipeagreg <- left_join(db_fipeagreg, db_domic)
db_fipeagreg <- db_fipeagreg %>% mutate(mkt_sh = qtdd_total / (tot_domic * 1000))

db_fipe <- left_join(db_fipe, db_domic)
db_fipe <- db_fipe %>% mutate(mkt_sh = quantidade / (tot_domic * 1000))

# Modelando o outside good

db_fipeagreg <- db_fipeagreg %>% group_by(ano_ref) %>% mutate(qtdd_out = (tot_domic * 1000) - sum(qtdd_total)) %>% mutate(out_mktsh = qtdd_out / (tot_domic * 1000))
db_fipe <- db_fipe %>% group_by(ano_ref) %>% mutate(qtdd_out = (tot_domic * 1000) - sum(quantidade)) %>% mutate(out_mktsh = qtdd_out / (tot_domic * 1000))


#### Estimação Logit ####

# Criando a variável dependente

db_fipeagreg <- db_fipeagreg %>% mutate(y = log(mkt_sh) - log(out_mktsh))
db_fipe <- db_fipe %>% mutate(y = log(mkt_sh) - log(out_mktsh))


# Rodando modelo agregado

logit <- lm(y ~ preco_media + cc_media + alcool + diesel + gasolina + cc_max + cc_min + marca + as.character(ano_ref), data = db_fipeagreg)
summary(logit)

# Rodando modelo desagregado

logit2 <- lm(y ~ preco + cc + combustivel , data = db_fipe)
summary(logit2)

#### Estimação em IV ####

# IV completo

iv_agregado <- ivreg(y~preco_media+gasolina+diesel+flex+cc_media | ipi+gasolina+diesel+flex+cc_media, data = db_fipeagreg)
summary(iv_agregado)


iv_desagregado <- ivreg(y~preco+gasolina+diesel+flex+cc | ipi+gasolina+diesel+flex+cc, data = db_fipe)
summary(iv_desagregado)

# 1º estágio

fs_agregado <- lm(preco_media~ipi+gasolina+diesel+cc_media, data = db_fipeagreg)
summary(fs_agregado)

fs_desagregado <- lm(preco~ipi+gasolina+diesel+cc, data = db_fipe)
summary(fs_desagregado)


#### Elasticidades Logit ####



#### Custo Marginal Logit ####

#### BLP ####

J <- nrow(db_fipeagreg)

Z <- matrix(data = 0, nrow = J, ncol = 6)
for (j in 1:J) {
  
  for (r in 1:J) {
    
    if (db_fipeagreg$modelo[j] == db_fipeagreg$modelo[r]) {
      next
    }
    else if(db_fipeagreg$marca[j] == db_fipeagreg$marca[r]) {
      Z[j, 1] <- Z[j, 1] + (db_fipeagreg$combustivel[r] == "Flex")
      Z[j, 2] <- Z[j, 2] + (db_fipeagreg$combustivel[r] == "Gasolina")
      Z[j, 3] <- Z[j, 3] + db_fipeagreg$cc_media[r]
    } else if(db_fipeagreg$marca[j] != db_fipeagreg$marca[r]) {
      Z[j, 4] <- Z[j, 4] + (db_fipeagreg$combustivel[r] == "Flex")
      Z[j, 5] <- Z[j, 5] + (db_fipeagreg$combustivel[r] == "Gasolina")
      Z[j, 6] <- Z[j, 6] + db_fipeagreg$cc_media[r]
    }
    
  }
}

df_blp <- db_fipeagreg %>% cbind(Z %>% as.data.frame())

model <- as.formula("y ~ preco_media + combustivel + cc_media |
    0 + combustivel + cc_media |
    preco_media + combustivel + cc_media |
    0 + ipi + V1 + V2 + V3 + V4 + V5 + V6")


blp_data <- BLP_data(model, 
                     market_identifier = "ano_ref",
                     product_identifier = "modelo",
                     productData = df_blp,
                     integration_method = "MLHS",
                     integration_seed = 1,
                     integration_accuracy = 40)

blp_estimate <- estimateBLP(blp_data,
                            solver_method = "BFGS", 
                            solver_maxit = 1000, 
                            solver_reltol = 1e-6,
                            standardError = "homoskedastic",
                            printLevel = 1)

(blp_summ <- summary(blp_estimate))

