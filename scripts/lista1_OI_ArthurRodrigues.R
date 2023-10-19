library(tidyverse)
library(AER)
library(BLPestimatoR)
library(knitr)
library(plotly)
library(kableExtra)
library(fixest)
library(deflateBR)
library(modelsummary)
library(stargazer)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}

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

# Extraindo variáveis da descrição do modelo

db_fipe$portas = str_extract(db_fipe$descmodelo, "\\d+(?=p)") %>% as.numeric()
db_fipe$cambio = str_extract(db_fipe$descmodelo,'Aut|Mec')
db_fipe$motor = str_extract(db_fipe$descmodelo, "\\d+\\.\\d+") %>% as.numeric()
db_fipe$valvulas = str_extract(db_fipe$descmodelo, "\\d+(?=[vV])") %>% as.numeric()
db_fipe$cavalos = str_extract(db_fipe$descmodelo, "\\d+(?=cv)") %>% as.numeric()


# Criando dummies combustível

db_fipe <- db_fipe %>% mutate(flex = ifelse(combustivel == "Flex", 1, 0), 
                              gasolina = ifelse(combustivel == "Gasolina", 1, 0),
                              alcool = ifelse(combustivel == "Álcool", 1, 0),
                              diesel = ifelse(combustivel == "Diesel", 1, 0))

# Agregação

db_fipeagreg <- db_fipe %>% group_by(ano_ref, modelo) %>% summarise(preco_media = mean(preco),
                                                           qtdd_total = sum(quantidade),
                                                           cc_media = mean(cc, na.rm = T),
                                                           cc_max = max(cc),
                                                           cc_min = min(cc),
                                                           alcool = max(alcool),
                                                           diesel = max(diesel),
                                                           gasolina = max(gasolina),
                                                           flex = max(flex),
                                                           ipi = mean(ipi),
                                                           combustivel = getmode(combustivel),
                                                           portas = median(portas, na.rm = T),
                                                           cambio = getmode(cambio),
                                                           valvulas = mean(valvulas, na.rm = T),
                                                           cavalos = mean(cavalos, na.rm = T),
                                                           cc_mediana = median(cc, na.rm = T))

# Adicionando marcas

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

tabela1 <- db_fipeagreg %>% group_by(ano_ref) %>% summarise(out_good = mean(qtdd_out), out_mktsh = mean(out_mktsh))

# Deflacionando

db_fipeagreg$preco_media_def <- deflate(db_fipeagreg$preco_media, as.Date(paste(db_fipeagreg$ano_ref, 12, 01, sep = "-")), "12/2016", index = "ipc")
db_fipe$preco_media_def <- deflate(db_fipe$preco, as.Date(paste(db_fipe$ano_ref, 12, 01, sep = "-")), "12/2016", index = "ipc")

# Visualização de dados 

#### Estimação Logit ####

# Criando a variável dependente

db_fipeagreg <- db_fipeagreg %>% mutate(y = log(mkt_sh) - log(out_mktsh))
db_fipe <- db_fipe %>% mutate(y = log(mkt_sh) - log(out_mktsh))


# Rodando modelo agregado

logit1 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex , fixef = c("ano","marca"), data = db_fipeagreg)
logit2 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex,fixef = c("marca"), data = db_fipeagreg)
logit3 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex  + cambio, fixef = c("ano","marca"), data = db_fipeagreg)
logit4 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex  + cambio + valvulas + portas, fixef = c("ano","marca"), data = db_fipeagreg)

etable(list(logit1, logit2, logit3, logit4), tex=TRUE)


# Rodando modelo desagregado

logitdes1 <- feols(y ~ preco + cc + alcool + diesel + flex , fixef = c("ano","marca"), data = db_fipe)
logitdes2 <- feols(y ~ preco + cc + alcool + diesel + flex,fixef = c("marca"), data = db_fipe)
logitdes3 <- feols(y ~ preco + cc + alcool + diesel + flex + cambio, fixef = c("ano","marca"), data = db_fipe)
logitdes4 <- feols(y ~ preco + cc + alcool + diesel + flex + cambio + valvulas + portas, fixef = c("ano","marca"), data = db_fipe)

etable(list(logitdes1, logitdes2, logitdes3, logitdes4), tex=TRUE)

# Rodando modelo restrito

db_fiperest = db_fipeagreg %>% group_by(ano_ref) %>%
  arrange(ano_ref, desc(qtdd_total)) %>%
  slice(1:35) %>% ungroup()

logitrest1 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex , fixef = c("ano","marca"), data = db_fiperest)
logitrest2 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex , fixef = c("marca"), data = db_fiperest)
logitrest3 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex + cambio, fixef = c("ano","marca"), data = db_fiperest)
logitrest4 <- feols(y ~ preco_media + cc_mediana + alcool + diesel + flex + cambio + valvulas + portas, fixef = c("ano","marca"), data = db_fiperest)

etable(list(logitrest1, logitrest2, logitrest3, logitrest4), tex=TRUE)


#### Estimação em IV ####

# IV completo

iv_agregado1 <- feols(y ~ alcool + diesel + flex + cc_mediana  | 
                         ano_ref + marca | preco_media ~ ipi, data = db_fipeagreg, se = "standard")
iv_agregado2 <- feols(y ~ alcool + diesel + flex + cc_mediana  | 
                        ano_ref | preco_media ~ ipi, data = db_fipeagreg, se = "standard")
iv_agregado3 <- feols(y ~ alcool + diesel + flex + cc_mediana + cambio | 
                        ano_ref + marca | preco_media ~ ipi, data = db_fipeagreg, se = "standard")
iv_agregado4 <- feols(y ~ alcool + diesel + flex + cc_mediana + cambio + portas + valvulas | 
                        ano_ref + marca | preco_media ~ ipi, data = db_fipeagreg, se = "standard")


etable(list(iv_agregado1, iv_agregado2, iv_agregado3, iv_agregado4), tex=TRUE)

# 1º estágio


etable(list(summary(iv_agregado1,stage = 1), summary(iv_agregado2,stage = 1), summary(iv_agregado3,stage = 1), summary(iv_agregado1,stage = 1)), tex=TRUE)


#### Elasticidades Logit ####

alpha <- -as.numeric(iv_agregado1$coefficients["fit_preco_media"])

elastgol_logit <- db_fipeagreg %>% filter(ano_ref == 2016) %>% mutate(elasticidade = ifelse(modelo == "GOL", -alpha * preco_media * (1-mkt_sh), alpha * preco_media * mkt_sh))
elastgol_logit <- elastgol_logit %>% ungroup()

elastgol_logit %>% select(marca, modelo, qtdd_total, mkt_sh, elasticidade) %>% kable(format = "latex")

p <- elastgol_logit %>% filter(modelo != "GOL") %>% ggplot(aes(x = qtdd_total, y = elasticidade, color = marca, label = modelo)) + 
  ggtitle("Elasticidade e Quantidade Vendida 2016 (Logit IV)") + geom_point() + theme_bw()+ xlab("Quantidade") + ylab("Elasticidade")

p
ggplotly(p)

#### Custo Marginal Logit ####


J <- nrow(elastgol_logit) #número de carros

p <- matrix(elastgol_logit$preco_media, nrow = J) #vetor de preços
s <- matrix(elastgol_logit$mkt_sh, nrow = J) #vetor de market shares

Delta <- matrix(nrow = J, ncol = J) #criando a matriz delta
for (j in 1:J) {
  
  for (r in 1:J) {
    
    if (j == r) {
      Delta[j, r] <- alpha * s[j] * (1 - s[j])
    }
    else if(elastgol_logit$marca[j] != elastgol_logit$marca[r]) {
      Delta[j, r] <- 0
    } else {
      Delta[j, r] <- -alpha * s[r] * s[j]
    }
    
  }
}

cmg <- c(p - solve(Delta) %*% s)#vetor custo marginal (a partir da inversa de Delta)
custos_logit <- elastgol_logit %>% cbind("cmg" = cmg)#criando dataframe com custos marginais
custos_logit <- custos_logit %>% mutate(markup = (preco_media/cmg)-1)

custos_logit %>% select(marca, modelo, preco_media, mkt_sh, cmg, markup) %>% kable(format = "latex")



#### BLP ####

# Criando instrumentos

J <- nrow(db_fipeagreg)

Z <- matrix(data = 0, nrow = J, ncol = 6) # matriz de instrumentos a partir de BLP95
for (j in 1:J) {
  
  for (r in 1:J) {
    
    if (db_fipeagreg$modelo[j] == db_fipeagreg$modelo[r]) {
      next
    }
    else if(db_fipeagreg$marca[j] == db_fipeagreg$marca[r]) {
      Z[j, 1] <- Z[j, 1] + (db_fipeagreg$combustivel[r] == "Flex")
      Z[j, 2] <- Z[j, 2] + (db_fipeagreg$combustivel[r] == "Gasolina")
      Z[j, 3] <- sum(Z[j, 3], db_fipeagreg$cc_media[r], na.rm = TRUE)
    } else if(db_fipeagreg$marca[j] != db_fipeagreg$marca[r]) {
      Z[j, 4] <- Z[j, 4] + (db_fipeagreg$combustivel[r] == "Flex")
      Z[j, 5] <- Z[j, 5] + (db_fipeagreg$combustivel[r] == "Gasolina")
      Z[j, 6] <- sum(Z[j, 6], db_fipeagreg$cc_mediana[r], na.rm = TRUE)
    }
    
  }
}

# Especificando o modelo

db_blp <- db_fipeagreg %>% cbind(Z %>% as.data.frame())

          
model <- as.formula("mkt_sh ~ preco_media + combustivel + cc_mediana |
    0 + combustivel + cc_mediana |
    preco_media + combustivel + cc_mediana |
    0 + ipi + V1 + V2 + V3 + V4 + V5 + V6") # 1. linear variables, 2. exog variables, 3. random coefs, 4. instruments


blp_data <- BLP_data(model, 
                     market_identifier = "ano_ref",
                     product_identifier = "modelo",
                     productData = db_blp,
                     integration_method = "MLHS",
                     integration_seed = 8,
                     integration_accuracy = 50)

blp_estimate <- estimateBLP(blp_data,
                            solver_method = "BFGS", 
                            solver_maxit = 1000, 
                            solver_reltol = 1e-9,
                            standardError = "homoskedastic",
                            printLevel = 1)

blp_coefficients <- summary(blp_estimate)

# Tabelas

stargazer(blp_coefficients$LinCoefficients, summary = F, out = "tex")
stargazer(blp_coefficients$RcCoefficients, summary = F, out = "tex")



#### Custo e Elasticidades BLP ####

# Extraindo parâmetros estimados do BLP

db_blp_results <- update_BLP_data(data_update = db_blp, blp_data = blp_data)

theta2 <- as.matrix(blp_coefficients$RcCoefficients$Estimate)
rownames(theta2) <- c("(Intercept)", "preco_media", "combustivelFlex", "combustivelGasolina", "cc_mediana")
colnames(theta2) <- c("unobs_sd")

mkt_sh_novo = getShareInfo(blp_data = db_blp_results, par_theta2 = theta2)


blp_elasticidades <- get_elasticities(blp_data = db_blp_results, 
                                     share_info = mkt_sh_novo,
                                     theta_lin = blp_coefficients$LinCoefficients['preco_media','Estimate'],
                                     variable = "preco_media",
                                     market = "2016")

blp_elasticidade_GOL <- blp_elasticidades['GOL',]
blp_elasticidade_GOL <- cbind(db_blp[db_blp$ano_ref==2016,], blp_elasticidade_GOL)
names(blp_elasticidade_GOL)[32] <- 'elasticidade'

# Tabela elasticidades

blp_elasticidade_GOL %>% select(marca, modelo, mkt_sh, preco_media, elasticidade) %>% kable(format = "latex")

# Gráfico elasticidades

 ggplot(data = blp_elasticidade_GOL[blp_elasticidade_GOL$modelo!='GOL',],
       aes(x = qtdd_total, y = elasticidade, color = marca)) + geom_point() + theme_bw() + ggtitle("Elasticidade e Quantidade Vendida 2016 (BLP)")+ xlab("Quantidade") + ylab("Elasticidade")

# Custos
 
 preco = blp_elasticidade_GOL$preco_media %>% as.matrix()
 J = nrow(preco)
 market_share = blp_elasticidade_GOL$mkt_sh %>% as.matrix()
 
 Delta_BLP <- matrix(nrow = J, ncol = J)
 
 for (j in 1:J) {
   for (r in 1:J) {
     if(blp_elasticidade_GOL$marca[j] != blp_elasticidade_GOL$marca[r]) {
       Delta_BLP[j, r] = 0;
     } else {
       Delta_BLP[j, r] = blp_elasticidades[j,r] * -market_share[j]/preco[r];
     }
   }
 }
 
 cmg_BLP <- c(preco - solve(Delta_BLP) %*% market_share)
 cmg_BLP <- blp_elasticidade_GOL %>% cbind("cmg_BLP" = cmg_BLP)

# Markup 
 cmg_BLP <- cmg_BLP %>% mutate(markup = (preco_media-cmg_BLP)/cmg_BLP)
 
 
# Tabela Custos e Markups
 
 cmg_BLP %>% select( marca, modelo, qtdd_total, mkt_sh, preco_media, cmg_BLP, markup) %>% kable(format = "latex")
 
 