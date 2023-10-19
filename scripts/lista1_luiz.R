# lista 1 Organização Industrial


##### QUESTAO 1 #####
# precode
wd = 'C:/Users/luizb/Desktop/Organização Industrial/Lista 1/';
setwd(wd);


#library
library(tidyverse);
library(dplyr);
library(stringr);


# importing
col_names = c('ano_ref','marca','modelo','descmodelo','preco','quantidade',
              'combustivel','cc','ipi');
data = read.csv('fipefenabrave.csv', header = F, col.names = col_names);


# adding new description vars
  # portas
data$portas = str_extract(data$descmodelo, "\\d+(?=p)") %>% as.numeric();
  # cambio
data$cambio = str_extract(data$descmodelo,'Aut|Mec');
  # motor
data$motor = str_extract(data$descmodelo, "\\d+\\.\\d+") %>% as.numeric();
  # valvulas
data$valvulas = str_extract(data$descmodelo, "\\d+(?=[vV])") %>% as.numeric();
  # cavalos
data$cavalos = str_extract(data$descmodelo, "\\d+(?=cv)") %>% as.numeric();


# mode function
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# dataset with representative models

data = data %>% 
  group_by(ano_ref, marca, modelo) %>% 
  summarise(preco = median(preco),
            quantidade = sum(quantidade),
            combustivel = get_mode(combustivel),
            cc = median(cc, na.rm = T),
            ipi = mean(ipi, na.rm = T),
            portas = median(portas, na.rm = T),
            cambio = get_mode(cambio),
            motor = median(motor, na.rm = T),
            valvulas = median(valvulas, na.rm =T),
            cavalos = median(cavalos, na.rm = T));

#### QUESTAO 2 ####

# keeping only the 29 most sold models from each year

  # using question 1's model with modified quantities (representative model)
sub_data = data %>% group_by(ano_ref) %>%
  arrange(ano_ref, desc(quantidade)) %>%
  slice(1:29) %>% ungroup();


#### QUESTAO 3 ####


# library
library(readxl);

# household data
domicilios = read.csv('domicilios.csv');

# domicilios = read.csv('domicilios2.csv', sep = ';')[3:16,]
names(domicilios) = c('ano_ref','domicilios');
# domicilios$domicilios = domicilios$domicilios*1000


# yearly sum of all cars sold
for (i in 2003:2016) {
  data$total_vendido[data$ano_ref==i] = sum(data$quantidade[data$ano_ref==i]);
}

# merging with data
data = merge(data,domicilios, by ='ano_ref');
  # market share of cars in each year
data$share = data$quantidade/data$domicilios;
  # outside good "share" in each year
data$outside_share = (data$domicilios-data$total_vendido)/data$domicilios;
  # log difference
data$y = log(data$share)-log(data$outside_share);

# subsample of 29 yearly most sold models
sub_data = data %>% group_by(ano_ref) %>%
  arrange(ano_ref, desc(quantidade)) %>%
  slice(1:29) %>% ungroup();



#### QUESTAO 4 ####

#library
library(fixest);
library(modelsummary);
library(knitr);

# fixed effects
data$ano_ref = factor(data$ano_ref);
data$marca = factor(data$marca);

# logit for full sample

logit1.1 = feols(y ~ preco + combustivel + cc + portas + valvulas | 
                   ano_ref + marca, data = data, se = "standard");

logit1.2 = feols(y ~ preco + combustivel + cc + portas| 
                   ano_ref + marca, data = data, se = "standard");

logit1.3 = feols(y ~ preco + combustivel + cc | 
                   ano_ref + marca, data = data, se = "standard");




table_1 = modelsummary(list(logit1.1, logit1.2, logit1.3),
                       gof_omit = 'DF|Deviance|Log.Lik.|R.*|AIC|BIC|Std.*|FE.*',
                       title = "50 modelos mais vendidos",
                       stars = T,
                       fmt = 5,
                       notes = 'Fixed effects: ano_ref, marca'
                       ) %>% kable(format = 'latex');

cat(table_1, file = 'table_1.tex');

# logit for subsample

logit2.1 = feols(y ~ preco + combustivel + cc + portas + valvulas | 
                   ano_ref + marca, data = sub_data, se = "standard");

logit2.2 = feols(y ~ preco + combustivel + cc + portas| 
                   ano_ref + marca, data = sub_data, se = "standard");

logit2.3 = feols(y ~ preco + combustivel + cc | 
                   ano_ref + marca, data = sub_data, se = "standard");




table_2 = modelsummary(list(logit2.1, logit2.2, logit2.3),
                       gof_omit = 'DF|Deviance|Log.Lik.|R.*|AIC|BIC|Std.*|FE.*',
                       title = "29 modelos mais vendidos",
                       stars = T,
                       fmt = 5,
                       notes = 'Fixed effects: ano_ref, marca'
) %>% kable(format = 'latex');



cat(table_2, file = 'table_2.tex');


#### QUESTAO 5 ####

# iv regressions
  
  # full sample

second_stage1.1 = feols(y ~ combustivel + cc + portas + valvulas | 
                        ano_ref + marca | preco ~ ipi, data = data, se = "standard");

second_stage1.2 = feols(y ~ combustivel + cc + portas | 
                        ano_ref + marca | preco ~ ipi, data = data, se = "standard");

second_stage1.3 = feols(y ~  combustivel + cc | 
                          ano_ref + marca | (preco) ~ (ipi), data = data, se = "standard");



  # subsample

second_stage2.1 = feols(y ~ combustivel + cc + portas + valvulas | 
                          ano_ref + marca | preco ~ ipi, data = sub_data, se = "standard");

second_stage2.2 = feols(y ~ combustivel + cc + portas | 
                          ano_ref + marca | preco ~ ipi, data = sub_data, se = "standard");

second_stage2.3 = feols(y ~ combustivel + cc | 
                          ano_ref + marca | preco ~ ipi, data = sub_data, se = "standard");


# first stage tables
table_3 = modelsummary(list(summary(second_stage1.1, stage = 1),
                  summary(second_stage1.2, stage = 1),
                  summary(second_stage1.3, stage = 1)),
             gof_omit = 'DF|Deviance|Log.Lik.|R.*|AIC|BIC|Std.*|FE.*',
             title = "50 modelos mais vendidos",
             stars = T,
             fmt = 3,
             notes = 'Fixed effects: ano_ref, marca'
) %>% kable(format = 'latex');

cat(table_3, file = 'table_3.tex');


table_annex_1 =  modelsummary(list(summary(second_stage2.1, stage = 1),
                                   summary(second_stage2.2, stage = 1),
                                   summary(second_stage2.3, stage = 1)),
                              gof_omit = 'DF|Deviance|Log.Lik.|R.*|AIC|BIC|Std.*|FE.*',
                              title = "29 modelos mais vendidos",
                              stars = T,
                              fmt = 3,
                              notes = 'Fixed effects: ano_ref, marca'
) %>% kable(format = 'latex');

cat(table_annex_1, file = 'table_annex_1.tex');



# second stage tables

table_4 = modelsummary(list(summary(second_stage1.1, stage = 2),
                  summary(second_stage1.2, stage = 2),
                  summary(second_stage1.3, stage = 2)),
             gof_omit = 'DF|Deviance|Log.Lik.|R.*|AIC|BIC|Std.*|FE.*',
             title = "50 modelos mais vendidos",
             stars = T,
             fmt = 5,
             notes = 'Fixed effects: ano_ref, marca'
) %>% kable(format = 'latex');

cat(table_4, file = 'table_4.tex');

table_annex_2 = modelsummary(list(summary(second_stage2.1, stage = 2),
                  summary(second_stage2.2, stage = 2),
                  summary(second_stage2.3, stage = 2)),
             gof_omit = 'DF|Deviance|Log.Lik.|R.*|AIC|BIC|Std.*|FE.*',
             title = "50 modelos mais vendidos",
             stars = T,
             fmt = 5,
             notes = 'Fixed effects: ano_ref, marca'
) %>% kable(format = 'latex');

cat(table_annex_2, file = 'table_annex_2.tex');


#### QUESTAO 6 ####

# library
library(xtable);


# alpha

alpha = -as.numeric(second_stage1.3$coefficients['fit_preco'])

# elasticity
elasticidade_2009 = data[data$ano_ref==2009,] %>%
  mutate(elasticidade = ifelse(modelo == 'GOL',
                               -alpha * preco * (1 - share),
                               alpha * preco * share));


# elasticities table
table_elasticities = elasticidade_2009 %>% 
  select(marca, modelo, share, preco, elasticidade) %>% 
  xtable(digits = c(0,0, 0, 4, 0, 4));


# elasticities x quantity graph
ggplot(data = elasticidade_2009[elasticidade_2009$modelo!='GOL',],
       aes(x = quantidade, y = elasticidade)) + geom_point(); 
ggsave(file = 'figura_1.pdf');
  

#### QUESTAO 7 ####

preco = elasticidade_2009$preco %>% as.matrix();
J = nrow(preco);
share = elasticidade_2009$share %>% as.matrix();

Omega = matrix(nrow = J, ncol = J);

for (j in 1:J) {
  
  for (r in 1:J) {
    
    if (j == r) {
      Omega[j, r] = alpha * share[j] * (1 - share[j]);
    }
    else if(elasticidade_2009$marca[j] != elasticidade_2009$marca[r]) {
      Omega[j, r] = 0;
    } else {
      Omega[j, r] = -alpha * share[r] * share[j];
    }
  }
}

# marginal costs
custo_marginal = preco - solve(Omega) %*% share %>% as.data.frame();

# combining with data
elasticidade_2009$custo_marginal = custo_marginal$V1;

custo_marginal = select(elasticidade_2009, marca, modelo, share,
                        preco, custo_marginal) %>%
  mutate(markup = (preco-custo_marginal)/custo_marginal) %>%
  xtable(digits = c(0,0,0,4,0,0,3));
  


#### QUESTAO 8 ####
library(BLPestimatoR);
library(stargazer);

  
df = data;
J = nrow(df);


# creating IV matrix, with sum of other vehicules characteristics
Z <- matrix(data = 0, nrow = J, ncol = 6)
for (j in 1:J) {

for (r in 1:J) {
  if (df$modelo[j] == df$modelo[r]) {
    next
    }
  # discriminating by brand
  else if(df$marca[j] == df$marca[r]) {
    Z[j, 1] <- Z[j, 1] + (df$combustivel[r] == "Flex")
    Z[j, 2] <- Z[j, 2] + (df$combustivel[r] == "Gasolina")
    Z[j, 3] <- Z[j, 3] + df$cc[r]
    } 
  else if(df$marca[j] != df$marca[r]) {
    Z[j, 4] <- Z[j, 4] + (df$combustivel[r] == "Flex")
    Z[j, 5] <- Z[j, 5] + (df$combustivel[r] == "Gasolina")
    Z[j, 6] <- Z[j, 6] + df$cc[r]
    }
  }
}
df_blp = df %>% cbind(Z %>% as.data.frame());


##### version discriminating by year
df = data;
J = nrow(df);


# creating IV matrix, with sum of other vehicules characteristics
Z2 <- matrix(data = 0, nrow = J, ncol = 6)
for (j in 1:J) {
  for (r in 1:J) {
    # discriminating by year
    if(df$ano_ref[j] == df$ano_ref[r]){
      if (df$modelo[j] == df$modelo[r]) {
        next;
      }
      
      # discriminating by brand
      else if(df$marca[j] == df$marca[r]) {
        Z2[j, 1] <- Z2[j, 1] + (df$combustivel[r] == "Flex");
        Z2[j, 2] <- Z2[j, 2] + (df$combustivel[r] == "Gasolina");
        Z2[j, 3] <- Z2[j, 3] + df$cc[r];
      } else if(df$marca[j] != df$marca[r]) {
        Z2[j, 4] <- Z2[j, 4] + (df$combustivel[r] == "Flex");
        Z2[j, 5] <- Z2[j, 5] + (df$combustivel[r] == "Gasolina");
        Z2[j, 6] <- Z2[j, 6] + df$cc[r];
      }
    }
  }
}


df_blp_2 = df %>% cbind(Z2 %>% as.data.frame());





model = as.formula("share ~ preco + combustivel + cc |
    0 + combustivel + cc |
    preco + combustivel + cc |
    0 + ipi + V1 + V2 + V3 + V4 + V5 + V6");

# preparing blp
blp_data = BLP_data(model, 
                     market_identifier = "ano_ref",
                     product_identifier = "modelo",
                     productData = df_blp,
                     integration_method = "MLHS",
                     integration_seed = 8,
                     integration_accuracy = 50);

blp_data_2 = BLP_data(model, 
                      market_identifier = "ano_ref",
                      product_identifier = "modelo",
                      productData = df_blp_2,
                      integration_method = "MLHS",
                      integration_seed = 8,
                      integration_accuracy = 50);
# estimating
blp = estimateBLP(blp_data,
                  solver_method = "BFGS", 
                  solver_maxit = 1000, 
                  solver_reltol = 1e-6,
                  standardError = "homoskedastic",
                  printLevel = 1);

blp_2 = estimateBLP(blp_data_2,
                  solver_method = "BFGS", 
                  solver_maxit = 1000, 
                  solver_reltol = 1e-6,
                  standardError = "homoskedastic",
                  printLevel = 1);

# summary
blp_summary = summary(blp);
blp_summary_2 = summary(blp_2)

# tables
  # linear coefs
blp_table_1 = blp_summary$LinCoefficients;
colnames(blp_table_1) = c('Estimativa','Erro-padrão','Estatística t','p-valor');
stargazer(blp_table_1, align = T, summary = F, digits = 4) %>% write('blp_1.tex');

  # random coefs
blp_table_2 = blp_summary$RcCoefficients;
rownames(blp_table_2) = c('(Intercept)','preco','combustivelFlex','combustivelGasolina','cc');
colnames(blp_table_2) = c('Estimativa','Erro-padrão','Estatística t','p-valor');
stargazer(blp_table_2, align = T, summary = F, digits = 4) %>% write('blp_2.tex');




##s## QUESTAO 9 ####

  #### PARTE 1 ####
# parameters
theta_rc = as.matrix(blp_summary$RcCoefficients$Estimate);
rownames(theta_rc) = c("(Intercept)", "preco", "combustivelFlex", "combustivelGasolina", "cc");
colnames(theta_rc) = c("unobs_sd");


df_blp = df_blp %>% mutate('delta' = blp$delta); 

blp_new_data = update_BLP_data(data_update = df_blp, blp_data = blp_data);

shares = getShareInfo(blp_data = blp_new_data, par_theta2 = theta_rc);

blp_elasticidades = get_elasticities(blp_data = blp_new_data, 
                                        share_info = shares,
                                        theta_lin = blp_summary$LinCoefficients['preco','Estimate'],
                                        variable = "preco",
                                        market = "2009");

blp_elasticidade_GOL = blp_elasticidades['GOL',];

blp_elasticidade_GOL = cbind(df_blp[df_blp$ano_ref==2009,], blp_elasticidade_GOL);
names(blp_elasticidade_GOL)[26] = 'elasticidade';

# elasticities table
  # tabela 9
table_blp_elasticities = blp_elasticidade_GOL %>% 
  select(marca, modelo, share, preco, elasticidade) %>% 
  xtable(digits = c(0,0, 0, 4, 0, 4));


# elasticities x quantity graph
  # figura 2
ggplot(data = blp_elasticidade_GOL[blp_elasticidade_GOL$modelo!='GOL',],
       aes(x = quantidade, y = elasticidade)) + geom_point(); 
ggsave(file = 'figura_2.pdf');


  #### PARTE 2 ####
preco = blp_elasticidade_GOL$preco %>% as.matrix();
J = nrow(preco);
share = blp_elasticidade_GOL$share %>% as.matrix();

Omega_BLP = matrix(nrow = J, ncol = J);

for (j in 1:J) {
  for (r in 1:J) {
    if(blp_elasticidade_GOL$marca[j] != blp_elasticidade_GOL$marca[r]) {
      Omega_BLP[j, r] = 0;
    } else {
      Omega_BLP[j, r] = blp_elasticidades[j,r] * -share[j]/preco[r];
    }
  }
}

# derivando custo marginal
custo_marginal_BLP = preco - solve(Omega_BLP) %*% share %>% as.data.frame();

# combining with data
blp_elasticidade_GOL = blp_elasticidade_GOL %>% mutate(custo_marginal_BLP$V1);
names(blp_elasticidade_GOL)[27] = 'custo_marginal_BLP';

# tabela 10
df_custo_marginal_BLP = select(blp_elasticidade_GOL, marca, modelo, share,
                        preco, custo_marginal_BLP) %>%
  mutate(markup = (preco-custo_marginal_BLP)/custo_marginal_BLP) %>%
  xtable(digits = c(0,0,0,4,0,0,3));



