setwd("C:/Users/Dimitri/Documents/desigualdade")
library(haven)
library(tidyverse)
library(ineq)
library(survey)
library(convey)
library(srvyr)
library(janitor)
library(PNADcIBGE)
rm(list = ls())
gc()

#   Contrstrução do Dataframe

pnad_1t <- get_pnadc(year = 2018,
                        quarter = 1,
                        defyear = 2018,
                        defperiod = 4,
                        deflator = T,
                        design = F)
pnad_2t <- get_pnadc(year = 2018,
                     quarter = 2,
                     defyear = 2018,
                     defperiod = 4,
                     deflator = T,
                     design = F)
pnad_3t <- get_pnadc(year = 2018,
                     quarter = 3,
                     defyear = 2018,
                     defperiod = 4,
                     deflator = T,
                     design = F)
pnad_4t <- get_pnadc(year = 2018,
                     quarter = 4,
                     defyear = 2018,
                     defperiod = 4,
                     deflator = T,
                     design = F)
x <- rbind(pnad_1t,pnad_2t)
y <- rbind(x,pnad_3t)
pnad <- rbind(y,pnad_4t) %>% group_by(ID_DOMICILIO,Trimestre,Ano) %>%
  mutate(renda_pc = mean(VD4020, na.rm = T)/V2001) %>% ungroup()
rm(x,y)


br <- as_survey_design(pnad, weights = V1028, strata = Estrato)
se <- filter(pnad, UF == "Sergipe")
se <- as_survey_design(se, weights = V1028, strata = Estrato)

#   Contrstrução das Curvas dos Quantis

quantil <- svyquantile(x = ~ renda_pc,
                       design = se,
                       quantiles = seq(0, 1, .01),
                       na.rm = T)
plot(seq(0,1,.01), unlist(quantil$renda_pc[,1]), type = "l",
     main = "Curva de Quantis da distribuição de renda pessoal,
     Sergipe 2018",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     ylab = "Renda per capita (em R$2018Q4)")

quantil99 <- svyquantile(x = ~ renda_pc,
                       design = se,
                       quantiles = seq(0, .99, .01),
                       na.rm = T)
plot(seq(0,.99,.01), unlist(quantil99$renda_pc[,1]), type = "l",
     main = "Curva de Quantis da distribuição de renda pessoal,
     Sergipe 2018 (excetuando o primeiro centil)",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     ylab = "Renda per capita (em R$2018Q4)")

quantil90 <- svyquantile(x = ~ renda_pc,
                         design = se,
                         quantiles = seq(0, .90, .01),
                         na.rm = T)
plot(seq(0,.90,.01), unlist(quantil90$renda_pc[,1]), type = "l",
     main = "Curva de Quantis da distribuição de renda pessoal
     Sergipe 2018 (excetuando o primeiro decil)",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     ylab = "Renda per capita (em R$2018Q4)")

#   Contrstrução das Curvas de Lorenz

lorenz_se <- svylorenz(formula = ~renda_pc,
                        design = se,
                        quantiles = seq(0, 1, .01),
                        na.rm = T)
lorenz_br <- svylorenz(formula = ~renda_pc,
                       design = br,
                       quantiles = seq(0, 1, .01),
                       na.rm = T)
plot(seq(0,1,.01), unlist(lorenz_se$quantiles[1,]), type = "l",
     main = "Curva de Lorenz da distribuição da renda
     pessoal per capita, 2018",
     ylab = "% rendimentos acumulados",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     col = "red")
lines(seq(0,1,.01), unlist(lorenz_br$quantiles[1,]), col = "blue")
lines(seq(0,1,.01), seq(0,1,.01), col = 1)
legend(-0.035, 1.035, legend=c("Sergipe", "Brasil"),
       col=c("red", "blue"), lty=1, cex=0.8, box.lty = 0)
