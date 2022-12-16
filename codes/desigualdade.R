setwd("C:/Users/Dimitri/Documents/desigualdade")
library(haven)
library(tidyverse)
library(ineq)
library(survey)
library(convey)
library(srvyr)
library(janitor)
library(PNADcIBGE)
library(aod)
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
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
  mutate(renda_pc = sum(VD4020, na.rm = T)/V2001,
         SE = ifelse(UF == "Sergipe", 1, 0),
         SP = ifelse(UF == "São Paulo", 1, 0),
         miseravel = ifelse(renda_pc < 293.0085, 1, 0),
         pobre = ifelse(renda_pc < 493.98, 1, 0)) %>% ungroup()
rm(x,y)


br <- as_survey_design(filter(pnad, V2005 == "Pessoa responsável pelo domicílio"),
                       weights = V1028, strata = Estrato)
se <- filter(pnad, UF == "Sergipe" & V2005 == "Pessoa responsável pelo domicílio")
se <- as_survey_design(se, weights = V1028, strata = Estrato)


#   Contrstrução das Curvas dos Quantis

q100_se <- svyquantile(x = ~ renda_pc,
                       design = se,
                       quantiles = seq(0, 1, .01),
                       na.rm = T)
q100_br <- svyquantile(x = ~ renda_pc,
                       design = br,
                       quantiles = seq(0, 1, .01),
                       na.rm = T)
plot(seq(0,1,.01), unlist(q100_se$renda_pc[,1]), type = "l",
     main = "Figura 10: Curva de Quantis da distribuição de renda domiciliar",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     ylab = "Renda per capita", col = "red")
  lines(seq(0,1,.01), unlist(q100_br$renda_pc[,1]), col = "blue")
  legend(-0.035, 51900, legend=c("Sergipe", "Brasil"),
       col=c("red", "blue"), lty=1, cex=0.8, box.lty = 0)

q99_se <- svyquantile(x = ~ renda_pc,
                       design = se,
                       quantiles = seq(0, .99, .01),
                       na.rm = T)
q99_br <- svyquantile(x = ~ renda_pc,
                      design = br,
                      quantiles = seq(0, .99, .01),
                      na.rm = T)
plot(seq(0,.99,.01), unlist(q99_se$renda_pc[,1]), type = "l",
     main = "Figura 11: Curva de Quantis da distribuição de
renda domiciliar (excetuando o primeiro centil)",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     ylab = "Renda per capita", col = "red")
  lines(seq(0,.99,.01), unlist(q99_br$renda_pc[,1]), col = "blue")
  legend(-0.035, 6650, legend=c("Sergipe", "Brasil"),
       col=c("red", "blue"), lty=1, cex=0.8, box.lty = 0)

q90_se <- svyquantile(x = ~ renda_pc,
                         design = se,
                         quantiles = seq(0, .90, .01),
                         na.rm = T)
q90_br <- svyquantile(x = ~ renda_pc,
                      design = br,
                      quantiles = seq(0, .90, .01),
                      na.rm = T)
plot(seq(0,.90,.01), unlist(q90_se$renda_pc[,1]), type = "l",
     main = "Figura 12: Curva de Quantis da distribuição de
renda domiciliar (excetuando o primeiro decil)",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     ylab = "Renda per capita", col = "red")
lines(seq(0,.90,.01), unlist(q90_br$renda_pc[,1]), col = "blue")
legend(-0.035, 1550, legend=c("Sergipe", "Brasil"),
       col=c("red", "blue"), lty=1, cex=0.8, box.lty = 0)

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
     main = "Figura 13: Curva de Lorenz da distribuição
da renda pessoal per capita",
     ylab = "% rendimentos acumulados",
     xlab = "Centis da população, ordenados por rendimentos crescentes",
     col = "red")
lines(seq(0,1,.01), unlist(lorenz_br$quantiles[1,]), col = "blue")
lines(seq(-1,2,1), seq(-1,2,1), col = 1, lty = 2)
legend(-0.035, 1.035, legend=c("Sergipe", "Brasil"),
       col=c("red", "blue"), lty=1, cex=0.8, box.lty = 0)

br <- convey_prep(design = br)
se <- convey_prep(design = se)

gini_se <- svygini(formula = ~renda_pc,
                  design = se,
                  na.rm = T)
            unlist(gini_se)
gini_br <- svygini(formula = ~renda_pc,
                  design = br,
                  na.rm = T)
            unlist(gini_br)
            
            
## PROBIT
#   Determinante da pobreza em sergipe
sergipe <- filter(pnad, UF == "Sergipe" & V2005 == "Pessoa responsável pelo domicílio") %>%
  mutate(branco = ifelse(V2010 == "Branca", 1, 0),
         faculdade = ifelse((V3009A == "Superior - graduação" |
                               V3009A == "Especialização de nível superior" |
                               V3009A == "Mestrado" | V3009A == "Doutorado"), 1, 0))
det_pobre_se <- glm(formula = pobre ~ as.factor(V1022) + V2001 + as.factor(V2007)
    + branco + faculdade,
    family = binomial(link = "probit"), weights = V1028, data = sergipe)
    se_dps <- sqrt(diag(vcovHC(det_pobre_se, type = "HC0")))
det_mis_se <- glm(formula = miseravel ~ as.factor(V1022) + V2001 + as.factor(V2007)
                    + branco + faculdade,
                    family = binomial(link = "probit"), weights = V1028, data = sergipe)
  se_dms <- sqrt(diag(vcovHC(det_mis_se, type = "HC0")))


base1 <- filter(pnad, V2005 == "Pessoa responsável pelo domicílio") %>% 
  mutate(branco = ifelse(V2010 == "Branca", 1, 0),
         faculdade = ifelse((V3009A == "Superior - graduação" |
                               V3009A == "Especialização de nível superior" |
                               V3009A == "Mestrado" | V3009A == "Doutorado"), 1, 0))
det_pobre_migr <- glm(formula = pobre ~ as.factor(V1022) + V2001 + as.factor(V2007)
                    + branco + faculdade + SE,
                    family = binomial(link = "probit"), weights = V1028, data = base1)
se_dpm <- sqrt(diag(vcovHC(det_pobre_migr, type = "HC0")))
det_mis_migr <- glm(formula = miseravel ~ as.factor(V1022) + V2001 + as.factor(V2007)
                  + branco + faculdade + SE,
                  family = binomial(link = "probit"), weights = V1028, data = base1)
se_dmm <- sqrt(diag(vcovHC(det_mis_migr, type = "HC0")))

stargazer(det_pobre_se, det_mis_se, det_pobre_migr, det_mis_migr, type = "latex",
          se = list(se_dps, se_dms, se_dpm, se_dmm))
