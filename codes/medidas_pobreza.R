# Trabalho Distribuição de Renda-------------------------------------
# Este código limpa a PNAD de 2018 e prepara um painel para construção 
# do trabalho de DDP

# 0. Configuração---------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr','haven','survey', 'srvyr','convey', 'PNADcIBGE'), install=T)

# Input
pnad_2018 <- 'Distribuição de Renda/Trabalho/input/PNADC2018/PNADC2018.dta'
deflator <- readr::read_csv('Distribuição de Renda/Trabalho/input/deflator.csv') %>% 
  dplyr::mutate(trimestre = as.character(trimestre))

## 1.1 Base Pacote IBGE ---------------------------------------------------------

func_ibge <- function(trimestre){get_pnadc(year = 2018,
                                           quarter = trimestre,
                                           defyear = 2018,
                                           defperiod = 4,
                                           deflator = T, 
                                           design = F,
                                           vars = c("UF",
                                                    "Ano",
                                                    "Trimestre"))}

pnad <- purrr::map_df(.x = 1:4, .f = ~ func_ibge(trimestre = .x))

pnad_sergipe_ibge <- pnad %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(UF == 'Sergipe') %>% 
  janitor::clean_names(.) %>% 
  distinct(trimestre, efetivo, habitual)

write_csv(pnad_sergipe_ibge, file = 'Distribuição de Renda/Trabalho/input/deflator.csv')

## 1.2 Base PNAD 2018 ----------------------------------------------------------

design_pnad_covid <- function(data) {
  
  pop <- data %>% # base auxiliar com projeções populacionais
    select(posest, Freq = v1029) %>% 
    distinct() %>% 
    arrange(posest)
  
  data %>% # definindo desenho amostral + pós-estratificação
    svydesign(data = ., ids = ~ upa, strata = ~ estrato, weights = ~ v1027, nest = TRUE) %>% 
    postStratify(design = ., strata = ~ posest, population = pop) %>% 
    return()
  
}

base_pnad_sergipe <- haven::read_dta(file = pnad_2018) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(UF == 28) %>% 
  janitor::clean_names(.) %>% 
  dplyr::left_join(deflator, by = 'trimestre') %>% 
  dplyr::group_by(hous_id,trimestre,ano) %>%
  dplyr::mutate(renda_dom_mensal = sum(vd4020, na.rm = T),
                membros = v2001,
                renda_pc = (renda_dom_mensal/membros),
                renda_dom_mensal_def = sum(vd4020, na.rm = T)*efetivo,
                renda_pc_def = (renda_dom_mensal_def/membros)) %>%
  dplyr::ungroup() %>% 
  design_pnad_covid()

base_pnad_brasil <- haven::read_dta(file = pnad_2018) %>% 
  tibble::as_tibble() %>%  
  janitor::clean_names(.) %>% 
  dplyr::group_by(hous_id,trimestre,ano) %>%
  dplyr::mutate(renda_dom_mensal =sum(vd4020, na.rm = T),
                membros = v2001,
                renda_pc = (renda_dom_mensal/membros)) %>%
  dplyr::ungroup() %>% 
  design_pnad_covid()

# 2. Pobreza: FGT --------------------------------------------------------------

# 2.1. FGT pobreza monetária média da população de Sergipe ---------------------

fgt <- function(base,linha,local){
  fgt0 <- base %>% 
    srvyr::as_survey() %>% 
    dplyr::filter(trimestre ==1) %>% 
    convey::convey_prep() %>% 
    convey::svyfgt(formula = ~ renda_pc, g = 0, abs_thresh = linha, na.rm = TRUE) %>% 
    tibble::as_tibble(rownames = 'conceito') %>% 
    dplyr::mutate(local = local) %>% 
    dplyr::select(conceito, fgt0,local)
  
  fgt1 <- base %>% 
    srvyr::as_survey() %>% 
    dplyr::filter(trimestre ==1) %>% 
    convey::convey_prep() %>% 
    convey::svyfgt(formula = ~ renda_pc, g = 1, abs_thresh = linha, na.rm = TRUE) %>% 
    tibble::as_tibble(rownames = 'conceito') %>% 
    dplyr::mutate(local = local) %>% 
    dplyr::select(conceito, fgt1,local)
  
  fgt2 <- base %>% 
    srvyr::as_survey() %>% 
    dplyr::filter(trimestre ==1) %>% 
    convey::convey_prep() %>% 
    convey::svyfgt(formula = ~ renda_pc, g = 2, abs_thresh = linha, na.rm = TRUE) %>% 
    tibble::as_tibble(rownames = 'conceito') %>% 
    dplyr::mutate(local = local) %>% 
    dplyr::select(conceito, fgt2,local)
  
  fgt0 %>%
    dplyr::left_join(fgt1, by = c('conceito','local')) %>%
    dplyr::left_join(fgt2, by =c('conceito','local')) %>% 
    tidyr::pivot_longer(cols = !c(conceito,local), names_to = 'medida',values_to = 'valor') %>% 
    dplyr::mutate(medida = case_when(medida == 'fgt0' ~ 'Incidência da Pobreza',
                                     medida == 'fgt1' ~ 'Hiato da Pobreza',
                                     medida == 'fgt2' ~ 'Severidade da Pobreza')) 
}


fgt_media_sergipe <- fgt(base = base_pnad_sergipe, linha = 587,local = 'Sergipe')

graph_media_sergipe <- fgt_media_sergipe %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= medida)) +
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) + 
  scale_fill_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200"))+
  labs(y = 'Valor da medida', x='', 
       title = 'Medidas FGT com linha de pobreza monetária (renda per capita média da população)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_media_sergipe,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_media_sergipe.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

fgt_media_brasil <- fgt(base = base_pnad_brasil, linha = 587,local = 'Brasil') %>% 
  dplyr::bind_rows(fgt_media_sergipe)

graph_media_brasil <- fgt_media_brasil %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= local)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#F8AFA8", "#E2D200","#0B775E"))+
  labs(y = 'Valor da medida', x='', fill = '',
       title = 'Medidas FGT com linha de pobreza monetária (renda per capita média da população)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_media_brasil,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_media_brasil.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

# 2.2. FGT pobreza monetária Banco Mundial -------------------------------------
fgt_bm_sergipe <- fgt(base = base_pnad_sergipe, linha = 208.2894,local = 'Sergipe')

graph_bm_sergipe <- fgt_bm_sergipe %>% 
ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= medida)) +
  geom_bar(stat = 'identity', show.legend = F) + 
  scale_fill_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200"))+
  labs(y = 'Valor da medida', x='', 
       title = 'Medidas FGT com linha de pobreza monetária (Banco Mundial USD$1.9)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_bm_sergipe,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_bm_sergipe.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

fgt_bm_brasil <- fgt(base = base_pnad_brasil, linha = 208.2894,local = 'Brasil') %>% 
  dplyr::bind_rows(fgt_bm_sergipe)

graph_bm_brasil <- fgt_bm_brasil %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= local)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#F8AFA8", "#E2D200","#0B775E"))+
  labs(y = 'Valor da medida', x='', fill = '',
       title = 'Medidas FGT com linha de pobreza monetária (Banco Mundial USD$1.9)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_bm_brasil,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_bm_brasil.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

# 2.3. FGT pobreza monetária Banco Mundial - Raça ------------------------------
fgt_branco_sergipe <- base_pnad_sergipe %>% 
  as_survey(.) %>% 
  dplyr::filter(v2010==1) %>% 
  fgt(base = ., linha = 208.2894,local = 'Sergipe') %>% 
  dplyr::mutate(var = 'Branco')

fgt_nbranco_sergipe <- base_pnad_sergipe %>% 
  as_survey(.) %>% 
  dplyr::filter(v2010 !=1) %>% 
  fgt(base = ., linha = 208.2894,local = 'Sergipe')%>% 
  dplyr::mutate(var = 'Não Branco')

graph_cor_sergipe <- fgt_branco_sergipe %>% 
  dplyr::bind_rows(fgt_nbranco_sergipe) %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= var)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#FAD510","#cf3a36"))+
  labs(y = 'Valor da medida', x='', fill = '',
       title = 'Medidas FGT para Sergipe por Raça/Cor (Banco Mundial USD$1.9)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_cor_sergipe,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_cor_sergipe.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

# 2.4. FGT pobreza monetária Banco Mundial - Escolaridade ----------------------
fgt_sabeler_sergipe <- base_pnad_sergipe %>% 
  as_survey(.) %>% 
  dplyr::filter(v3001== 1) %>% 
  fgt(base = ., linha = 208.2894,local = 'Sergipe') %>% 
  dplyr::mutate(var = 'Sabe Ler')

fgt_nsabeler_sergipe <- base_pnad_sergipe %>% 
  as_survey(.) %>% 
  dplyr::filter(v3001 == 2) %>% 
  fgt(base = ., linha = 208.2894,local = 'Sergipe')%>% 
  dplyr::mutate(var = 'Não Sabe Ler')

graph_ler_sergipe <- fgt_sabeler_sergipe %>% 
  dplyr::bind_rows(fgt_nsabeler_sergipe) %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= var)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#FAD510","#cf3a36"))+
  labs(y = 'Valor da medida', x='', fill = '',
       title = 'Medidas FGT para Sergipe por Escolaridade (Banco Mundial USD$1.9)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_ler_sergipe,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_ler_sergipe.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')


# 2.5. FGT pobreza monetária Banco Mundial - Sexo ------------------------------
fgt_homem_sergipe <- base_pnad_sergipe %>% 
  as_survey(.) %>% 
  dplyr::filter(v2007== 1 & v2005 == 1) %>% 
  fgt(base = ., linha = 208.2894,local = 'Sergipe') %>% 
  dplyr::mutate(var = 'Homem')

fgt_mulher_sergipe <- base_pnad_sergipe %>% 
  as_survey(.) %>% 
  dplyr::filter(v2007== 2 & v2005 == 1) %>% 
  fgt(base = ., linha = 208.2894,local = 'Sergipe')%>% 
  dplyr::mutate(var = 'Mulher')

graph_sexo_sergipe <- fgt_homem_sergipe %>% 
  dplyr::bind_rows(fgt_mulher_sergipe) %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= var)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#FAD510","#cf3a36"))+
  labs(y = 'Valor da medida', x='', fill = '',
       title = 'Medidas FGT para Sergipe por Sexo do Responsável (Banco Mundial USD$1.9)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_sexo_sergipe,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_sexo_sergipe.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

# 3. Pobreza: Dinâmica da Pobreza ----------------------------------------------

## 3.1. Definir domicílios acompanhados ao longo dos trimestres ----------------
dom <- base_pnad_sergipe %>%
  as_survey %>%
  srvyr::as_tibble() %>%
  dplyr::filter(is.nan(renda_pc_def) == F) %>% 
  distinct(hous_id,trimestre) %>% 
  dplyr::add_count(hous_id) %>% 
  dplyr::distinct(hous_id, n) %>% 
  dplyr::filter(n ==4)

## 3.2. Dinâmica da Pobreza com Renda Média ------------------------------------

base_dinamica_renda_media <- base_pnad_sergipe %>% 
  as_survey %>%
  srvyr::as_tibble() %>%
  dplyr::right_join(dom, by = 'hous_id') %>%
  dplyr::mutate(pobre  = case_when(renda_pc_def < 587 ~ 1,T ~0)) %>%
  dplyr::distinct(hous_id,trimestre, ano,pobre) %>%
  dplyr::group_by(hous_id) %>% 
  dplyr::summarise(n = sum(pobre, na.rm = T)) %>% 
  dplyr::mutate(classe = case_when(n == 4 ~ 'Sempre Pobre',
                                   n == 0 ~ 'Nunca Pobre',
                                   n == 3 ~ 'Usualmente Pobre',
                                   n == 2 ~ 'Ciclicamente Pobre',
                                   n == 1 ~ 'Ocasionalmente Pobre'))

dinamica_renda_media <-  base_dinamica_renda_media %>%
  add_count(classe) %>% 
  distinct(classe, nn) %>% 
  dplyr::mutate(perc = nn*100/746) %>% 
  ggplot(mapping = aes(x = classe, y = perc, fill = classe))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = c("#FAD510","#cf3a36", "#F8AFA8", "#5c66a8","#0B775E"))+
  labs(y = 'Porcentagem de Domicílios (%)', x='', fill = '',
       title = 'Categorização da Dinâmica da Pobreza\n (Renda Per capita Média da População de Sergipe em reais de 2018Q4)') +
  theme_minimal() +
  geom_text(aes(label= format(round(perc,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(dinamica_renda_media,
       filename = 'Distribuição de Renda/Trabalho/output/dinamica.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

## 3.3. Dinâmica da Pobreza com Banco Mundial ----------------------------------

base_dinamica_renda_bm <- base_pnad_sergipe %>% 
  as_survey %>%
  srvyr::as_tibble() %>%
  dplyr::right_join(dom, by = 'hous_id') %>%
  dplyr::mutate(pobre  = case_when(renda_pc_def < 208.2894 ~ 1,T ~0)) %>%
  dplyr::distinct(hous_id,trimestre, ano,pobre) %>%
  dplyr::group_by(hous_id) %>% 
  srvyr::summarise(n = sum(pobre, na.rm = T)) %>% 
  dplyr::mutate(classe = case_when(n == 4 ~ 'Sempre Pobre',
                                   n == 0 ~ 'Nunca Pobre',
                                   n == 3 ~ 'Usualmente Pobre',
                                   n == 2 ~ 'Ciclicamente Pobre',
                                   n == 1 ~ 'Ocasionalmente Pobre'))

dinamica_renda_bm <-  base_dinamica_renda_bm %>%
  add_count(classe) %>% 
  distinct(classe, nn) %>% 
  dplyr::mutate(perc = nn*100/746) %>% 
  ggplot(mapping = aes(x = classe, y = perc, fill = classe))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = c("#FAD510","#cf3a36", "#F8AFA8", "#5c66a8","#0B775E"))+
  labs(y = 'Porcentagem de Domicílios (%)', x='', fill = '',
       title = 'Categorização da Dinâmica da Pobreza\n (Linha de Pobreza Banco Mundial USD$ 1.9 em reais de 2018Q4)') +
  theme_minimal() +
  geom_text(aes(label= format(round(perc,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)


ggsave(dinamica_renda_bm,
       filename = 'Distribuição de Renda/Trabalho/output/dinamica_bm.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

# 4. Probabilidade de Entrada na Pobreza ---------------------------------------
trimestre_1 <- base_pnad_sergipe %>% 
  as_survey %>%
  srvyr::as_tibble() %>%
  srvyr::filter(trimestre == 1) %>% 
  tidyr::pivot_wider(names_from = 'trimestre',
                     names_prefix = 'renda_pc_def_',
                     values_from = 'renda_pc_def')

trimestre_4 <-base_pnad_sergipe %>% 
  as_survey %>%
  srvyr::as_tibble() %>%
  srvyr::filter(trimestre == 4) %>% 
  tidyr::pivot_wider(names_from = 'trimestre',
                     names_prefix = 'renda_pc_def_',
                     values_from = 'renda_pc_def') %>% 
  dplyr::select(renda_pc_def_4, hous_id)


reg <- trimestre_1 %>% 
  left_join(trimestre_4, by = 'hous_id')%>% 
  srvyr::mutate(pobre_4 = case_when(renda_pc_def_4 <  208.2894 ~ 1, T~0),
              pobre_1 = case_when(renda_pc_def_1 <  208.2894 ~ 1, T ~0),
              sexo_domicilio = case_when(v2007== 1 & v2005 == 1 ~ 1, T~0),
              cor = case_when(v2010==1 ~ 1, T~ 0),
              escolaridade = case_when(v3009a %in% c(1:8) ~ 1, T~0),
              across(.cols = c(v1022, v3001), ~case_when(. == 1~ 0, . == 2 ~ 1,T ~.)),
              domicilio_rural = v1022,
              n_sabe_ler=v3001,
              num_pessoas = v2001)

reg_income <- survey::svyglm(formula = renda_pc_def_4 ~ v2001 + sexo_domicilio + cor +
                   escolaridade+  v1022 + v3001 + renda_pc_def_1, design =  reg)



logit_income <- glm(formula = pobre_4 ~ num_pessoas + sexo_domicilio + cor +
                               escolaridade+  domicilio_rural + n_sabe_ler + pobre_1,data = reg,
                    family = 'binomial')


des <- base_pnad_sergipe %>% 
  as_survey() %>% 
  as_tibble() %>%
  filter(trimestre ==1 & v2005 == 1) %>%
  distinct(ind_id, .keep_all = T) %>% 
  ggplot(., aes(x=renda_pc/1000)) + 
  geom_density(fill="#F8AFA8") +
  labs(x = 'Rendimento bruto mensal', y='', fill = '',
       title = 'Densidade de renda per capita 20181Q (em milhares)') +
  scale_x_continuous(breaks = 0:30)+
  theme_minimal()

ggsave(des,
       filename = 'Distribuição de Renda/Trabalho/output/densidade.png',
       width = 25, height = 20, device = 'png', bg = 'white',
       units = 'cm')
