# Trabalho Distribuição de Renda-------------------------------------
# Este código limpa a PNAD de 2018 e prepara um painel para construção 
# do trabalho de DDP

# 0. Configuração---------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr','haven'), install=T)

# Input
pnad_2018 <- 'Distribuição de Renda/Trabalho/PNADC2018/PNADC2018.dta'

base_pnad_sergipe <- haven::read_dta(file = pnad_2018) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(UF == 28)

base_pnad_brasil <- haven::read_dta(file = pnad_2018) %>% 
  tibble::as_tibble()

# 1. Pobreza: FGT --------------------------------------------------------------

# 1.1. FGT pobreza monetária média da população de Sergipe ---------------------
fgt_media <- base_pnad_sergipe %>% 
  dplyr::group_by(hous_id,Trimestre,Ano) %>%
  dplyr::summarise(renda_dom_mensal = sum(VD4020, na.rm = T),
                   membros = V2001) %>% 
  dplyr::mutate(renda_pc = (renda_dom_mensal/membros),
                g_0 = case_when(renda_pc <= 510.3161 ~ 1,
                                T ~ 0),
                g_1 = case_when(renda_pc <= 510.3161 ~ ((510.3161 - renda_pc)/510.3161),
                                T ~ 0),
                g_2 = case_when(renda_pc <= 510.3161 ~ g_1*g_1,
                                T ~ 0),
                n = nrow(.)) %>% 
  dplyr::group_by(Trimestre) %>% 
  dplyr::summarise(sum_g_0 = sum(g_0, na.rm=T),
                   sum_g_1 = sum(g_1, na.rm =T),
                   sum_g_2 = sum(g_2, na.rm = T)) %>% 
  dplyr::mutate(p_0 = sum_g_0/43769,
                p_1 = sum_g_1/43769,
                p_2 = sum_g_2/43769) %>%
  dplyr::select(p_0,p_1,p_2, Trimestre) %>%
  tidyr::pivot_longer(cols = !Trimestre,
                      names_to = 'medida',
                      values_to = 'valor')
fgt_media %>%
  dplyr::mutate(medida = case_when(medida == 'p_0' ~ 'Incidência da Pobreza',
                                   medida == 'p_1' ~ 'Hiato da Pobreza',
                                   medida == 'p_2' ~ 'Severidade da Pobreza')) %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= Trimestre)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200"))+
  labs(y = 'Valor da medida', x='', 
       title = 'Medidas FGT com linha de pobreza monetária (média da população)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

fgt_media_br <- base_pnad_brasil %>% 
  dplyr::group_by(hous_id,Trimestre,Ano) %>%
  dplyr::summarise(renda_dom_mensal = sum(VD4020, na.rm = T),
                   membros = V2001) %>% 
  dplyr::mutate(renda_pc = (renda_dom_mensal/membros),
                g_0 = case_when(renda_pc <= 510.3161 ~ 1,
                                T ~ 0),
                g_1 = case_when(renda_pc <= 510.3161 ~ ((510.3161 - renda_pc)/510.3161),
                                T ~ 0),
                g_2 = case_when(renda_pc <= 510.3161 ~ g_1*g_1,
                                T ~ 0),
                n = nrow(.)) %>% 
  dplyr::group_by(Trimestre) %>% 
  dplyr::summarise(sum_g_0 = sum(g_0, na.rm=T),
                   sum_g_1 = sum(g_1, na.rm =T),
                   sum_g_2 = sum(g_2, na.rm = T)) %>% 
  dplyr::mutate(p_0 = sum_g_0/43769,
                p_1 = sum_g_1/43769,
                p_2 = sum_g_2/43769) %>%
  dplyr::select(p_0,p_1,p_2, Trimestre) %>%
  tidyr::pivot_longer(cols = !Trimestre,
                      names_to = 'medida',
                      values_to = 'valor')

fgt_media_sergipe <- fgt_media %>% 
  dplyr::filter(Trimestre == 1) %>% 
  dplyr::mutate(local = 'Sergipe')

fgt_media_br %>%
  dplyr::mutate(local = 'Brasil') %>% 
  dplyr::filter(Trimestre == 1) %>% 
  dplyr::bind_rows(fgt_media_sergipe) %>% 
  dplyr::mutate(medida = case_when(medida == 'p_0' ~ 'Incidência da Pobreza',
                                   medida == 'p_1' ~ 'Hiato da Pobreza',
                                   medida == 'p_2' ~ 'Severidade da Pobreza')) %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= Trimestre)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                               "#F8AFA8","#0B775E", "#E2D200"))+
  labs(y = 'Valor da medida', x='', 
       title = 'Medidas FGT com linha de pobreza monetária (média da população)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)


# 1.2. FGT pobreza monetária Banco Mundial -------------------------------------

fgt_bm <- base_pnad_sergipe %>%
  dplyr::group_by(hous_id,Trimestre,Ano) %>%
  dplyr::summarise(renda_dom_mensal = sum(VD4020, na.rm = T),
                   membros = V2001) %>% 
  dplyr::mutate(renda_pc = (renda_dom_mensal/membros),
                g_0 = case_when(renda_pc <= 180 ~ 1,
                                T ~ 0),
                g_1 = case_when(renda_pc <= 180 ~ ((180 - renda_pc)/180),
                                T ~ 0),
                g_2 = case_when(renda_pc <= 180 ~ g_1*g_1,
                                T ~ 0),
                n = nrow(.)) %>% 
  dplyr::group_by(Trimestre) %>% 
  dplyr::summarise(sum_g_0 = sum(g_0, na.rm=T),
                   sum_g_1 = sum(g_1, na.rm =T),
                   sum_g_2 = sum(g_2, na.rm = T)) %>% 
  dplyr::mutate(p_0 = sum_g_0/43769,
                p_1 = sum_g_1/43769,
                p_2 = sum_g_2/43769) %>%
  dplyr::select(p_0,p_1,p_2, Trimestre) %>%
  tidyr::pivot_longer(cols = !Trimestre,
                      names_to = 'medida',
                      values_to = 'valor')
fgt_bm %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, color= Trimestre)) +
  geom_bar(stat = 'identity') + 
  scale_color_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200"))+
  labs(y = 'Valor da medida', x='', 
       title = 'Medidas FGT com linha de pobreza monetária (média da população)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)


# 2. Pobreza: Dinâmica da Pobreza ----------------------------------------------

# 3. ------------------------------------------------------------------------------
# 4. ------------------------------------------------------------------------------
