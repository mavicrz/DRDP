# Trabalho Distribuição de Renda-------------------------------------
# Este código limpa a PNAD de 2018 e prepara um painel para construção 
# do trabalho de DRDP

# 0. Configuração---------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr','haven','survey', 'srvyr','convey'), install=T)

# Input
pnad_2018 <- 'Distribuição de Renda/Trabalho/input/PNADC2018/PNADC2018.dta'


# 1. Bases ----------------------------------------------------------------------
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
  dplyr::group_by(hous_id,trimestre,ano) %>%
  dplyr::mutate(renda_dom_mensal = mean(vd4020, na.rm = T),
                membros = v2001,
                renda_pc = (renda_dom_mensal/membros)) %>%
  dplyr::ungroup() %>% 
  design_pnad_covid()


base_pnad_brasil <- haven::read_dta(file = pnad_2018) %>% 
  tibble::as_tibble() %>%  
  janitor::clean_names(.) %>% 
  dplyr::group_by(hous_id,trimestre,ano) %>%
  dplyr::mutate(renda_dom_mensal =mean(vd4020, na.rm = T),
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
fgt_bm_sergipe <- fgt(base = base_pnad_sergipe, linha = 180,local = 'Sergipe')

graph_bm_sergipe <- fgt_bm_sergipe %>% 
ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= medida)) +
  geom_bar(stat = 'identity', show.legend = F) + 
  scale_fill_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200"))+
  labs(y = 'Valor da medida', x='', 
       title = 'Medidas FGT com linha de pobreza monetária (Banco Mundial USD$3.20)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_bm_sergipe,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_bm_sergipe.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

fgt_bm_brasil <- fgt(base = base_pnad_brasil, linha = 180,local = 'Brasil') %>% 
  dplyr::bind_rows(fgt_bm_sergipe)

graph_bm_brasil <- fgt_bm_brasil %>% 
  ggplot2::ggplot(mapping = aes(x=medida, y=valor, fill= local)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = c("#F8AFA8", "#E2D200","#0B775E"))+
  labs(y = 'Valor da medida', x='', fill = '',
       title = 'Medidas FGT com linha de pobreza monetária (Banco Mundial USD$3.20)') +
  theme_minimal() +
  geom_text(aes(label= format(round(valor,3), nsmall=3)), position=position_dodge(width=0.9), vjust=-0.25)

ggsave(graph_bm_brasil,
       filename = 'Distribuição de Renda/Trabalho/output/fgt_bm_brasil.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

# 2.3. FGT pobreza monetária Banco Mundial - Raça ------------------------------

# 2.4. FGT pobreza monetária Banco Mundial - Escolaridade ----------------------

# 2.5. FGT pobreza monetária Banco Mundial - Sexo ------------------------------


# 3. Pobreza: Dinâmica da Pobreza ----------------------------------------------

# 4. ------------------------------------------------------------------------------
# 5. ------------------------------------------------------------------------------
