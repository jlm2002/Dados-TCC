#====================================
#### Baixando pacotes ##############
#====================================

pacotes <- c(
  "sidrar",
  "ipeadatar",
  "rbcb",
  "tidyverse",
  "lubridate",
  "zoo",
  "stargazer",
  "readxl",
  "dplyr",
  "stringr",
  "ggplot2",
  "corrplot",
  "scales"
  #rbcb"
)

# Instalando os que faltarem
instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(instalar)) install.packages(instalar)

# Carregando todos
invisible(lapply(pacotes, library, character.only = TRUE))

#================================
##Puxando os dados ############
#================================


#1. PIB
#Dados puxados do IBGE para excel
pib_raw <- read_excel(
  path = "~/Desktop/FGV/TCC/Base de dados/Copy of Tab_Compl_CNT_4T24.xlsx",
  sheet = "Série com Ajuste Sazonal",
  skip = 2
)


#Limpeza e organização
pib_trimestral <- pib_raw %>%
  dplyr::select(Trimestre = `Período`, PIB = `PIB`) %>%
  filter(!is.na(Trimestre), !is.na(PIB)) %>%
  mutate(
    #Corrigir o formato dos trimestres para datas
    Trimestre = gsub("\\.I$", " Q1", Trimestre),
    Trimestre = gsub("\\.II$", " Q2", Trimestre),
    Trimestre = gsub("\\.III$", " Q3", Trimestre),
    Trimestre = gsub("\\.IV$", " Q4", Trimestre),
    Trimestre = as.yearqtr(Trimestre, format = "%Y Q%q")
  )


#Tranformar Série PIB trimestral para zoo
pib_zoo <- zoo(pib_trimestral$PIB, pib_trimestral$Trimestre)

#Sequência mensal como yearmon
datas_mensais <- seq(
  from = as.Date(as.yearmon(min(pib_trimestral$Trimestre))),
  to   = as.Date(as.yearmon(max(pib_trimestral$Trimestre))),
  by   = "month"
) %>% as.yearmon()

#Interpolação temporária para criar estrutura mensal
pib_mensal_zoo <- na.approx(pib_zoo, xout = datas_mensais, rule = 2)

#Manter apenas os meses finais dos trimestres (mar, jun, set, dez)
meses_finais <- format(index(pib_mensal_zoo), "%m") %in% c("03", "06", "09", "12")
pib_misto <- pib_mensal_zoo
pib_misto[!meses_finais] <- NA

#Data.frame final
pib_df <- data.frame(
  data = as.Date(as.yearmon(index(pib_misto))),
  pib_mensal = as.numeric(pib_misto)
)


#2. Produção Industrial
#Leitura do CSV
pim_csv <- read_csv2("~/Desktop/FGV/TCC/Base de dados/tabela8888.csv", skip = 3)

#Tratar dados
pim_mensal <- pim_csv %>%
  filter(`...1` == "Brasil") %>%
  pivot_longer(
    cols = -`...1`,
    names_to = "mes_ano",
    values_to = "valor_bruta"
  ) %>%
  mutate(
    valor = as.numeric(str_replace(valor_bruta, ",", ".")),
    mes = word(mes_ano, 1),
    ano = word(mes_ano, 2),
    mes_num = case_when(
      mes == "janeiro" ~ "01",
      mes == "fevereiro" ~ "02",
      mes == "março" ~ "03",
      mes == "abril" ~ "04",
      mes == "maio" ~ "05",
      mes == "junho" ~ "06",
      mes == "julho" ~ "07",
      mes == "agosto" ~ "08",
      mes == "setembro" ~ "09",
      mes == "outubro" ~ "10",
      mes == "novembro" ~ "11",
      mes == "dezembro" ~ "12",
      TRUE ~ NA_character_
    ),
    data = as.Date(paste0(ano, "-", mes_num, "-01"))
  ) %>%
  filter(!is.na(valor), !is.na(data)) %>%
  dplyr::select(data, producao_industrial = valor)

#3. Varejo (PMC)
#Leitura do CSV
varejo_pmc <- read_csv2("~/Desktop/FGV/TCC/Base de dados/tabela8880.csv", skip = 3)

#Tratar dados
varejo_mensal <- varejo_pmc %>%
  filter(`...1` == "Brasil") %>%
  pivot_longer(
    cols = -`...1`,
    names_to = "mes_ano",
    values_to = "valor_bruta"
  ) %>%
  mutate(
    valor = as.numeric(str_replace(valor_bruta, ",", ".")),
    mes = word(mes_ano, 1),
    ano = word(mes_ano, 2),
    mes_num = case_when(
      mes == "janeiro" ~ "01",
      mes == "fevereiro" ~ "02",
      mes == "março" ~ "03",
      mes == "abril" ~ "04",
      mes == "maio" ~ "05",
      mes == "junho" ~ "06",
      mes == "julho" ~ "07",
      mes == "agosto" ~ "08",
      mes == "setembro" ~ "09",
      mes == "outubro" ~ "10",
      mes == "novembro" ~ "11",
      mes == "dezembro" ~ "12",
      TRUE ~ NA_character_
    ),
    data = as.Date(paste0(ano, "-", mes_num, "-01"))
  ) %>%
  filter(!is.na(valor), !is.na(data)) %>%
  dplyr::select(data, varejo_ampliado = valor)

#4. Serviços (PMS)
#Ler CSV 
pms_csv <- read_csv2("~/Desktop/FGV/TCC/Base de dados/tabela5906.csv", skip = 3)

#Tratar dados
pms_mensal <- pms_csv %>%
  filter(`...1` == "Brasil") %>%
  pivot_longer(
    cols = -`...1`,
    names_to = "mes_ano",
    values_to = "valor_bruta"
  ) %>%
  mutate(
    valor = as.numeric(str_replace(valor_bruta, ",", ".")),
    mes = word(mes_ano, 1),
    ano = word(mes_ano, 2),
    mes_num = case_when(
      mes == "janeiro" ~ "01",
      mes == "fevereiro" ~ "02",
      mes == "março" ~ "03",
      mes == "abril" ~ "04",
      mes == "maio" ~ "05",
      mes == "junho" ~ "06",
      mes == "julho" ~ "07",
      mes == "agosto" ~ "08",
      mes == "setembro" ~ "09",
      mes == "outubro" ~ "10",
      mes == "novembro" ~ "11",
      mes == "dezembro" ~ "12",
      TRUE ~ NA_character_
    ),
    data = as.Date(paste0(ano, "-", mes_num, "-01"))
  ) %>%
  filter(!is.na(valor), !is.na(data)) %>%
  dplyr::select(data, volume_servicos = valor)

#5. Desemprego
#Leitura do excel
desemprego_raw <- read_excel("~/Desktop/FGV/TCC/Base de dados/tabela6381.xlsx", skip = 1)

#Extrai nomes dos períodos e valores
periodos <- as.character(desemprego_raw[2, -1])
valores <- as.numeric(gsub(",", ".", as.character(desemprego_raw[3, -1])))

#Criação do data frame tratado
desemprego_mensal <- tibble(
  periodo = as.character(desemprego_raw[2, -1]),
  taxa_desemprego = as.numeric(gsub(",", ".", as.character(desemprego_raw[3, -1])))
) %>%
  mutate(
    mes_abrev = str_sub(periodo, 1, 3),
    ano = as.numeric(str_extract(periodo, "\\d{4}")),
    mes_num = case_when(
      mes_abrev == "jan" ~ "01",
      mes_abrev == "fev" ~ "02",
      mes_abrev == "mar" ~ "03",
      mes_abrev == "abr" ~ "04",
      mes_abrev == "mai" ~ "05",
      mes_abrev == "jun" ~ "06",
      mes_abrev == "jul" ~ "07",
      mes_abrev == "ago" ~ "08",
      mes_abrev == "set" ~ "09",
      mes_abrev == "out" ~ "10",
      mes_abrev == "nov" ~ "11",
      mes_abrev == "dez" ~ "12",
      TRUE ~ NA_character_
    ),
    data = as.Date(paste0(ano, "-", mes_num, "-01"))
  ) %>%
  filter(!is.na(taxa_desemprego), !is.na(data)) %>%
  dplyr::select(data, taxa_desemprego)

#6. Rendimeno médio
#Leitura do excel
rendimento_raw <- read_excel("~/Desktop/FGV/TCC/Base de dados/tabela6388.xlsx", skip = 1)

#Tratamento
rendimento_mensal <- tibble(
  periodo = as.character(rendimento_raw[2, -1]),
  rendimento_real = as.numeric(gsub(",", ".", as.character(rendimento_raw[3, -1])))
) %>%
  mutate(
    mes_abrev = str_sub(periodo, 1, 3),
    ano = as.numeric(str_extract(periodo, "\\d{4}")),
    mes_num = case_when(
      mes_abrev == "jan" ~ "01",
      mes_abrev == "fev" ~ "02",
      mes_abrev == "mar" ~ "03",
      mes_abrev == "abr" ~ "04",
      mes_abrev == "mai" ~ "05",
      mes_abrev == "jun" ~ "06",
      mes_abrev == "jul" ~ "07",
      mes_abrev == "ago" ~ "08",
      mes_abrev == "set" ~ "09",
      mes_abrev == "out" ~ "10",
      mes_abrev == "nov" ~ "11",
      mes_abrev == "dez" ~ "12",
      TRUE ~ NA_character_
    ),
    data = as.Date(paste0(ano, "-", mes_num, "-01"))
  ) %>%
  filter(!is.na(rendimento_real), !is.na(data)) %>%
  dplyr::select(data, rendimento_real)


#7. IPCA
#7.1 IPCA - índice cheio (SGS 433)
ipca_mensal <- get_series(433, start_date = "2000-01-01") %>%
  rename(ipca = `433`) %>%
  mutate(data = as.Date(date)) %>%
  dplyr::select(data, ipca)

#7.2 IPCA - bens industriais (SGS 27863)
ipca_bens_industriais_mensal <- get_series(27863, start_date = "2000-01-01") %>%
  rename(ipca_bens_industriais = `27863`) %>%
  mutate(data = as.Date(date)) %>%
  dplyr::select(data, ipca_bens_industriais)

#8. IGP-M (SGS 189) 
igpm_mensal <- get_series(189, start_date = "2000-01-01") %>%
  rename(igpm = `189`) %>%
  mutate(data = as.Date(date)) %>%
  dplyr::select(data, igpm)

#9 Selic - média mensal (código 4189)
selic_mensal <- get_series(4189, start_date = "2000-01-01") %>%
  rename(selic = `4189`) %>%
  mutate(data = as.Date(date)) %>%
  dplyr::select(data, selic)

#10. Crédito 
#Saldo de crédito total do sistema financeiro (código 20539)
saldo_credito_mensal <- get_series(20539, start_date = "2000-01-01") %>%
  rename(saldo_credito = `20539`) %>%
  mutate(data = as.Date(date))

#Deflacionando com IPCA
saldo_credito_real_mensal <- saldo_credito_mensal %>%
  left_join(ipca_mensal, by = "data") %>%
  mutate(saldo_credito_real = saldo_credito / (1 + ipca / 100)) %>%
  dplyr::select(data, saldo_credito_real)

#10. Dados de setor externo 
#Exportações e importações mensais (em milhões de US$ FOB)
exportacoes_mensal <- get_series(22708, start_date = "2000-01-01") %>%
  rename(exportacoes = `22708`) %>%
  mutate(data = as.Date(date),
         log_exportacoes = log(exportacoes)) %>%
  dplyr::select(data, log_exportacoes)

importacoes_mensal <- get_series(22709, start_date = "2000-01-01") %>%
  rename(importacoes = `22709`) %>%
  mutate(data = as.Date(date),
         log_importacoes = log(importacoes)) %>%
  dplyr::select(data, log_importacoes)

#11. Câmbio nominal (código 3695)
cambio_nominal_mensal <- get_series(3695, start_date = "2000-01-01") %>%
  rename(cambio_nominal = `3695`) %>%
  mutate(data = as.Date(date),
         log_cambio_nominal = log(cambio_nominal)) %>%
  dplyr::select(data, log_cambio_nominal)

#12. Câmbio real efetivo - REER (via arquivo CSV BIS)
reer_mensal <- read_csv("~/Desktop/FGV/TCC/Base de dados/RBBRBIS.csv") %>%
  rename(data = observation_date, reer = RBBRBIS) %>%
  mutate(
    data = as.Date(data),  # CORRIGIDO AQUI
    log_reer = log(reer)
  ) %>%
  filter(!is.na(log_reer)) %>%
  dplyr::select(data, log_reer)

#13. Expectativa e confiança
# 13.1 Índice de Confiança do Consumidor (ICC)
icc <- read.csv2(
  "~/Desktop/FGV/TCC/Base de dados/fgvicc.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

colnames(icc) <- c("data", "icc_valor")

icc_mensal <- icc %>%
  mutate(
    icc_valor = as.numeric(gsub(",", ".", icc_valor)),
    data = as.Date(as.yearmon(data, format = "%m/%Y"))
  ) %>%
  filter(!is.na(data) & !is.na(icc_valor)) %>%
  dplyr::select(data, icc = icc_valor)


#13.2 Índice de Confiança da Indústria (ICI)
dados <- read.csv2(
  "~/Desktop/FGV/TCC/Base de dados/fgvici.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

colnames(dados) <- c("data", "icc_valor", "ici_valor")

ici_mensal <- dados %>%
  mutate(
    ici_valor = as.numeric(gsub(",", ".", ici_valor)),
    data = as.Date(as.yearmon(data, format = "%m/%Y"))  
  ) %>%
  filter(!is.na(data) & !is.na(ici_valor)) %>%
  dplyr::select(data, ici = ici_valor)

#14. Variável logística
#Ler Excel
abcr_raw <- read_excel("~/Desktop/FGV/TCC/Base de dados/abcr_0525.xlsx",
                       sheet = "(C) Original", skip = 2)

abcr <- abcr_raw %>%
  dplyr::select(Data = `...1`, Valor = `PESADOS...3`) %>%
  filter(!is.na(Data), !is.na(Valor))

#Corrige datas se necessário
if (!inherits(abcr$Data, "Date")) {
  abcr$Data <- seq(from = as.Date("1999-01-01"), by = "month", length.out = nrow(abcr))
}

#Tratamento
abcr_mensal <- abcr %>%
  mutate(
    data = as.Date(Data),
    valor = as.numeric(Valor)
  ) %>%
  filter(!is.na(data) & !is.na(valor)) %>%
  dplyr::select(data, fluxo_pesados = valor)

#=================================================================
######## Compilando os dados e estatísticas descritivas ###########
#=================================================================

#Unir todas as variáveis pela coluna "data"
macro_df_mensal <- pib_df %>%
  full_join(pim_mensal, by = "data") %>%
  full_join(varejo_mensal, by = "data") %>%
  full_join(pms_mensal, by = "data") %>%
  full_join(desemprego_mensal, by = "data") %>%
  full_join(rendimento_mensal, by = "data") %>%
  full_join(ipca_mensal, by = "data") %>%
  full_join(ipca_bens_industriais_mensal, by = "data") %>%
  full_join(igpm_mensal, by = "data") %>%
  full_join(selic_mensal, by = "data") %>%
  full_join(saldo_credito_real_mensal, by = "data") %>%
  full_join(exportacoes_mensal, by = "data") %>%
  full_join(importacoes_mensal, by = "data") %>%
  full_join(cambio_nominal_mensal, by = "data") %>%
  full_join(reer_mensal, by = "data") %>%
  full_join(icc_mensal, by = "data") %>%
  full_join(ici_mensal, by = "data") %>%
  arrange(data)

#Coverter data para Date
macro_df_mensal$data <- as.Date(macro_df_mensal$data)

#Filtrando o período de interesse
macro_df_mensal_filtrado <- macro_df_mensal %>%
  filter(format(data, "%Y-%m") >= "2012-01" & format(data, "%Y-%m") <= "2024-12")

#Estatísticas descritivas

##PIB
 #Cria data frame com a interpolação completa
pib_interp_df <- data.frame(
  data = as.Date(as.yearmon(index(pib_mensal_zoo))),
  pib_interp = as.numeric(pib_mensal_zoo)
)

 #Cria data frame com os pontos reais observados (os meses finais de trimestre)
pib_obs_df <- pib_df %>% filter(!is.na(pib_mensal))


 #Gráfico
 #Filtrar intervalo de datas desejado
pib_interp_df_filtrado <- pib_interp_df %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2024-12-31"))

pib_obs_df_filtrado <- pib_obs_df %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2024-12-31"))

 #Gerando o gráfico
ggplot() +
  geom_line(data = pib_interp_df_filtrado, aes(x = data, y = pib_interp),
            color = "mediumpurple", size = 1) +
  geom_point(data = pib_obs_df_filtrado, aes(x = data, y = pib_mensal),
             color = "purple4", size = 2) +
  geom_vline(data = pib_obs_df_filtrado, aes(xintercept = as.numeric(data)),
             linetype = "dotted", color = "purple", alpha = 0.3) +
  scale_x_date(
    date_labels = "%Y", date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Ano",
    y = "PIB (índice)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = margin(t = 25, r = 20, b = 25, l = 20)
  )


##ABCR
 #Filtrar período de interesse
abcr_filtrado <- abcr_mensal %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2024-12-31"))

 #Gráfico de linha
ggplot(abcr_filtrado, aes(x = data, y = fluxo_pesados)) +
  geom_line(color = "purple4", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "mediumpurple", linetype = "dashed") +
  labs(
    x = "Ano",
    y = "Fluxo de veículos pesados (índice)"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(t = 30, r = 15, b = 15, l = 15)
  )

##Atividade Econômica
 #Filtrar e renomear colunas 
atividade_df_cru <- macro_df_mensal_filtrado %>%
  dplyr::select(data,
                `Produção Industrial` = producao_industrial,
                `Varejo Ampliado` = varejo_ampliado,
                `Volume de Serviços` = volume_servicos) %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2024-12-31")) %>%
  pivot_longer(cols = -data, names_to = "Setor", values_to = "Índice")

 #Gráfico
ggplot(atividade_df_cru, aes(x = data, y = Índice, color = Setor)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Produção Industrial" = "mediumpurple",
    "Varejo Ampliado" = "orchid4",
    "Volume de Serviços" = "deeppink"
  )) +
  labs(
    x = "Ano",
    y = "Índice (base original)",
    color = NULL
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    legend.position = "bottom",
    plot.margin = margin(t = 25, r = 20, b = 20, l = 20)
  )

##Condições Monetárias
monetario_df_z <- macro_df_mensal_filtrado %>%
  dplyr::select(data,
                `Inflação (IPCA % a.m.)` = ipca,
                `Taxa SELIC (% a.m.)` = selic,
                `Saldo de Crédito Real` = saldo_credito_real) %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2024-12-31")) %>%
  mutate(across(-data, ~ scale(.)[, 1])) %>%
  pivot_longer(-data, names_to = "Indicador", values_to = "Valor")

ggplot(monetario_df_z, aes(x = data, y = Valor, color = Indicador)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Inflação (IPCA % a.m.)" = "orchid4",
    "Taxa SELIC (% a.m.)" = "mediumpurple",
    "Saldo de Crédito Real" = "deeppink"
  )) +
  labs(
    x = "Ano", y = "Z-score",
    color = NULL
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    legend.position = "bottom"
  )

##Setor Externo
setor_externo_log_df <- macro_df_mensal_filtrado %>%
  dplyr::select(data,
                `Exportações (log)` = log_exportacoes,
                `Importações (log)` = log_importacoes,
                `Câmbio Real Efetivo (REER, log)` = log_reer) %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2024-12-31")) %>%
  pivot_longer(-data, names_to = "Indicador", values_to = "Valor")

ggplot(setor_externo_log_df, aes(x = data, y = Valor, color = Indicador)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Exportações (log)" = "mediumpurple",
    "Importações (log)" = "deeppink",
    "Câmbio Real Efetivo (REER, log)" = "orchid4"
  )) +
  labs(
    x = "Ano", y = "Log (índice original)",
    color = NULL
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


#=========================================================
###### Tratamento para aplicação do FAVAR ################
#=========================================================

#Lista de pacotes
pacotes <- c(
  "vars",     
  "urca",      
  "forecast",  
  "KFAS"       
)

#Instalando os que faltarem
instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(instalar)) install.packages(instalar)

#Carregando todos
invisible(lapply(pacotes, library, character.only = TRUE))

##Fazendo as diferenças necessárias para estacionarizar as séries
  #1. Preparar base
macro_df_dif <- macro_df_mensal_filtrado %>% arrange(data)

  #2. PIB – calcular diff(log(.)) só nos trimestres, mantendo NAs
pib_raw <- macro_df_dif$pib_mensal
pib_idx_obs <- which(!is.na(pib_raw))
pib_diff_log <- c(NA, diff(log(pib_raw[pib_idx_obs])))
pib_final <- rep(NA, length(pib_raw))
pib_final[pib_idx_obs] <- pib_diff_log
macro_df_dif$pib_mensal <- pib_final

  #3. Aplicar diff(log(.)) nas variáveis com tendência
macro_df_dif <- macro_df_dif %>%
  mutate(
    producao_industrial    = c(NA, diff(log(producao_industrial))),
    varejo_ampliado        = c(NA, diff(log(varejo_ampliado))),
    volume_servicos        = c(NA, diff(log(volume_servicos))),
    rendimento_real        = c(NA, diff(log(rendimento_real))),
    saldo_credito_real     = c(NA, diff(log(saldo_credito_real))),
    log_exportacoes        = c(NA, diff(log_exportacoes)),
    log_importacoes        = c(NA, diff(log_importacoes)),
    log_cambio_nominal     = c(NA, diff(log_cambio_nominal)),
    log_reer               = c(NA, diff(log_reer))
  )

  #4. Remover apenas linhas com NA nas colunas transformadas (sem afetar PIB ou outras)
variaveis_diferenciadas <- c(
  "producao_industrial", "varejo_ampliado", "volume_servicos",
  "rendimento_real", "saldo_credito_real",
  "log_exportacoes", "log_importacoes",
  "log_cambio_nominal", "log_reer"
)

macro_df_dif <- macro_df_dif %>% drop_na(all_of(variaveis_diferenciadas))

#Selecionar apenas as colunas numéricas (excluindo data)
X_macro <- macro_df_dif %>%
  dplyr::select(-data)

#Padronizar (z-score)
X_macro_pad <- as.data.frame(
  lapply(X_macro, function(col) {
    if (is.numeric(col)) {
      (col - mean(col, na.rm = TRUE)) / sd(col, na.rm = TRUE)
    } else {
      col
    }
  })
)
X_macro_pad <- as.matrix(X_macro_pad)

##Modelo
n_obs <- ncol(X_macro_pad)    # número de variáveis observadas
n_fatores <- 3                # número de fatores latentes
n_periodos <- nrow(X_macro_pad)

##Construindo as matrizes 
#Z com identificação por blocos de variáveis
Z_mat <- matrix(0, nrow = n_obs, ncol = n_fatores)

bloco1 <- c(1, 2, 3, 4, 10, 11)        # PIB, PIM, PMC, Selic, Crédito
bloco2 <- c(7, 8, 9, 12, 13, 14, 15)    # Inflação e setor externo
bloco3 <- c(5, 6, 16, 17)      # Confiança e mercado de trabalho

Z_mat[bloco1, 1] <- NA
Z_mat[bloco2, 2] <- NA
Z_mat[bloco3, 3] <- NA

Z_na_idx <- which(is.na(Z_mat)) #vetor com as posições que serão atualizadas

#Q e H totalmente livres
Q_mat <- diag(NA, n_fatores)
H_mat <- diag(NA, n_obs)


##Criando modelo com NAS
modelo_base <- SSModel(
  X_macro_pad ~ -1 + SSMcustom(
    Z = Z_mat,
    T = diag(n_fatores),
    R = diag(n_fatores),
    Q = Q_mat,
    P1 = diag(n_fatores)
  ),
  H = H_mat,
  distribution = "gaussian"
)


##Mapeando corretamente os fatores
updatefn <- function(pars, model) {
  
  #Atualiza Z
  model$Z[Z_na_idx] <- pars[1:length(Z_na_idx)]
  
  #Atualiza Q (variância dos fatores)
  start_q <- length(Z_na_idx) + 1
  end_q <- start_q + n_fatores - 1
  diag(model$Q[,,1]) <- exp(pars[start_q:end_q])
  
  #Atualiza H (variância dos erros idiossincráticos)
  start_h <- end_q + 1
  diag(model$H[,,1]) <- exp(pars[start_h:length(pars)])
  
  return(model)
}

##Estimando o modelo
n_Z <- length(Z_na_idx)
n_Q <- n_fatores
n_H <- n_obs
inits <- c(rep(0.1, n_Z), rep(1, n_Q), rep(0.1, n_H))

modelo_estimado <- fitSSM(
  inits = inits,
  model = modelo_base,
  updatefn = updatefn,
  method = "BFGS"
)

##Interpretando a composição dos fatores latentes
Lambda_estimada <- matrix(modelo_estimado$model$Z, nrow = n_obs, ncol = n_fatores)
rownames(Lambda_estimada) <- colnames(X_macro_pad)
colnames(Lambda_estimada) <- paste0("Fator_", 1:n_fatores)

df_cargas <- as.data.frame(Lambda_estimada) 
stargazer(df_cargas, type = "latex", summary = FALSE, digits = 3)

#Montando um df com os lambdas estimados
Lambda_df <- as.data.frame(Lambda_estimada) %>%
  rownames_to_column("variavel") %>%
  pivot_longer(cols = starts_with("Fator_"), names_to = "fator", values_to = "carga")

#Gráfico de barras agrupadas por fator
ggplot(Lambda_df, aes(x = reorder(variavel, carga), y = carga, fill = fator)) +
  geom_col(position = "dodge") +
  facet_wrap(~ fator, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Cargas Fatoriais das Variáveis nos Fatores Latentes",
    x = "Variável",
    y = "Carga Fatorial"
  ) +
  theme_minimal()

##Obtendo os fatores
kfs_result <- KFS(modelo_estimado$model, smoothing = "state")
fatores_estimados <- kfs_result$alphahat

#Transformando em data frame
fatores_df <- as.data.frame(fatores_estimados)
colnames(fatores_df) <- c("F1", "F2", "F3")
fatores_df$data <- macro_df_dif$data


##Visualização
datas <- fatores_df$data

par(mar = c(4, 4, 2, 2)) 

#Gráfico
matplot(x = datas, y = fatores_estimados, type = "l", lty = 1,
        col = c("deeppink", "purple4", "skyblue"),
        main = "", ylab = "Valor do Fator", xlab = "Ano",
        lwd = 1.5, xaxt = "n")

axis.Date(1, at = seq(min(datas), max(datas), by = "1 year"), format = "%Y")

legend("topleft",
       legend = paste("Fator", 1:3),
       col = c("deeppink", "purple4", "skyblue"),
       lty = 1, lwd = 1.5,
       bty = "n", inset = c(0.02, 0.02),
       cex = 0.9)

##Incluindo a variável logística
#Garantir que as datas estejam no mesmo formato
abcr_mensal$data <- as.Date(abcr_mensal$data)

# Renomear a variável para facilitar
col_abcr <- setdiff(colnames(abcr_mensal), "data")
abcr_mensal <- abcr_mensal %>% rename(abcr = all_of(col_abcr))

#Juntando as variáveis macro com a variável logística
base_var <- fatores_df %>%
  inner_join(abcr_mensal, by = "data") %>%
  dplyr::select(F1, F2, F3, abcr)



#=======================================================
######## Aplicação do FAVAR e IRFs #####################
#=======================================================

##Escolhando o número ótimo de defasagens pra rodar o VAR
criterios <- VARselect(base_var, lag.max = 6, type = "const")
criterios$criteria

##Estimando o VAR
modelo_var <- vars::VAR(base_var, p = 3, type = "const")
summary(modelo_var)

#tenstando a estabilidade do modelo
stability_var <- stability(modelo_var, type = "OLS-CUSUM")
plot(stability_var)

#Gerando as IRFs
#Choque no fator 1
irf_f1 <- irf(modelo_var, impulse = "F1", response = "abcr", n.ahead = 12, boot = TRUE)
plot(irf_f1, main = "")

# Choque no fator 2
irf_f2 <- irf(modelo_var, impulse = "F2", response = "abcr", n.ahead = 12, boot = TRUE)
plot(irf_f2, main = "")

#Choque no fator 3
irf_f3 <- irf(modelo_var, impulse = "F3", response = "abcr", n.ahead = 12, boot = TRUE)
plot(irf_f3, main = "")

