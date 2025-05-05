library(readxl)
library(dplyr)
library(rjags)
library(coda)
library(tidybayes)
library(tidyr)
library(HDInterval)
library(writexl)


# Carregar e filtrar dados
data <- read_excel("DADOS BRUTOS/ECQ_OUT_24.xlsx", sheet = "Export") %>%
  filter(ANF == 83) %>%
  mutate(TESTES_ECQ_OK = round(TESTES_ECQ * ECQ, 0))  # Recriar a variável resposta

# Criar IDs únicos para grupos (agora só ANF 83)
data <- data %>%
  group_by(ANF, MUNICIPIO) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup()

# Mapeamento de grupos para ANF (só 1 ANF agora)
group_df <- data %>%
  distinct(group_id, ANF) %>%
  arrange(group_id)

# Substituir a preparação de dados anterior por:
jags_data <- list(
  N_group = max(data$group_id),
  N_sites = nrow(data),
  group_per_site = data$group_id,
  n_tests = data$TESTES_ECQ,
  n_success = data$TESTES_ECQ_OK
)
model_string <- "
model {
  # Hiperparâmetros para ANF
  mu_anf ~ dbeta(2, 2)
  
  # Priors com restrições mais fortes
  phi_municipio ~ dgamma(1, 0.1)  # Prior mais informativa
  phi_site ~ dgamma(1, 0.1)
  
  # Parâmetros municipais com proteção robusta
  for(g in 1:N_group) {
    a_municipio[g] <- mu_anf * phi_municipio + 0.01
    b_municipio[g] <- (1 - mu_anf) * phi_municipio + 0.01
    mu_municipio[g] ~ dbeta(a_municipio[g], b_municipio[g])
  }
  
  # Nível dos sites com transformação logística
  for(s in 1:N_sites) {
    logit_mu_site[s] <- logit(mu_municipio[group_per_site[s]])
    theta_site[s] <- ilogit(logit_mu_site[s] + epsilon[s])
    epsilon[s] ~ dnorm(0, 1/phi_site)
    
    n_success[s] ~ dbin(theta_site[s], n_tests[s])
  }
}
"
# Adicionar inicializações explícitas
inits <- function() {
  list(
    mu_anf = runif(1, 0.2, 0.8),
    phi_municipio = runif(1, 1, 10),
    phi_site = runif(1, 1, 10)
  )
}

# Modificar a inicialização do modelo
model <- jags.model(
  textConnection(model_string),
  data = jags_data,
  inits = inits,
  n.chains = 4,
  n.adapt = 1000
)

# Amostragem com monitoramento cuidadoso
samples <- coda.samples(
  model,
  variable.names = c("mu_anf", "phi_municipio", "phi_site", 
                     "mu_municipio", "theta_site"),
  n.iter = 5000,
  thin = 2  # Reduzir autocorrelação
)

# Verificar convergência
gelman.diag(samples)
effectiveSize(samples)
#traceplot(samples)

# Analisar resultados
summary(samples)

#Analise 


# 1. Mapear group_id para ANF e Município
group_map <- data %>%
  distinct(group_id, ANF, MUNICIPIO)

# 2. Extrair estatísticas dos municípios
municipio_stats <- samples %>%
  spread_draws(mu_municipio[group_id]) %>%
  group_by(group_id) %>%
  summarise(
    mean_municipio = mean(mu_municipio),
    stddev_municipio = sd(mu_municipio),
    hdi_inf_Mun = hdi(mu_municipio, credMass = 0.95)[1],
    hdi_sup_Mun = hdi(mu_municipio, credMass = 0.95)[2]
  ) %>%
  left_join(group_map, by = "group_id")

# 3. Extrair estatísticas dos sites
site_stats <- samples %>%
  spread_draws(theta_site[site_index]) %>%  # site_index corresponde à ordem nos dados originais
  group_by(site_index) %>%
  summarise(
    mean_site = mean(theta_site),
    stddev_site = sd(theta_site),
    hdi_inf = hdi(theta_site, credMass = 0.95)[1],
    hdi_sup = hdi(theta_site, credMass = 0.95)[2]
  ) %>%
  mutate(site_index = as.integer(site_index))

# 4. Combinar com dados originais
final_df <- data %>%
  mutate(site_index = row_number()) %>%  # Criar índice correspondente ao theta_site
  left_join(site_stats, by = "site_index") %>%
  left_join(municipio_stats, by = c("group_id", "ANF", "MUNICIPIO")) %>%
  mutate(
    FALHAS_ECQ = TESTES_ECQ - TESTES_ECQ_OK,
    # Calcular métricas de impacto
    FALHAS_ECQ_norm = (FALHAS_ECQ - min(FALHAS_ECQ)) / 
      (max(FALHAS_ECQ) - min(FALHAS_ECQ) + 1e-8),
    impacto = (1 - mean_site) * FALHAS_ECQ_norm,
    rank = dense_rank(desc(impacto))
  ) %>%
  select(
    ANF, MUNICIPIO, ENDERECO_ID,
    hdi_inf, mean_site, hdi_sup,
    hdi_inf_Mun, mean_municipio, hdi_sup_Mun,
    TESTES_ECQ, TESTES_ECQ_OK, FALHAS_ECQ,
    impacto, rank
  ) %>%
  arrange(ANF, MUNICIPIO, rank)

# 5. Verificar e exportar
if(nrow(final_df) == nrow(data)) {
  write_xlsx(final_df, "priorizacao_sites_ECQ_R_OUT24.xlsx")
  message("Arquivo exportado com sucesso!")
} else {
  warning("Verificar correspondência entre índices e dados!")
}


# Visualizar estrutura dos dados processados
glimpse(final_df)

# Verificar primeiros rankings
final_df %>%
  select(ENDERECO_ID, impacto, rank) %>%
  head(10)





# Extrair amostras de mu_anf
mu_anf_samples <- samples %>%
  spread_draws(mu_anf) %>%  # Extrai todas as amostras
  mutate(parameter = "mu_anf")  # Para identificação

# Calcular estatísticas sumárias
mu_anf_stats <- mu_anf_samples %>%
  group_by(parameter) %>%
  summarise(
    mean = mean(mu_anf),
    std_dev = sd(mu_anf),
    hdi_3 = hdi(mu_anf, credMass = 0.94)[1],
    hdi_97 = hdi(mu_anf, credMass = 0.94)[2]
  )

# Formatar para exibição
mu_anf_formatted <- mu_anf_stats %>%
  mutate(
    across(c(mean, std_dev, hdi_3, hdi_97), ~ round(., 3)),
    hdi_interval = glue::glue("[{hdi_3}, {hdi_97}]")
  ) %>%
  select(parameter, mean, std_dev, hdi_interval)

mu_anf_formatted
