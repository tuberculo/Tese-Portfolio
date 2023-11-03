library(tidyverse)
library(lubridate)
library(cplexAPI)

source("src/Functions.R")
source("src/parameters.R")
Sys.setenv(PATH = paste(CplexPath, Sys.getenv("PATH"), sep = ":"))
Sys.setenv(LD_LIBRARY_PATH = paste(CplexPath, Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))

SeriesToRun <- readRDS("data/WindSolar.rds")

# Rodar tudo com demanda ---------------------------------------------------
# SeriesToRun <- SeriesToRun %>% mutate(Demand = Demand + 1)
# 
# Amp <- SeriesToRun %>% summarise(abs(max(Demand) - min(Demand))) %>% pull()
# SeriesToRun <- mutate(SeriesToRun, Demand = Demand / Amp)

# Armazenamento (rodada para 1h, 5h e 25h, altere os parâmertros que desejar aqui)
Storages01 <- AddStorOP(SeriesToRun, MaxStorHours = 1, efi = 0.85, ModFile = "src/storage_bina.mod") 
Storages05 <- AddStorOP(SeriesToRun, MaxStorHours = 5, efi = 0.85, ModFile = "src/storage_bina.mod") 
Storages25 <- AddStorOP(SeriesToRun, MaxStorHours = 25, efi = 0.85, ModFile = "src/storage_bina.mod") 

Storages01long <- Storages01 %>% pivot_longer(paste0("STOb_1h_", SeriesToRun[-1] %>% colnames()), 
                                              names_to = "StorageName", values_to = "Value") %>% 
  mutate(NHours = 1, .after = Variable)

Storages05long <- Storages05 %>% pivot_longer(paste0("STOb_5h_", SeriesToRun[-1] %>% colnames()), 
                                              names_to = "StorageName", values_to = "Value") %>% 
  mutate(NHours = 5, .after = Variable)

Storages25long <- Storages25 %>% pivot_longer(paste0("STOb_25h_", SeriesToRun[-1] %>% colnames()), 
                                              names_to = "StorageName", values_to = "Value") %>% 
  mutate(NHours = 25, .after = Variable)

#saveRDS(bind_rows(Storages01long, Storages05long, Storages25long), "data/StoragesOPbina_1_5_25_hours.rds")

SC <- left_join(SeriesToRun, Storages01 %>% filter(Variable == "B") %>% select(-c(Variable, Index))) %>% 
  left_join(Storages05 %>% filter(Variable == "B") %>% select(-c(Variable, Index))) %>% 
  left_join(Storages25 %>% filter(Variable == "B") %>% select(-c(Variable, Index)))


# Termelétricas
Amp <- SeriesToRun %>% summarise(abs(max(Demand) - min(Demand))) %>% pull()
SeriesToRun %>% mutate(Demand = (1 + Demand) / Amp) -> SeriesToRun
# Patamares de ativação 10%, 30% e 60%. Altere ou inclua mais aqui.
Percentuais <- c(0.1, 0.3, 0.6)

Sterm <- map_dfr(Percentuais, ~SeriesToRun %>% 
                mutate(across(-timedate, function(y) if_else(y > .x, 0, 1), .names = "UTElim_{.col}"),
                       Perc = .x)) %>% relocate(Perc, .after = timedate) %>% 
  pivot_wider(names_from = Perc, names_glue = "{.value}_{Perc * 100}p", values_from = contains("UTE")) 

SC <- bind_cols(SC, Sterm |> select(contains("UTElim_")))
SC <- relocate(SC, Demand, .after = last_col())

saveRDS(SC, "data/SeriesComplete.rds")

