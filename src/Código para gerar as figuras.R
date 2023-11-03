# Preparação --------------------------------------------------------------
library(tidyverse)
library(corrr)
library(geosphere)
library(ggrepel)
#library(geobr)

source("src/Functions.R")
source("src/parameters.R")
if (!dir.exists("figuras")) dir.create("figuras")

SeriesComplete <- readRDS(SeriesFile)
PlantData <- readRDS(PlantDataFile)

Series <- SeriesComplete
MaxLoad <- AveLoad / -colMeans(Series["Demand"])  # Valor médio da carga.

PlantData <- left_join(PlantData,
                       bind_rows(PlantData %>% filter(Type == "Storage") %>% 
                                   separate(name, into = c("SepType", "SepH", "BaseName"), sep = "_", remove = FALSE),
                                 PlantData %>% filter(Type == "Thermal") %>% 
                                   separate(name, into = c("SepType", "BaseName"), sep = "_", remove = FALSE)) %>% 
                         select(name, BaseName) %>% left_join(PlantData, by = c("BaseName" = "name")) %>% 
                         select(name, BaseName, BaseType = Type))

PlantData <- PlantData %>% 
  mutate(Type2 = case_when(Type == "Thermal" & str_detect(name, fixed("Comp", ignore_case = TRUE)) ~ "ThermalComp",
                           Type == "Thermal" & str_detect(name, fixed("peak", ignore_case = TRUE)) ~ "ThermalPeak",
                           #Type == "Thermal" & str_detect(name, fixed("demand", ignore_case = TRUE)) ~ paste0(Type2, "L"),
                           #Type == "Thermal" & str_detect(name, fixed("demand", ignore_case = TRUE)) ~ paste0(Type2, "L"),
                           TRUE ~ Type),
         Type2 = case_when(BaseType == "Load" ~ paste0(Type2, "L"),
                           BaseType == "Wind" ~ paste0(Type2, "W"),
                           BaseType == "PV" ~ paste0(Type2, "P"),
                           TRUE ~Type2
))


VariancePlant <- sapply(Series %>% select(-timedate), function(x) var(x))
SDplant <- sqrt(VariancePlant)

list2env(readRDS(ResultFile), envir = .GlobalEnv)

CodeIndex <- tribble(~Codename, ~TipoDemanda, ~NomeTese, ~AvailPlants, ~OptType, ~omega, ~Beta,
                     "A1", "Flat", "G*'-'*Cap^{Flat}", "Wind, PV; flat load", "MaxGen_IsoCap", 0, 0.95,
                     "A2", "Obs", "G*'-'*Cap^{Dem}", "Wind, PV; real load", "MaxGen_IsoCap", 0, 0.95,
                     "B1", "Flat", "C*'-'*Gen^{Flat}", "Wind, PV; flat load", "MinCost_IsoGen", 0, 0.95,
                     "B2", "Obs", "C*'-'*Gen^{Dem}", "Wind, PV; real load", "MinCost_IsoGen", 0, 0.95,
                     "C1", "Flat", "C*'-'*CVaR^{Flat}", "Wind, PV; flat load", "MinCost_IsoRisk", 0, 0.95,
                     "C2", "Obs", "C*'-'*CVaR^{Dem}", "Wind, PV; real load", "MinCost_IsoRisk", 0, 0.95,
                     "B1_LCPV", "Cost_Flat_lcpv", "C*'-'*Gen*'-'*B^{Flat}", "Wind, PV; flat load, PV half cost", "MinCost_IsoGen", 0, 0.95,
) |> mutate(ControllableVersion = 0)

ExecParam <- left_join(ExecParam, CodeIndex, by = join_by(Beta, omega, OptType, ControllableVersion, AvailPlants))
ExecParam <- ExecParam %>% 
  mutate(DemandType = factor(case_when(str_detect(AvailPlants, fixed("real load", ignore_case = TRUE)) ~ "Real",
                                                                TRUE ~ "Flat")),
         HasWind = str_detect(AvailPlants, fixed("Wind", ignore_case = TRUE)),
         HasPV = case_when(str_detect(AvailPlants, fixed("PV half")) ~ "Half cost",
                           str_detect(AvailPlants, fixed("PV")) ~ "Full cost",
                           TRUE ~ "No"),
         HasStorage = case_when(str_detect(AvailPlants, fixed("STO low", ignore_case = TRUE)) ~ "Low cost",
                                str_detect(AvailPlants, fixed("Storage", ignore_case = TRUE)) ~ "Full cost",
                                TRUE ~ "No"),
         HasThermal = str_detect(AvailPlants, fixed("Thermal", ignore_case = TRUE)))

#MultiplierResults <- readRDS("results/MultiplierResults.rds")
ModelName <- c(MaxGen_IsoCap = "G-Cap",
               MinCost_IsoGen = "C-Gen",
               MinCost_IsoRisk = "C-CVaR")

## Termelétricas-sombra com 9 níveis (figuras 3.1 e 3.2) ------
Dates <- as.Date(c("12/10/2016", 
                   "19/10/2016"), "%d/%m/%Y")
## Como fica somando as diferentes usinas
Usinas <- c("Cerro Chato", "810", "Demand")
SeriesToRun <- select(Series, c(timedate, all_of(Usinas)))
# Para ter as séries de UTE para testes e gráficos
Percentuais <- 1:9 / 10
Multip <- (lead(c(Percentuais, 1)) - c(Percentuais, 1)) %>% head(-1)
TermBin <- map_dfr(Percentuais, ~SeriesToRun %>% 
                     mutate(across(-timedate, function(y) if_else(y > .x, 0, 1), .names = "{.col}_TermBina"),
                            Perc = .x)) %>% relocate(Perc, .after = timedate)
nome <- sym("Cerro Chato")
nomeTerm <- sym(paste0(nome, "_TermBina"))
# (figura 3.1)
TermBin %>% left_join(tibble(Perc = Percentuais, Mult = Multip)) %>% 
  filter(timedate >= Dates[1], timedate < Dates[2]) %>% 
  group_by(timedate) %>% summarise(GeraComb = sum(!!nomeTerm * Mult)) %>% 
  left_join(TermBin) %>% left_join(tibble(Perc = Percentuais, Mult = Multip)) %>% 
  mutate(Hora = as.numeric(timedate - min(timedate)) / 3600) %>% 
  ggplot() + geom_line(aes(x = Hora, y = !!nome, color = "Eólica")) + 
  geom_area(aes(x = Hora, y = !!nomeTerm * Mult, fill = paste0(Perc * 100, "%")), alpha = 0.6) + 
  geom_line(aes(x = Hora, y = GeraComb + !!nome, color = "Eólica + térmicas")) +
  theme_light() + labs(fill = bquote(G[min]), y = "Geração em % do máximo") + 
  scale_color_manual(name='Geração',
                     breaks=c('Eólica + térmicas', 'Eólica'),
                     values=c('Eólica + térmicas'='black', 'Eólica'='brown')) + 
  scale_y_continuous(labels = scales::label_percent())
ggsave("figuras/3.1 - Demonstração de 9 UTE para EOL.pdf", width = 15, height = 12, units = "cm")
nome <- sym("810")
nomeTerm <- sym(paste0(nome, "_TermBina"))
# (figura 3.2)
TermBin %>% left_join(tibble(Perc = Percentuais, Mult = Multip)) %>% 
  filter(timedate >= Dates[1], timedate < Dates[2]) %>% 
  group_by(timedate) %>% summarise(GeraComb = sum(!!nomeTerm * Mult)) %>% 
  left_join(TermBin) %>% left_join(tibble(Perc = Percentuais, Mult = Multip)) %>% 
  mutate(Hora = as.numeric(timedate - min(timedate)) / 3600) %>% 
  ggplot() + geom_line(aes(x = Hora, y = !!nome, color = "FV")) + 
  geom_area(aes(x = Hora, y = !!nomeTerm * Mult, fill = paste0(Perc * 100, "%")), alpha = 0.6) + 
  geom_line(aes(x = Hora, y = GeraComb + !!nome, color = "FV + térmicas")) +
  theme_light() + labs(fill = bquote(G[min]), y = "Geração em % do máximo") + 
  scale_color_manual(name='Geração',
                     breaks=c('FV + térmicas', 'FV'),
                     values=c('FV + térmicas'='black', 'FV'='brown')) + 
  scale_y_continuous(labels = scales::label_percent())
ggsave("figuras/3.2 - Demonstração de 9 UTE para UFV.pdf", width = 15, height = 12, units = "cm")
## Comparação de métodos de armazenamento (figuras 3.3 e 3.4) ------------
Dates <- as.Date(c("01/02/2015", 
                   "08/02/2015"), "%d/%m/%Y")
StoragesL <- readRDS("data/Comparação de métodos para armazenamento.rds")

nome <- sym("810")
StoragesLong <- StoragesL %>% filter(StorageName == nome) %>% 
  filter(timedate >= Dates[1], timedate < Dates[2], Variable == "B") %>%  
  left_join(Series %>% select(timedate, !!nome) %>% rename(Geração = !!nome)) %>% 
  mutate(Value = case_when(Variable == "StoLev" ~ Value / NHours, TRUE ~ Value),
         Value = case_when(TipoOpt %in% c("Quad_Peq", "Lin_Peq") ~ Value * 100, 
                           TRUE ~ Value),
         Hora = Index - min(Index)) %>% #, GeraComb = 0.5 * Value + Geração) %>% 
  pivot_longer(any_of(c("Value", "Geração", "GeraComb")), names_to = "Tipo", values_to = "Valor") %>% 
  filter(TipoOpt %in% c("Lin_Peq", "Quad_Peq"))
# Método linear (figura 3.3) 
ggplot(StoragesLong %>% filter(TipoOpt == "Lin_Peq")) + 
  geom_line(aes(x = Hora, y = Valor, color = Tipo)) + 
  facet_wrap(~NHours, ncol = 1) +
  labs(y = "Saída em % da capacidade", color = "Tipo de saída") +
  scale_color_discrete(labels = c(Geração = "Geração UFV", Value = "Armazenamento", GeraComb = "Saída conjunta")) + 
  theme_light() +
  theme(legend.position = "bottom") + scale_y_continuous(labels = scales::label_percent())
ggsave("figuras/3.3 - Comparação de método para armazenamento - linear.pdf", width = 15, height = 12, units = "cm")
# Método quadrático (figura 3.4) 
ggplot(StoragesLong %>% filter(TipoOpt == "Quad_Peq")) + 
  geom_line(aes(x = Hora, y = Valor, color = Tipo)) + 
  facet_wrap(~NHours, ncol = 1) +
  labs(y = "Saída em % da capacidade", color = "Tipo de saída") +
  scale_color_discrete(labels = c(Geração = "Geração UFV", Value = "Armazenamento", GeraComb = "Saída conjunta")) + 
  theme_light() +
  theme(legend.position = "bottom") + scale_y_continuous(labels = scales::label_percent())
ggsave("figuras/3.4 - Comparação de método para armazenamento - quadrático.pdf", width = 15, height = 12, units = "cm")
# Método quadrático - eólica (figura 3.5)
Dates <- as.Date(c("12/10/2016", 
                   "19/10/2016"), "%d/%m/%Y")
nome <- sym("Cerro Chato")
StoragesLong <- StoragesL %>% filter(StorageName == nome) %>% 
  filter(timedate >= Dates[1], timedate < Dates[2], Variable == "B") %>%  
  left_join(Series %>% select(timedate, !!nome) %>% rename(Geração = !!nome)) %>% 
  mutate(Value = case_when(Variable == "StoLev" ~ Value / NHours, TRUE ~ Value),
         Value = case_when(TipoOpt %in% c("Quad_Peq", "Lin_Peq") ~ Value * 100, 
                           TRUE ~ Value),
         Hora = Index - min(Index)) %>% #, GeraComb = 0.5 * Value + Geração) %>% 
  pivot_longer(any_of(c("Value", "Geração", "GeraComb")), names_to = "Tipo", values_to = "Valor") %>% 
  filter(TipoOpt %in% c("Lin_Peq", "Quad_Peq"))
ggplot(StoragesLong %>% filter(TipoOpt == "Quad_Peq")) + 
  geom_line(aes(x = Hora, y = Valor, color = Tipo)) + 
  facet_wrap(~NHours, ncol = 1) +
  labs(y = "Saída em % da capacidade", color = "Tipo de saída") +
  scale_color_discrete(labels = c(Geração = "Geração eólica", Value = "Armazenamento", GeraComb = "Saída conjunta")) + 
  theme_light() +
  theme(legend.position = "bottom") + scale_y_continuous(labels = scales::label_percent())
ggsave("figuras/3.5 - Comparação de método para armazenamento - quadrático e eólica.pdf", width = 15, height = 12, units = "cm")

# Dados de entrada --------------------------------------------------------
# Prepare data
NomesPort <- c(Load = "Demanda",
               PV = "FV",
               Wind = "Eólica")

Correl <- correlate(SeriesComplete %>% mutate(Demand = -Demand) %>% 
                      select(any_of(PlantData %>% 
                                      filter(Used, Type %in% c("Wind", "PV", "Load")) %>% 
                                      pull(name))), use = "everything")
plotcorrel <- stretch(Correl) %>%
  left_join(select(PlantData, name, Type.x = Type), by = c("x" = "name")) %>%
  left_join(select(PlantData, name, Type.y = Type), by = c("y" = "name")) %>% drop_na()


## Correlação entre usinas (figura 3.6) -------------------------------------------------
CorrelStats <- plotcorrel %>% group_by(Type.x, Type.y) %>% 
  summarise(Min = min(r), Mean = mean(r), Max = max(r), Median = median(r))
p <- ggplot(plotcorrel) + geom_histogram(aes(x = r, fill = Type.y), bins = 40)
p + facet_wrap(~Type.x, scales = "free_y", labeller = labeller(Type.x = NomesPort)) + 
  scale_y_continuous() + theme_light() + theme(legend.position = "bottom") + 
  scale_fill_discrete(labels = NomesPort) + 
  labs(fill = "Tipo de componente", x = "Correlação de Pearson", y = "Quantidade de pares de localidades")
ggsave("figuras/3.6 - Correlação entre usinas.pdf", width = 15, height = 12, units = "cm")

## Distance scatterplot (figura 3.7) --------------------------------------------------
distance <- as_cordf(distm(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used,
                                     c("long", "lat")]))
colnames(distance) <- c(colnames(distance[1]),
                        deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")]))
distance$term <- deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")])
distance <- stretch(distance) %>% mutate(dist = r / 1000, .keep = "unused")

# Compare only by the same type (wind to wind and PV to PV)
left_join(plotcorrel, distance) %>% distinct(r, dist, .keep_all = TRUE) %>% 
  filter(Type.x == Type.y) %>%
  ggplot(aes(x = dist, y = r, fill = Type.x)) + geom_point(shape = 21, stroke = 0.1) + 
  geom_smooth(aes(color = Type.x)) + 
  labs(x = "Distância (km)", y = "Correlação de Pearson", 
       fill = "Tecnologia", color = "Tecnologia") + 
  scale_fill_brewer(palette = "Set1", labels = NomesPort) + scale_color_brewer(palette = "Set1", labels = NomesPort) +
  xlim(0, NA) + theme_light()
ggsave("figuras/3.7 - Correlação por distância.pdf", width = 15, height = 12, units = "cm")

# Resultados --------------------------------------------------------------
#Abaixo estão as figuras para comparar as fronteiras eficientes

## ComparaFronteirasFC (figura 4.1) -----------------------------------------------------
SeleCode <- c("A1", "B1", "B1_LCPV")
PlotDataCompare <- left_join(ExecParam, MainResults) %>% 
  mutate(PortSD = W_SD / SumRC2L, PortSD_Gen = W_SD / SumWc) 
ModelName <- PlotDataCompare %>% filter(Codename %in% SeleCode) %>% distinct(NomeTese) %>% deframe()

# Tabela auxiliar para colocar a ordem das cores como eu quiser (ggplot organiza por ordem alfabética)
ColorCodes <- tibble(Code = c("M1", "M2", "M3", "A1", "A2"), 
                     Names = c(ModelName, c("Wind", "PV")),
                     Names2 = c(ModelName, c("Eólica", "FV")))

PlotCompare <- ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% 
               filter(Type %in% c("Wind", "PV")) %>% left_join(ColorCodes, by = c("Type" = "Names")),
             mapping = aes(x = SD, y = CF, color = Code)) +
  geom_line(PlotDataCompare %>% filter(Codename %in% SeleCode) %>% left_join(ColorCodes, by = c("NomeTese" = "Names")),
            mapping = aes(x = PortSD, y = SumWc / SumRC2L, color = Code)) + 
  geom_point(PlotDataCompare %>% 
               filter(Codename == "B1") %>% filter(PortSD == min(PortSD)), 
             mapping = aes(x = PortSD, y = SumWc / SumRC2L)) + 
  geom_label_repel(PlotDataCompare %>% 
                     filter(Codename == "B1") %>% 
                     filter(PortSD == min(PortSD)), mapping = aes(x = PortSD, y = SumWc / SumRC2L, label = "MinCV")) + 
  labs(x = "Desvio padrão (% da potência)", y = "Fator de capacidade", color = "Modelos e tecnologias") + theme_light() +
  scale_color_brewer(palette = "Set1", 
                     labels = scales::label_parse()(deframe(ColorCodes[c(1,3)])),
                     breaks = ColorCodes$Code, 
                     guide = guide_legend(override.aes = # Separa as legendas entre as de ponto e as de linha
                                            list(shape = c(rep(NA, length(ModelName)), 
                                                           rep(16, length(c("Wind", "PV")))), 
                                                 linetype = c(rep(1, length(ModelName)), 
                                                              rep(NA, length(c("Wind", "PV"))))))) + 
  scale_x_continuous(labels = scales::percent) + scale_y_log10(labels = scales::percent) 

PlotCompare + theme(legend.position = c(0.27, 0.2))
ggsave("figuras/4.1 - ComparaFronteirasFC.pdf", width = 15, height = 12, units = "cm")

## ComparaFronteirasCusto (figura 4.2) --------------------------------------------------
SeleCode <- c("A1", "B1")
ModelName <- PlotDataCompare %>% filter(Codename %in% SeleCode) %>% distinct(NomeTese) %>% deframe()
PlotCompare <- ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% filter(Type %in% c("Wind", "PV")) %>% 
               left_join(ColorCodes, by = c("Type" = "Names")),
             mapping = aes(x = SD / CF, y = CostMWh, color = Code)) +
  geom_path(PlotDataCompare %>% filter(Codename %in% SeleCode) %>% left_join(ColorCodes, by = c("NomeTese" = "Names")),
            mapping = aes(x = PortSD_Gen, y = W_Cost / SumWc, color = Code)) +
  geom_point(PlotDataCompare %>% 
               filter(Codename == "B1") %>% filter(PortSD == min(PortSD)), 
             mapping = aes(x = PortSD_Gen, y = W_Cost / SumWc)) + 
  geom_label_repel(PlotDataCompare %>% 
                     filter(Codename == "B1") %>% 
                     filter(PortSD == min(PortSD)), mapping = aes(x = PortSD_Gen, y = W_Cost / SumWc, label = "MinCV")) + 
  labs(x = "Desvio padrão (% da geração esperada)", y = "Custo (R$/MWh)", color = "Modelos e tecnologias") + theme_light() +
  scale_color_brewer(palette = "Set1",
                     labels = scales::label_parse()(deframe(ColorCodes[c(1,3)])),
                     breaks = ColorCodes$Code, 
                     guide = guide_legend(override.aes = # Separa as legendas entre as de ponto e as de linha
                                            list(shape = c(rep(NA, length(ModelName)), 
                                                           rep(16, length(c("Wind", "PV")))), 
                                                 linetype = c(rep(1, length(ModelName)), 
                                                              rep(NA, length(c("Wind", "PV"))))))) + 
  scale_x_log10(labels = scales::percent) + scale_y_log10()

PlotCompare + theme(legend.position = c(0.83, 0.2))
ggsave("figuras/4.2 - ComparaFronteirasCusto.pdf", width = 15, height = 12, units = "cm")

# Participação de FV por cenário (figura 4.3) ------
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         #| OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         #| AvailPlants == "Wind, PV; real load"
         | AvailPlants == "Wind, PV; flat load"
         | AvailPlants == "Wind, PV; flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()
left_join(ExecParam, MainResults) %>% left_join(PlantResults) %>%  
  left_join(PlantData, by = c("Plant" = "name", "Type", "UF", "Storage capacity")) %>% 
  filter(idExec %in% Selec, Type != "Load") %>% group_by(NomeTese, W_SD, SumRC2L, Type2) %>% 
  summarise(TypeGenShare = sum(GenShare), TypeCapShare = sum(CapShare))%>% 
  filter(Type2 == "PV") %>% 
  ggplot() + 
  geom_line(aes(y = TypeCapShare, x = W_SD / SumRC2L, 
                color = NomeTese), size = 1) + 
  theme_light() + labs(x = "Desvio padrão (% da potência)", y = "Participação de FV na capacidade do portfólio", color = "Cenário") + 
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ",")) + 
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) + 
  scale_color_discrete(labels = scales::label_parse()) + 
  theme(legend.position = c(0.85, 0.5))
ggsave("figuras/4.3 - UFV por cenário.pdf", width = 15, height = 9, units = "cm")

### Fronteira com demanda (figura 4.4) ------------------------------------------------------
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         #| OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind, PV; real load"
         | AvailPlants == "Wind, PV; flat load"
         #| AvailPlants == "Wind, PV; flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()
ModelName <- c(MaxGen_IsoCap = "G-Cap",
               MinCost_IsoGen = "C-Gen",
               MinCost_IsoRisk = "C-CVaR")
ggplot(left_join(ExecParam, MainResults) %>% 
         filter(idExec %in% Selec), aes(x = sqrt(W_Variance_no_Load) / SumRC2L, 
                                        y = SumWc / SumRC2L,
                                        color = OptType, linetype = AvailPlants)) + 
  geom_path() + theme_light() +
  labs(x = "Desvio padrão sem demanda (% da potência)", y = "Fator de capacidade", 
       color = "Modelo", linetype = "Tipo de demanda") + 
  scale_linetype_discrete(labels = c("Constante", "Observada")) +
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ",")) + 
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  theme(legend.position = c(0.8, 0.4)) + scale_color_hue(labels = ModelName)
ggsave("figuras/4.4 - Fronteiras com carga.pdf", width = 15, height = 9, units = "cm")

## Participação de FV com curva de carga (figura 4.5) -----------------------------------
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind, PV; real load"
         | AvailPlants == "Wind, PV; flat load"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()

PVshare <- filter(left_join(ExecParam, left_join(MainResults, PlantResults)), 
                  str_detect(Plant, "Demanda", negate = TRUE), idExec %in% Selec) %>% 
  group_by(idExec, OptType, AvailPlants, Sim, W_Variance_no_Load, 
           W_SD, SumRC2L, Type, Codename) %>% 
  summarise(SumCapShare = sum(CapShare), 
            SumGenShare = sum(GenShare)) %>% 
  filter(Type == "PV")

ggplot(PVshare, aes(x = sqrt(W_Variance_no_Load) / SumRC2L, 
                    y = SumCapShare, color = AvailPlants)) + 
  geom_line() + theme_light() + 
  labs(x = "Desvio padrão sem demanda (% da potência)", 
       y = "Participação de FV na capacidade do portfólio", color = "Tipo de demanda") + 
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1, decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) + 
  scale_colour_discrete(labels = c("Constante", "Observada")) + 
  facet_wrap(~OptType, scales = "free_x", labeller = labeller(OptType = ModelName)) + 
  theme(legend.position = "bottom")
ggsave("figuras/4.5 - ParticipFV.pdf", width = 15, height = 9, units = "cm", scale = 1.2)

### Fronteira com CVaR ------------------------------------------------------
### Fronteira com CVaR e carga (figuras 4.6 e 4.7) ------------------------------------------------------
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind, PV; real load"
         | AvailPlants == "Wind, PV; flat load"
         #| AvailPlants == "Wind, PV; flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()
#Versão com facet (figura 4.6)
p <- ggplot(left_join(ExecParam, MainResults) %>% 
         filter(idExec %in% Selec), aes(x = sqrt(W_Variance_no_Load) / SumRC2L , 
                                        y = SumWc / SumRC2L,
                                        color = OptType)) + 
  geom_path() + theme_light() +
  labs(x = "Desvio padrão sem demanda (% da potência)", y = "Fator de capacidade", 
       color = "Modelo") +
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme(legend.position = "bottom") + 
  scale_color_hue(labels = ModelName) + 
  facet_wrap(~AvailPlants, labeller = 
               labeller(AvailPlants = c("Wind, PV; flat load" = "Demanda flat", 
                                        "Wind, PV; real load" = "Demanda observada"))) 
p
ggsave("figuras/4.6 - Fronteiras com CVaR e carga.pdf", width = 15, height = 10, units = "cm")
#Versão com facet e por custo (figura 4.7)
p2 <- ggplot(left_join(ExecParam, MainResults) %>% 
              filter(idExec %in% Selec), aes(x = sqrt(W_Variance_no_Load) / SumWc , 
                                             y = W_Cost / SumWc,
                                             color = OptType)) + 
  geom_path() + theme_light() +
  labs(x = "Desvio padrão sem demanda (% da geração esperada)", y = "Custo (R$/MWh)", 
       color = "Modelo") +
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  theme(legend.position = "bottom") + 
  scale_color_hue(labels = ModelName) + 
  facet_wrap(~AvailPlants, labeller = 
               labeller(AvailPlants = c("Wind, PV; flat load" = "Demanda flat", 
                                        "Wind, PV; real load" = "Demanda observada"))) 
p2 + coord_cartesian(xlim = c(0.295, 0.4))#, ylim = c(0.3, 0.4))
ggsave("figuras/4.7 - Fronteiras com CVaR e carga por custo - zoom.pdf", width = 15, height = 10, units = "cm")

# Diversificação (figura 4.8) -----
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind, PV; real load"
         | AvailPlants == "Wind, PV; flat load"
         #| AvailPlants == "Wind, PV; flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()
IndexName <- c(EucDiv = "DistEuc",
               GeoDiv = "DistGeo",
               HHI =  "{}^2*D",  #"Núm. equi. ordem 2",
               Shannon = "{}^{1}*D ") #"Núm. equi. ordem 1")

ggplot(left_join(ExecParam, MainResults) %>% left_join(DivIndex) %>% 
         filter(idExec %in% Selec, By == "Gen", WhichPlants == "All") %>% 
         mutate(invValue = case_when(Index == "Shannon" ~ exp(Value),
                                     Index == "HHI" ~ 1 / Value,
                                     TRUE ~ Value))) +
  geom_path(aes(x = W_SD / SumRC2L, y = invValue, color = OptType), size = 0.7) + 
  scale_colour_discrete(labels = ModelName) +
  facet_grid(Index~DemandType, scales = "free_y", 
             labeller = labeller(Index = as_labeller(IndexName, default = label_parsed), 
                                 DemandType = c(Flat = "Demanda constante", 
                                                Real = "Demanda observada"))) + 
  theme_light() + theme(legend.position = "bottom") + 
  scale_x_continuous(labels = scales::label_percent()) +
  labs(y = "Índice de diversidade", x = "Desvio padrão (% da potência)", color = "Modelo")
ggsave("figuras/4.8 - Diversificação só EOL e FV.pdf", width = 15, height = 13, units = "cm")

# Semelhança entre portfólios (figura 4.9) ---------------------------------------------
IdNomes <- ExecParam %>% select(idExec, NomeTese) %>% drop_na() %>% deframe()

CorGenShare <- left_join(ExecParam, MainResults) %>% left_join(PlantResults) %>% 
  filter(idExec %in% Selec, Type != "Load") %>% 
  select(idExec, Sim, Plant, CapShare) %>% mutate(portfolio = paste0(idExec, "-", Sim)) %>% 
  pivot_wider(id_cols = -c(idExec, Sim), names_from = portfolio, values_from = CapShare) %>% 
  select(-Plant) %>% correlate(diagonal = 1) %>% stretch(remove.dups = FALSE) 

SimuNames <- c(`1` = "Mais baixo",
               `25` = "Intermediário",
               `51` = "Mais alto")

#Semelhança de composição dos ativos
CorGenShare %>% separate(x, into = c("idExec.x", "Sim.x")) %>% 
  separate(y, into = c("idExec.y", "Sim.y")) %>% 
  mutate(Sim.y = as.numeric(Sim.y), idExec.x = as.numeric(idExec.x), idExec.y = as.numeric(idExec.y)) %>% 
  # Apenas algumas carteiras e sem usar G-Cap como facet
  filter(Sim.y %in% c(1, 25, 51)) %>% 
  left_join(ExecParam %>% select(idExec.y = idExec, NomeTese, DemandType.y = DemandType)) %>% 
  left_join(ExecParam %>% select(idExec.x = idExec, OptType, DemandType.x = DemandType)) %>% 
  group_by(idExec.y, Sim.y, DemandType.x, DemandType.y, OptType) %>% 
  summarise(minR = min(r), maxR = max(r)) %>% 
  ggplot(aes(x = "", ymin = minR, ymax = maxR, color = OptType, linetype = DemandType.x)) + 
  geom_linerange(size = 0.8, position = position_dodge(width = 0.8)) + 
  geom_point(aes(y = minR), size = 2, position = position_dodge(width = 0.8)) + 
  geom_point(aes(y = maxR), size = 2, position = position_dodge(width = 0.8)) +
  facet_grid(Sim.y~idExec.y, labeller = labeller(Sim.y = SimuNames, 
                                                 idExec.y = as_labeller(IdNomes, default = label_parsed)))  +
  scale_color_discrete(labels = ModelName) + 
  scale_linetype_discrete(labels = c(Flat = "Constante", Real = "Observada")) +
  labs(color = "Modelo", linetype = "Tipo de demanda", y = "Correlação da composição", x = "") + 
  theme_light() + theme(legend.position = "bottom")
ggsave("figuras/4.9 - Semelhança - composição.pdf", width = 15, height = 13, units = "cm", scale = 1.5)

# Custo para atender risco (figura 4.10) -----
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind, PV; real load"
         | AvailPlants == "Wind, PV; flat load"
         #| AvailPlants == "Wind, PV; flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()
Risk <- 1 - 0.95
LoadMName <- c("Wind, PV; flat load" = "Modelo com carga constante", 
               "Wind, PV; real load" = "Modelo com carga observada")
ggplot(left_join(ExecParam, MainResults) %>% 
         select(-any_of(colnames(MultiplierResults %>% 
                                   ungroup() %>% select(-c(idExec, Sim))))) %>% 
         left_join(MultiplierResults, by = c("idExec", "Sim")) %>% 
         filter(idExec %in% Selec, near(`Risk (%)`, Risk * 100)), 
       aes(x = sqrt(W_Variance_no_Load) * Multiplier, y = W_Cost * Multiplier, 
           color = OptType, linetype = MultLoadType)) + 
  geom_path() + theme_light() + 
  theme(legend.position = "bottom", legend.margin = margin(3, 0, 3, 3)) + 
  scale_y_log10(limits = c(NA, 800)) + 
  scale_x_log10(limits = c(NA, 3.00), labels = scales::percent) + 
  facet_grid(~AvailPlants, labeller = labeller(AvailPlants = LoadMName)) + 
  labs(x = "Desvio padrão", y = "Custo por MWh da carga (R$/MWh)", color = "Modelo",
       linetype = "Carga atendida") + 
  scale_color_discrete(labels = ModelName) + 
  scale_linetype_discrete(labels = c(Flat = "Constante", Real = "Observada")) + 
  guides(colour = guide_legend(ncol = 2), linetype = guide_legend(ncol = 2))
ggsave("figuras/4.10 - Custo-risco fixo.pdf", width = 15, height = 9, units = "cm")

# Distribuição por carteira (figura 4.11) -----------------------------------------------
#PlotCodenames <- c("A2", "B2", "C2")
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind, PV; real load"
         #| AvailPlants == "Wind, PV; flat load"
         #| AvailPlants == "Wind, PV; flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By ES
  ) %>% pull(idExec) %>% unique()

x <- left_join(ExecParam, MainResults) %>% 
  filter(idExec %in% Selec, Sim %in% c(1, 25, 51)) %>% 
  left_join(MultiplierResults, by = c("idExec", "Sim")) %>% 
  filter(near(`Risk (%)`, Risk * 100), MultLoadType == "Real") %>% 
  select(idExec, Sim, Multiplier)

# Calculate balances
balances <- map_dfr(1:nrow(x), 
                    ~CalcResul1Balance(x[[., "idExec"]], 
                                       x[[., "Sim"]], 
                                       Series, 
                                       x[[., "Multiplier"]])) %>% 
  rename(BalanceSame = Balance)
balances <- left_join(balances, map_dfr(1:nrow(x), ~CalcResul1Balance(x[[., "idExec"]], 
                                                                      x[[., "Sim"]],
                                                                      Series)) %>% 
                        rename(BalanceOrig = Balance))
# Percentile, mean shortage, mean positive values:
Probab <- balances %>% group_by(idExec, Sim) %>% 
  summarise(percentile5 = quantile(BalanceSame, Risk), 
            ShortageValue = mean(pmin(BalanceSame, 0)), 
            ShortageProb = length(BalanceSame[BalanceSame < 0]) / n(), 
            PosiLowValue = mean(pmin(pmax(BalanceSame, 0), percentile5) - pmin(BalanceSame - percentile5, 0) - percentile5), 
            PosiLowProb = length(BalanceSame[BalanceSame >= 0 & BalanceSame < percentile5]) / n(), 
            Low5Mean = mean(BalanceSame[BalanceSame < percentile5]), 
            Short = ShortageValue / ShortageProb, PosiLow = PosiLowValue / PosiLowProb)

SummaryData <- Probab %>% 
  left_join(left_join(balances, ExecParam) %>% 
              group_by(idExec, OptType, AvailPlants, NomeTese, Sim) %>% 
              summarise(MeanBal = mean(BalanceSame), yvalue = CalcCumDens(BalanceSame / AveLoad, quant = Risk)))

ggplot(left_join(balances, ExecParam), aes(x = BalanceSame / AveLoad, color = factor(Sim))) +
  geom_density() +
  geom_point(aes(x = percentile5 / AveLoad, color = factor(Sim), y = yvalue), SummaryData) +
  geom_vline(aes(xintercept = MeanBal / AveLoad, color = factor(Sim)), linetype = 2, SummaryData) + 
  geom_vline(aes(xintercept = 0), alpha = 0.3) + 
  geom_label_repel(aes(x = MeanBal / AveLoad, label = scales::percent(MeanBal / AveLoad, accuracy = 1), y = 0.4), SummaryData) +
  facet_grid(NomeTese~., labeller = label_parsed) + theme_light() + 
  labs(x = "Balanço", y = "Densidade de probabilidade", color = "Desvio padrão") +
  scale_color_discrete(labels = SimuNames) + theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::label_percent()) + 
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))
ggsave("figuras/4.11 - Balanço por portfólio.pdf", width = 15, height = 10, units = "cm", scale = 1.5)

# Fronteiras com controláveis (figuras 4.12, 4.13 e 4.14) ------
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         #| OptType == "MinCost_IsoCap" # Menor custo por kW, maior FC se tiver o mesmo custo por kW.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         #| OptType == "MaxGen_IsoCost" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         #| OptType == "MinCap_IsoGen" # Maior FC.
         # , AvailPlants == ""           # By load type
         # | AvailPlants == "Wind, PV; real load"
         , !AvailPlants %in% c("Wind, PV; flat load, PV half cost", "Wind, PV; real load, PV half cost")
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By omega
         , ControllableVersion %in% c(0, 2)
  ) %>% pull(idExec) %>% unique()

# (figura 4.12)
ggplot(PlotDataCompare %>% 
         filter(idExec %in% Selec), 
       aes(x = sqrt(W_Variance_no_Load) / SumRC2L, y = W_Cost / SumWc,
           color = paste0(HasThermal, HasStorage), linetype = DemandType)) + 
  facet_wrap(~OptType, scales = "free_x", labeller = labeller(OptType = ModelName)) +
  geom_path() + 
  scale_x_continuous(labels = scales::label_percent()) +
  scale_linetype_discrete(labels = c(Flat = "Constante", Real = "Observada")) +
  scale_color_discrete(labels = c(`FALSEFull cost` = "Armaz.", FALSENo = "Nenhuma", `TRUEFull cost` = "Armaz. e term.")) +
  labs(x = "Desvio padrão sem demanda (% da potência)", y = "Custo por geração do portfólio (R$/MWh)",
       linetype = "Tipo de demanda", color = "Controlável") + 
  theme_light() + theme(legend.position = "bottom")
ggsave("figuras/4.12 - Fronteiras com controláveis - todos.pdf", width = 15, height = 11, units = "cm", scale = 1.35)
# (figura 4.13)
ggplot(PlotDataCompare %>% 
         filter(idExec %in% Selec, OptType == "MaxGen_IsoCap"), 
       aes(x = PortSD, y = SumWc / SumRC2L,
           color = paste0(HasThermal, HasStorage), linetype = DemandType)) + 
  facet_wrap(~OptType, scales = "free_x", labeller = labeller(OptType = ModelName)) +
  geom_path() + 
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_linetype_discrete(labels = c(Flat = "Constante", Real = "Observada")) +
  scale_color_discrete(labels = c(`FALSEFull cost` = "Armaz.", FALSENo = "Nenhuma", `TRUEFull cost` = "Armaz. e term.")) +
  labs(x = "Desvio padrão (% da potência)", y = "Fator de capacidade",
       linetype = "Tipo de demanda", color = "Controlável") + 
  theme_light() + theme(legend.position = "bottom")
ggsave("figuras/4.13 - Fronteiras com controláveis - maxFC.pdf", width = 15, height = 11, units = "cm", scale = 1.35)
# (figura 4.14)
ggplot(PlotDataCompare %>% 
         filter(idExec %in% Selec, OptType != "MaxGen_IsoCap"), 
       aes(x = sqrt(W_Variance_no_Load) / SumWc, y = W_Cost,
           color = paste0(HasThermal, HasStorage), linetype = DemandType)) + 
  facet_wrap(~OptType, scales = "free_x", labeller = labeller(OptType = ModelName)) +
  geom_path() + 
  scale_x_continuous(labels = scales::label_percent()) +
  scale_linetype_discrete(labels = c(Flat = "Constante", Real = "Observada")) +
  scale_color_discrete(labels = c(`FALSEFull cost` = "Armaz.", FALSENo = "Nenhuma", `TRUEFull cost` = "Armaz. e term.")) +
  labs(x = "Desvio padrão sem demanda (% da geração esperada)", y = "Custo por demanda (R$/MWh)",
       linetype = "Tipo de demanda", color = "Controlável") + 
  theme_light() + theme(legend.position = "bottom")
ggsave("figuras/4.14 - Fronteiras com controláveis - minCusto.pdf", width = 15, height = 11, units = "cm", scale = 1.35)

# Capacidade instalada por modelo e configuração (tabela) -----------------
# Considerando capacidade das baterias
PlotDataCompare |> filter(idExec %in% Selec) |> 
  summarise(média = mean(SumRC2L), mediana = median(SumRC2L), máximo = max(SumRC2L), 
            mínimo = min(SumRC2L), 
            .by = c(idExec, OptType, DemandType, HasThermal, HasStorage))
## Sem considerar capacidade das baterias
left_join(select(left_join(ExecParam, MainResults), !any_of(c("SumWc", "SumWr", "SumRC2L", "SumCapacity"))), 
          group_by(filter(PlantResults, !Type %in% c("Load", "Storage")), idExec, Sim) %>% 
            summarise(SumWc = sum(Wc), SumWr = sum(Wr), SumRC2L = sum(RelCaptoLoad), 
                      SumCapacity = sum(Capacity)), by = c("idExec", "Sim")) |> 
  filter(idExec %in% Selec) |> ungroup() |> 
  summarise(média = mean(SumRC2L), mediana = median(SumRC2L), máximo = max(SumRC2L), 
            mínimo = min(SumRC2L), .by = c(idExec, OptType, DemandType, HasThermal, HasStorage))

# Participação de eólica por região -----
PlantResults |> filter(idExec %in% Selec, Type == "Wind") |> 
  mutate(Sist = if_else(UF %in% c("RS", "SC", "PR"), "Sul", "Nordeste")) |> 
  group_by(idExec, Sim, Sist) |> summarise(Cap = sum(RelCaptoLoad)) |> 
  mutate(CapRel = Cap / sum(Cap)) |> filter(Sist == "Nordeste") |> group_by(idExec) |> 
  summarise(max = max(CapRel), min = min(CapRel)) |> left_join(ExecParam) |> 
  arrange(OptType, DemandType, HasThermal, HasStorage) |> 
  select(idExec, OptType, DemandType, HasThermal, HasStorage, min, max)
## Composição dos portfólios - área (figura 4.15) ------
NomesType2 <- c(Wind = "Eólica", PV = "FV", StorageL = "Armaz. Carga", StorageP = "Armaz. FV", StorageW = "Armaz. Eol",
                ThermalP = "Térmica FV", ThermalW = "Térmica Eol", ThermalL = "Térmica Carga", ThermalFlat = "Térmica inflex.")
pa <- PlotDataCompare %>% left_join(PlantResults) %>%  
  left_join(PlantData, by = c("Plant" = "name", "Type", "UF", "Storage capacity")) %>% 
  filter(idExec %in% Selec, Type != "Load") %>% 
  group_by(idExec, Sim, Type, Type2, DemandType, OptType, HasStorage, HasThermal) %>% 
  summarise(across(c(W_Cost, W_SD, SumRC2L, SumWc, W_Variance_no_Load),
                   \(x) first(x)),
            across(c(GenShare, CapShare, RelCaptoLoad, Wc, Wr),
                   \(x) sum(x)))

pa %>% filter(DemandType == "Real") %>% ungroup() %>% 
  mutate(Type2 = fct_relevel(Type2, "StorageL", "StorageP", "StorageW", "PV", "Wind")) %>% 
  ggplot() + geom_area(aes(y = CapShare, fill = Type2, x = W_Cost / SumWc)) + 
  facet_grid(OptType~paste0(HasThermal, HasStorage),
             labeller = labeller(OptType = ModelName,
                                 .cols = c(`FALSEFull cost` = "Armaz.", FALSENo = "Sem controláveis", `TRUEFull cost` = "Armaz. e term."))) + 
  scale_fill_brewer(palette = "PuOr", labels = NomesType2) +
  scale_y_continuous(labels = scales::label_percent()) + 
  theme_light() + labs(fill = "Tecnologia", y = "% da capacidade instalada do portfólio", 
                       x = "Custo por geração do portfólio (R$/MWh)")
ggsave("figuras/4.15 - Composição - Área, carga real.pdf", width = 15, height = 12, units = "cm", scale = 1.35)
# Decomposição de baterias e termelétricas por tempo de armazenamento/profundidade (figura 4.16) -----
PlotDataCompare %>% left_join(PlantResults) %>%  
  left_join(PlantData, by = c("Plant" = "name", "Type", "UF", "Storage capacity")) %>% 
  filter(idExec %in% Selec, Type != "Load") %>% 
  group_by(idExec, Sim, Type, DemandType, OptType, HasStorage, HasThermal, 
           `Storage capacity`, ThermPctLim) %>% 
  summarise(across(c(W_Cost, W_SD, SumRC2L, SumWc, W_Variance_no_Load),
                   \(x) first(x)),
            across(c(GenShare, CapShare, RelCaptoLoad, Wc, Wr),
                   \(x) sum(x))) %>% ungroup() %>% 
  mutate(ThermPctLim = factor(ThermPctLim, labels = c(1, 2, 3)), 
         `Storage capacity` = factor(`Storage capacity`, 
                                     labels = c(1, 2, 3))) %>% 
  filter(DemandType == "Real", Type %in% c("Storage", "Thermal")) %>%
  ggplot() + geom_area(aes(y = CapShare, fill = Type, 
                           alpha = coalesce(`Storage capacity`, ThermPctLim), 
                           x = W_Cost / SumWc)) + 
  facet_grid(OptType~paste0(HasThermal, HasStorage),
             labeller = labeller(OptType = ModelName,
                                 .cols = c(`FALSEFull cost` = "Armaz.", 
                                           FALSENo = "Sem controláveis", 
                                           `TRUEFull cost` = "Armaz. e term."))) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  theme_light() + labs(fill = "Tecnologia", y = "% da capacidade instalada do portfólio", 
                       x = "Custo por geração do portfólio (R$/MWh)", alpha = "Categoria") + 
  scale_alpha_discrete(range = c(0.33, 1), labels = c("1h ou 10%", "5h ou 30%", "25h ou 60%")) +
  scale_fill_discrete(labels = c(Storage = "Armazenamento", Thermal = "Térmica")) +
  theme(legend.position = "bottom")
ggsave("figuras/4.16 - Decomposição por tempo-profundidade, carga real.pdf", width = 15, height = 12, units = "cm", scale = 1.35)
# Diversificação com controláveis (figura 4.17) ------------
pr <- group_by(left_join(PlantResults, PlantData, by = c("Plant" = "name", "Type")), idExec, Sim)
DivIndex_semresidual <- bind_rows(
  ## For all plants
  ### HHI and Shannon entropy
  pr %>% 
    filter(Type != "Load", CapShare > 1e-6) %>% 
    mutate(CapShare = CapShare / sum(CapShare), GenShare = GenShare / sum(GenShare)) %>% # Normalize
    summarise(HHI_Cap = sum(CapShare^2), HHI_Gen = sum(GenShare^2),
              Shannon_Cap = -sum(CapShare * log(CapShare)), 
              Shannon_Gen = -sum(GenShare * log(GenShare))) %>% 
    pivot_longer(-c(idExec, Sim), names_to = c("Index", "By"), names_sep = "_", values_to = "Value") %>% 
    mutate(WhichPlants = "All"),
  # For renewables only
  ### HHI and Shannon entropy
  pr %>% 
    filter(Type %in% c("Wind", "PV"), CapShare > 1e-6) %>% 
    mutate(CapShare = CapShare / sum(CapShare), GenShare = GenShare / sum(GenShare)) %>% # Normalize
    summarise(HHI_Cap = sum(CapShare^2), HHI_Gen = sum(GenShare^2),
              Shannon_Cap = -sum(CapShare * log(CapShare)), 
              Shannon_Gen = -sum(GenShare * log(GenShare))) %>% 
    pivot_longer(-c(idExec, Sim), names_to = c("Index", "By"), names_sep = "_", values_to = "Value") %>% 
    mutate(WhichPlants = "Wind and PV"), 
  DivIndex |> filter(Index %in% c("GeoDiv", "EucDiv"))) %>% arrange(idExec, Sim)

ggplot(left_join(ExecParam, MainResults) %>% left_join(DivIndex_semresidual) %>% 
         filter(idExec %in% Selec, By == "Gen", DemandType == "Real") %>% 
         mutate(invValue = case_when(Index == "Shannon" ~ exp(Value),
                                     Index == "HHI" ~ 1 / Value,
                                     TRUE ~ Value))) +
  geom_path(aes(x = sqrt(W_Variance_no_Load) / SumRC2L * 100, 
                y = invValue, color = paste0(HasThermal, HasStorage), linetype = WhichPlants)) + 
  facet_grid(Index~OptType, scales = "free_y", 
             labeller = labeller(Index = as_labeller(IndexName, default = label_parsed), OptType = ModelName)) + 
  scale_x_continuous(labels = scales::label_percent()) +
  scale_color_discrete(labels = c(`FALSEFull cost` = "Armaz.", FALSENo = "Nenhuma", `TRUEFull cost` = "Armaz. e term.")) +
  scale_linetype_discrete(labels = c(All = "Todas", `Wind and PV` = "Eólicas e fotovoltaicas")) +
  theme_light() + theme(legend.position = "bottom") + 
  labs(y = "Índice de diversidade", color = "Controlável", x = "Desvio padrão (% da potência)", linetype = "Conjunto de usinas")
ggsave("figuras/4.17 - Diversificação com controláveis.pdf", width = 15, height = 11, units = "cm", scale = 1.35)

# Custo para atender risco com controláveis (figura 4.18) ------
# Ainda precisa ajeitar algumas coisas.
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         #| OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         , !AvailPlants %in% c("Wind, PV; flat load, PV half cost", "Wind, PV; real load, PV half cost")
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By omega
         , ControllableVersion %in% c(0, 2)
  ) %>% pull(idExec) %>% unique()
Risk <- 1 - 0.95

ggplot(left_join(ExecParam, MainResults) %>% 
         select(-any_of(colnames(MultiplierResults %>% 
                                   ungroup() %>% select(-c(idExec, Sim))))) %>% 
         # Apenas multiplicadores do mesmo tipo que a carga
         left_join(MultiplierResults, by = c("idExec", "Sim", "DemandType" = "MultLoadType")) %>% 
         filter(idExec %in% Selec, near(`Risk (%)`, Risk * 100)), 
       aes(x = sqrt(W_Variance_no_Load) * Multiplier, y = W_Cost * Multiplier, 
           color = OptType, linetype = paste0(HasThermal, HasStorage))) + 
  geom_path() + theme_light() + 
  theme(legend.position = "bottom", legend.margin = margin(3, 0, 3, 3)) + 
  scale_y_log10(limits = c(NA, 800)) + 
  scale_x_continuous(limits = c(NA, 3.00), labels = scales::percent) + 
  facet_grid(~DemandType, labeller = 
               labeller(DemandType = c(Flat = "Constante", Real = "Observada"))) + 
  labs(x = "Desvio padrão", y = "Custo por MWh da carga (R$/MWh)", color = "Modelo",
       linetype = "Controláveis") + 
  scale_color_discrete(labels = ModelName) + 
  scale_linetype_discrete(labels = c(`FALSEFull cost` = "Armaz.", 
                                     FALSENo = "Nenhuma", 
                                     `TRUEFull cost` = "Armaz. e term.")) 
ggsave("figuras/4.18 - Custo-risco fixo com controláveis.pdf", width = 15, height = 10, units = "cm", scale = 1.25)

# Semelhança entre portfólios com controláveis considerando apenas renováveis (figura 4.19) ---------------------------------------------
IdNomes <- ExecParam %>% select(idExec, NomeTese) %>% drop_na() %>% deframe()

CorGenShare <- left_join(ExecParam, MainResults) %>% left_join(PlantResults) %>% 
  filter(idExec %in% Selec, Type %in% c("Wind", "PV"), DemandType == "Flat", OptType == "MinCost_IsoRisk") %>% 
  select(idExec, Sim, Plant, CapShare) %>% group_by(idExec, Sim) %>% 
  mutate(portfolio = paste0(idExec, "-", Sim), CapShare = CapShare / sum(CapShare)) %>% 
  pivot_wider(id_cols = -c(idExec, Sim), names_from = portfolio, values_from = CapShare) %>% 
  select(-Plant) %>% correlate(diagonal = 1) %>% stretch(remove.dups = FALSE) 

CorGenSharePlot <- CorGenShare %>% separate(x, into = c("idExec.x", "Sim.x")) %>% 
  separate(y, into = c("idExec.y", "Sim.y")) %>% 
  mutate(Sim.x = as.numeric(Sim.x), Sim.y = as.numeric(Sim.y), idExec.x = as.numeric(idExec.x), idExec.y = as.numeric(idExec.y)) %>% 
  left_join(left_join(ExecParam, MainResults), by = c("idExec.x" = "idExec", "Sim.x" = "Sim")) %>% 
  left_join(left_join(ExecParam, MainResults), by = c("idExec.y" = "idExec", "Sim.y" = "Sim"))

#Semelhança de composição dos ativos
CorGenSharePlot %>% group_by(idExec.y) %>% 
  filter(Sim.y %in% c(1, 25, max(Sim.y))) %>% mutate(Sim.y = if_else(Sim.y == 50, 51, Sim.y)) %>% 
  group_by(idExec.y, Sim.y, HasStorage.x, HasThermal.x, HasStorage.y, HasThermal.y, DemandType.x, DemandType.y, OptType.x) %>% 
  summarise(minR = min(r), maxR = max(r)) %>% 
  ggplot(aes(x = "", ymin = minR, ymax = maxR, 
             color = paste0(HasThermal.x, HasStorage.x) %>% factor(levels = c("FALSENo", "FALSEFull cost", "TRUEFull cost")))) + 
  geom_linerange(linewidth = 0.8, position = position_dodge(width = 0.8)) + 
  geom_point(aes(y = minR), size = 2, position = position_dodge(width = 0.8)) + 
  geom_point(aes(y = maxR), size = 2, position = position_dodge(width = 0.8)) +
  facet_grid(Sim.y~paste0(HasThermal.y, HasStorage.y) %>% factor(levels = c("FALSENo", "FALSEFull cost", "TRUEFull cost")), 
             labeller = labeller(Sim.y = SimuNames, 
                                 .cols = c(`FALSEFull cost` = "Armaz.", 
                                           FALSENo = "Sem controláveis", 
                                           `TRUEFull cost` = "Armaz. e term.")))  +
  scale_color_discrete(breaks = c("FALSENo", "FALSEFull cost", "TRUEFull cost"), 
                       labels = c("Sem controláveis", "Armaz.", "Armaz. e term.")) + 
  labs(color = "Controláveis", linetype = "Tipo de demanda", y = "Correlação da composição", x = "") + 
  theme_light() 
ggsave("figuras/4.19 - Semelhança controláveis demanda constante - composição.pdf", width = 15, height = 10, units = "cm")
