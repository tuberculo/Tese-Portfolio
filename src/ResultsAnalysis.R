SeriesWindSolar <- SeriesComplete[deframe(PlantData[PlantData$Type %in% c("Wind", "PV", "Load") & PlantData$Used, "name"])]
rm(SeriesComplete)
# attach(readRDS("results/Resultados-20200923_X50.rds"))
#attach(readRDS("results/Resultados-20200923_X50-pós cálculos.rds")) # Versão com mais parâmetros calculados após as otimizações (HHI, DivIndex, Variance_noload)
ExecParam <- mutate(ExecParam, OptType = factor(case_when(str_detect(ExecParam$Notes, "MinCost_IsoRisk") ~ "MinCost_IsoRisk",
                                             str_detect(ExecParam$Notes, "MaxGen_IsoCap") ~ "MaxGen_IsoCap",
                                             str_detect(ExecParam$Notes, "MinCost_IsoCap") ~ "MinCost_IsoCap",
                                             str_detect(ExecParam$Notes, "MinCost_IsoGen") ~ "MinCost_IsoGen",
                                             str_detect(ExecParam$Notes, "MaxGen_IsoCost") ~ "MaxGen_IsoCost",
                                             str_detect(ExecParam$Notes, "MinCap_IsoGen") ~ "MinCap_IsoGen",)))
attach(readRDS("results/Resultados-20201220_Nora-pós-cálculos.rds")) # Versão com MinCap_IsoGen e com mais parâmetros calculados após as otimizações (HHI, DivIndex, Variance_noload, multiplicador para diferentes níveis de risco).

Selec <- ExecParam %>% 
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" # Igual a MinCost_IsoGen quando beta = 0.
         | OptType == "MaxGen_IsoCap" # Maior FC.
         | OptType == "MinCost_IsoCap" # Menor custo por kW, maior FC se tiver o mesmo custo por kW.
         | OptType == "MinCost_IsoGen" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         | OptType == "MaxGen_IsoCost" # Menor custo por MWh -> maior FC para usinas de mesmo investimento.
         | OptType == "MinCap_IsoGen" # Maior FC.
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind and PV, real load" 
         | AvailPlants == "Wind and PV, flat load"
         , Beta %in% c(0.95, 0.9, 0.5, 0) # By Beta
         , omega %in% c(0, -10000, -25000, -50000, -75000, -90000) # By omega
  ) %>% pull(idExec) %>% unique() 

# Plots ----------------------------------------------------------------

ggplot() + geom_col(filter(left_join(ExecParam, left_join(MainResults, PlantResults)), str_detect(Plant, "Demanda", negate = TRUE), idExec %in% Selec), 
                    mapping = aes(x = Simulação, y = CapShare, fill = Type)) + facet_wrap(~Notes, nrow = 4)

# PV share with and without load.
ggplot() + 
  geom_line(filter(left_join(ExecParam, left_join(MainResults, PlantResults)), 
                   str_detect(Plant, "Demanda", negate = TRUE), idExec %in% Selec) %>% 
              group_by(idExec, OptType, AvailPlants, Simulação, W_Variance_no_Load, 
                       W_SD, SumRC2L, Type) %>% summarise(SumCapShare = sum(CapShare), 
                                                          SumGenShare = sum(GenShare)) %>% 
              filter(Type == "PV"), mapping = aes(x = sqrt(W_Variance_no_Load) / SumRC2L * 100, 
                                                  y = SumCapShare, color = OptType, linetype = AvailPlants))

ggplot(filter(MainResults, idExec == idExe), aes(x = Variance, y = Cost)) + geom_line() + geom_point() + labs(x = "Variance", y = "Cost (M R$)")

ggplot() + geom_col(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                           idExec == idExe), mapping = aes(x = Cap_SD, y = Capacity, fill = Type)) + labs(x = "Standard deviation", y = "Capacity (MW)")
ggplot() + geom_col(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE)), 
                    mapping = aes(x = Simulação, y = CapShare, fill = Type)) + facet_wrap(~idExec, nrow = 4)
# Excluindo térmica flat com total = 1.
ggplot() + geom_col(filter(left_join(left_join(ExecParam, MainResults), PlantResults) %>% filter(idExec %in% Selec), 
                           str_detect(Plant, "Demanda", negate = TRUE), Plant != "UTE_Inflex."), 
                    mapping = aes(x = Simulação, y = CapShare, fill = Type), position = "fill") + 
  facet_grid(AvailPlants ~ OptimParam, labeller = labeller(AvailPlants = label_wrap_gen(), OptimParam = label_wrap_gen(15)))

ggplot() + geom_col(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                           idExec == idExe), mapping = aes(x = W_SD, y = Wc, fill = Type)) + labs(x = "Standard deviation", y = "Gen relative to load")
ggplot() + geom_col(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                           idExec == idExe), mapping = aes(x = W_SD, y = CapShare, fill = Type)) 

# Frontiers ---------------------------------------------------------------

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = W_SD * 100, y = W_Cost, group = Notes, color = Notes)) + 
  geom_line() + geom_point() + labs(x = "Standard deviation to load (%)", y = "Cost to load ($/MWh)") #+ scale_color_brewer(palette = "Dark2")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = W_SD * 100, y = SumWc / SumRC2L * 100, group = paste0(AvailPlants, OptimParam), 
                  color = OptimParam)) + 
  geom_line() + geom_point() + labs(x = "Standard deviation to load (%)", y = "Capacity factor (%)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = W_SD * 100, y = W_Cost / SumWc, group = paste0(AvailPlants, OptimParam), # Este "y" equivale a Cap_Cost * 1e6 / 8760 / MeanGen
                  color = OptimParam)) + 
  geom_path() + geom_point() + labs(x = "Standard deviation to load (%)", y = "Generation cost ($/MWh)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = W_SD / SumRC2L * 100, y = W_Cost / SumWc, group = paste0(AvailPlants, OptimParam), # Este "y" equivale a Cap_Cost * 1e6 / 8760 / MeanGen
                  color = OptimParam)) + 
  geom_path() + labs(x = "Relative standard deviation (%)", y = "Generation cost ($/MWh)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = SumWc / SumRC2L * 100, y = W_Cost / SumWc, group = paste0(AvailPlants, OptimParam), # "x" = capacity factor
                  color = OptimParam)) + 
  geom_path() + geom_point() + labs(x = "Capacity factor (%)", y = "Generation cost ($/MWh)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = SumWc / SumRC2L * 100, y = W_Cost, group = paste0(AvailPlants, OptimParam), # "x" = capacity factor
                  color = OptimParam)) + 
  geom_path() + geom_point() + labs(x = "Capacity factor (%)", y = "Cost to load ($/MWh)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = sqrt(W_Variance_no_Load) * 100, y = SumWc / SumRC2L * 100, # no load
                  color = OptimParam, linetype = AvailPlants)) + 
  geom_line() + labs(x = "SD to load (no load) (%)", y = "Capacity factor (%)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = W_SD / SumRC2L * 100, y = SumWc / SumRC2L * 100, 
                  color = OptimParam, linetype = AvailPlants)) + geom_path() +
  labs(x = "Relative SD (%)", y = "Capacity factor (%)")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = sqrt(W_Variance_no_Load) / SumRC2L * 100, y = SumWc / SumRC2L * 100, 
                  color = OptimParam, linetype = AvailPlants)) + geom_path() +
  labs(x = "Relative SD (no load) (%)", y = "Capacity factor (%)")

# Este aqui é para mostra que, a partir de certo ponto, a solução por IsoCap pode estar dominada se considerar IsoGen.
ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), 
       aes(x = W_SD / SumWc * 100, y = SumWc / SumRC2L * 100, 
           color = OptimParam, linetype = AvailPlants)) + geom_path() +
  labs(x = "SD por carga (%)", y = "Capacity factor (%)")

ggplot() + geom_path(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), 
                     mapping = aes(x = W_SD / SumRC2L * 100, y = SumWc / SumRC2L * 100,
                                   color = OptimParam, linetype = AvailPlants)) + 
  geom_point(left_join(PlantData, VariancePlant %>% enframe(value = "Variance")) %>% filter(Used, Type != "Load"), 
             mapping = aes(x = sqrt(Variance) * 100, y = CF * 100, colour = Type)) 

# Gráfico de custo por MWh para o mesmo risco.
# Calculado pelo valor do CVaR, só funciona para carga flat.
ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), 
       aes(x = W_SD * (1 / (1 + W_CVaR)) * 100, y = W_Cost * (1 / (1 + W_CVaR)), 
           color = OptType, linetype = factor(beta), group = OptimParam)) + 
  geom_path() + theme_light() + 
  theme(legend.position = "bottom") + scale_y_log10() + scale_x_log10()

# Gráfico de custo por MWh para o mesmo risco.
# A partir do multiplicador calculado. Funciona para todos os casos.
Risk <- 1 - beta
riskname <- sym(paste0("SameRiskMulti_", Risk * 100, "%"))
ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), 
       aes(x = W_SD * !!riskname * 100, y = W_Cost * !!riskname, 
           color = OptType, group = idExec, 
           linetype = factor(Beta, levels = sort(unique(Beta), decreasing = TRUE)))) + 
  geom_path() + theme_light() + 
  theme(legend.position = "bottom") + scale_y_log10(limits = c(NA, 1000)) + 
  scale_x_log10(limits = c(NA, 300)) + facet_grid(~AvailPlants) 

ggplot() + geom_density(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                               idExec %in% c(1, 2, 3)) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")), 
                        mapping = aes(x = CF.pos, fill = Type, weight = Wc), alpha = 0.4, n = 30) + facet_grid(Simulação ~ idExec)

ggplot() + geom_point(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                             idExec == idExe) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")) %>% filter(Wc > 0.01), 
                      mapping = aes(x = Wc, y = CF.pos, colour = Type)) + facet_wrap(~Simulação) + scale_color_brewer(palette = "Dark2")
ggplot() + geom_point(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                             idExec == idExe) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")) %>% filter(Wc > 0.001), 
                      mapping = aes(x = Wc, y = CustoMWh, colour = Type)) + facet_wrap(~Simulação) + scale_color_brewer(palette = "Dark2")
ggplot() + geom_bin2d(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                             idExec == idExe) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")) %>% filter(Wc > 0.001), 
                      mapping = aes(x = Wc, y = CustoMWh, group = Type)) + facet_wrap(~Simulação)
ggplot() + geom_col(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                           idExec == idExe) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")) %>% filter(Wc > 0.001), 
                    mapping = aes(y = CapShare, x = CustoMWh, fill = Type)) + facet_wrap(~Simulação) + scale_color_brewer(palette = "Dark2") +scale_x_binned(n.breaks = 30)
ggplot() + geom_col(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                           idExec == idExe) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")), 
                    mapping = aes(y = CapShare, x = CF.pos, fill = Type)) + facet_wrap(~Simulação) + scale_color_brewer(palette = "Dark2") +scale_x_binned(n.breaks = 50)
ggplot() + geom_density(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), 
                               idExec == idExe) %>% left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")) %>% filter(Wc > 0.001), 
                        mapping = aes(x = CapShare, fill = Type), alpha = 0.4, n = 30) + facet_wrap(~Simulação)
# Não funciona por problema no ggplot:
ggplot(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), idExec %in% Selec, Simulação %in% c(1, 10, 20, 25, 30, 40, 51)) %>% 
         left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")), 
       mapping = aes(x = CF.pos, weight = GenShare, fill = Type)) + 
  geom_density(alpha = 0.4, n = 30) + facet_grid(Simulação ~ idExec)
#Alternativa: geom_freqpoly
ggplot(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), idExec %in% Selec, Simulação %in% c(1, 10, 20, 25, 30, 40, 51)) %>% 
         left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")), 
       mapping = aes(x = CF.pos, weight = GenShare, color = Type)) + 
  geom_freqpoly(alpha = 0.4, binwidth = 0.06) + facet_grid(Simulação ~ idExec)

ggplot(filter(left_join(MainResults, PlantResults), str_detect(Plant, "Demanda", negate = TRUE), idExec %in% c(1, 2, 3, 4, 5, 11, 12, 13, 14, 15)) %>% 
         left_join(PlantData, by = c("Plant" = "name", "Type" = "Type")), 
       mapping = aes(x = CF.pos, weight = Wc)) + 
  geom_density(alpha = 0.4, n = 50) + facet_grid(Simulação ~ idExec)


# Gráfico de usinas virtuais e baterias -----------------------------------
PlantNameStr <- "Cerro Chato"
colnames(SeriesComplete) %>% str_detect(PlantNameStr) -> colunas
colunas[1] <- TRUE 
Dates <- as.Date(c("01/03/2015", 
                   "05/03/2015"), "%d/%m/%Y")

SeriesComplete[colunas] %>% 
  filter(timedate >= Dates[1], timedate < Dates[2]) %>% 
  pivot_longer(-c("timedate", PlantNameStr), names_to = "Plant", values_to = "Gen") %>% 
  mutate(CombinedGen = .[[sym(PlantNameStr)]] + Gen) %>% 
  pivot_longer(c(Gen, CombinedGen), names_to = "TypeGen", values_to = "GenValue") %>% 
  ggplot(aes(x = timedate)) + 
  geom_line(aes(y = GenValue, color = Plant, linetype = TypeGen)) + 
  geom_line(aes(y = !!sym(PlantNameStr))) + facet_wrap(~Plant)
# Um pouco melhor:
SeriesComplete[colunas] %>% 
  filter(timedate >= Dates[1], timedate < Dates[2]) %>% 
  pivot_longer(-c("timedate", PlantNameStr), names_to = "Plant", values_to = "Gen") %>% 
  mutate(CombinedGen = .[[sym(PlantNameStr)]] + Gen * if_else(str_detect(Plant, "peak"), 0.1, 1)) %>% 
  pivot_longer(c(Gen, CombinedGen, all_of(PlantNameStr)), 
               names_to = "TypeGen", values_to = "GenValue") %>% 
  ggplot(aes(x = timedate)) + 
  geom_line(aes(y = GenValue, color = TypeGen)) + facet_wrap(~Plant) + 
  scale_color_manual(values = c("black", "blue", "lightblue")) + theme_minimal()

# Ciclos por ano das baterias, em função da capacidade de armazenamento.
left_join(PlantData, sapply(Series[-1], function(x) sum(x[which(x >= 0)]) / NROW(x)) %>% 
  enframe(value = "AveOut")) %>% 
  mutate(CiclosAno = AveOut * 8760 / `Storage capacity`, .after = CF.pos) %>% 
  filter(Type == "Storage") %>% 
  ggplot() + geom_jitter(aes(x = UF, y = CiclosAno, size = `Storage capacity`, color = BaseType), 
                         alpha = 0.6)

left_join(PlantData, sapply(Series[-1], function(x) sum(x[which(x >= 0)]) / NROW(x)) %>% 
            enframe(value = "AveOut")) %>% 
  mutate(CiclosAno = AveOut * 8760 / `Storage capacity`, .after = CF.pos) %>% 
  filter(Type == "Storage") %>% group_by(BaseName) %>% 
  mutate(ProporCiclo = first(CiclosAno) / CiclosAno, .after= CiclosAno) %>% filter(`Storage capacity` > 1) %>% 
  ggplot() + geom_jitter(aes(x = UF, y = ProporCiclo, size = `Storage capacity`, color = BaseType), 
                         alpha = 0.6)

# Possível figura para o paper --------------------------------------------

th <- 75000
SeriesComplete %>% select(timedate, Demanda) %>% mutate(Demanda = Demanda * -100000) %>% 
  filter(timedate > "2016-02-13", timedate < "2016-02-20") %>% 
  ggplot(aes(x = timedate, y = Demanda)) + geom_line() + 
  geom_ribbon(aes(ymax = th, ymin = min(Demanda)), alpha = 0.3) + 
  geom_ribbon(aes(ymax = pmax(Demanda, th), ymin = th), alpha = 0.3, fill = 2) + 
  geom_ribbon(aes(ymin = pmin(Demanda, th), ymax = th), alpha = 0.3, fill = 3) +
  theme_minimal()

# Participação de cada usina lado a lado ----------------------------------
# Rascunho
ggplot(PlantResults %>% filter(idExec %in% c(26, 34), !(Plant %in% c("UTE_Inflex.", "DemandaFlat")))) + 
  geom_col(aes(x = "", y = Wc, fill = Plant), position = "dodge") + facet_grid(idExec~Simulação) + theme(legend.position = "none")
# Aqui:
ph <- left_join(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), PlantResults) %>% 
              left_join(PlantData, by = c("Plant" = "name", "Type", "CustoMWh", "Storage capacity", "UF")) %>% 
              group_by(idExec) %>% mutate(SimuFactor = factor(Simulação))
Nlevels <- length(levels(ph$SimuFactor)) # Get number of levels
levels(ph$SimuFactor)[(Nlevels - 1):Nlevels] <- "Last" # Change two last labels to "Last"
ggplot(ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE), 
                     !(Plant %in% c("UTE_Inflex.")), Simulação %in% c(1, 10, 20, 25, 30, 40, last(Simulação)))) + 
  geom_histogram(aes(x = CF.pos, fill = Type, weight = Wc)) + facet_grid(OptType~SimuFactor) 

ggplot(ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE), 
                     !(Plant %in% c("UTE_Inflex.")), Simulação %in% c(1, 10, 20, 25, 30, 40, last(Simulação)))) + 
  geom_histogram(aes(x = CF.pos, fill = Type, weight = CapShare)) + 
  facet_grid(OptimParam~SimuFactor) 
ggplot(ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE), 
                     !(Plant %in% c("UTE_Inflex.")), Simulação %in% c(1, 10, 20, 25, 30, 40, last(Simulação)))) + 
  geom_histogram(aes(x = CustoMWh, fill = Type, weight = GenShare)) + 
  facet_grid(Codename~SimuFactor) 

ggplot(ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE), 
              !(Plant %in% c("UTE_Inflex.")), Simulação %in% c(1, 25, last(Simulação)))) + 
  geom_histogram(aes(x = CF.pos, fill = Type, weight = RelCaptoLoad)) + 
  facet_grid(OptType~factor(Simulação, labels = c("First", "Intermediate", "Last"))) 
ggplot(ph %>% filter(Simulação %in% c(1, 39, 40, 41, 42, 43, 50))) + 
  geom_col(aes(x = "", y = CapShare, fill = Plant), position = "dodge") + facet_grid(idExec~Simulação) + theme(legend.position = "none")
ggplot(ph %>% filter(Simulação %in% c(1, 39, 40, 41, 42, 43, 50))) + 
  geom_histogram(aes(x = CF.pos, weight = CapShare, fill = Plant), bins = 40) + facet_grid(idExec~Simulação) + theme(legend.position = "none")

# Tamanho do ponto é a participação da usina.
# Para um idExec
ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% 
               filter(Type %in% c("Wind", "PV")) %>% 
               right_join(PlantResults %>% filter(idExec == 25)) %>% 
               filter(Simulação %in% c(1, 10, 20, 30, 40, last(Simulação)), str_detect(Plant, "Demanda", negate = TRUE)),
             mapping = aes(x = SD, y = CF, color = Type, size = GenShare)) + 
  facet_wrap(~Simulação) + scale_size_area()
# Para vários idExec
ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% 
               filter(Type %in% c("Wind", "PV")) %>% 
               right_join(PlantResults %>% filter(idExec %in% Selec, GenShare > 0.000001), 
                          by = c("name" = "Plant", "UF", "Storage capacity", "Type")) %>% 
               right_join(ExecParam) %>% drop_na(Simulação) %>% 
               filter(Simulação %in% c(1, 25, last(Simulação)), str_detect(name, "Demanda", negate = TRUE)),
             mapping = aes(x = SD, y = CustoMWh.x, color = Type, size = GenShare)) + 
  facet_grid(Codename~Simulação) + scale_size_area(max_size = 10)
# Para 2 idExec, comparando similares
left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% 
  filter(Type %in% c("Wind", "PV")) %>%
  right_join(PlotCompGeneric(PlotIds[1], PlotIds[2], UseSimilar = TRUE, correction = 1, 
                             MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD", n = 8)) %>% 
  ggplot() +
  geom_point(mapping = aes(x = SD, y = CF.pos, color = Type, size = GenShare)) + 
  facet_grid(idExec~SimiPan) + scale_size_area(max_size = 15)
# Usando o PlotData da figura CompareRunsSidebySide
ggplot(PlotData %>% left_join(SDplant %>% enframe(value = "SD"), by = c("Plant" = "name"))) +
  geom_point(mapping = aes(x = SD, y = CF.pos, color = Type, size = GenShare)) + 
  facet_grid(idExec~SimiPan) + scale_size_area(max_size = 15)

# Comparando como mapa
ggplot(left_join(ExecParam, MainResults) %>% left_join(PlantResults) %>% 
         left_join(PlantData, by = c("Plant" = "name", "Type")) %>% 
         filter(idExec %in% Selec, Simulação %in% c(1, 25, 51), 
       str_detect(Plant, "Demanda", negate = TRUE))) + 
  geom_point(aes(x = long, y = lat, size = GenShare, color = Type)) + 
  scale_size_area(max_size = 15) + facet_grid(Simulação~idExec) + theme_light()


# Comparando potência instalada com diferentes ES.
ph %>% pivot_wider(id_cols = c(Simulação, Plant), names_from = ES, values_from = Capacity) %>% 
  mutate(Step1 = `-75000` - `-90000`) %>% ggplot(aes(x = Step1)) + stat_ecdf()

ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE)) %>% pivot_wider(id_cols = c(Simulação, Plant), names_from = ES, values_from = Capacity) %>% 
  mutate(Step1 = `0` - `-10000`) %>% ggplot(aes(x = `0`, y = Step1)) + geom_point(aes(color = Simulação))

PlotClevPlant("1", "6", yValue = "CapShare", UseSimilar = TRUE, correction = 1, 
              MR = mutate(MainResults, Rel_SD_noload = sqrt(W_Variance_no_Load) / SumRC2L), 
              By = "Rel_SD_noload")
# Este gráfico mostra que a potência não aumenta sempre
ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE)) %>% 
  pivot_wider(id_cols = c(Simulação, Plant), names_from = ES, values_from = Capacity) %>% 
  mutate(Step1 = `0` - `-10000`) %>% 
  filter(abs(Step1) > 10) %>% ggplot(aes(x = `0`, y = Step1)) + 
  geom_point(aes(color = Plant, alpha = Simulação))
ph %>% filter(str_detect(Plant, "Demanda", negate = TRUE)) %>% 
  pivot_wider(id_cols = c(Simulação, Plant), names_from = ES, values_from = Capacity) %>% 
  mutate(Step1 = `0` - `-10000`) %>% 
  filter(abs(Step1) > 10) %>% ggplot(aes(y = `0`, x = `-10000`)) + 
  geom_point(aes(color = Plant, alpha = Simulação)) + geom_abline(slope = 1, intercept = 0)

# Compara caso com toda a carga atendida com o caso em que ela é atendida parcialmente (ES = -40000)

PlotScatPlant("1", "4")
PlotClevPlant("1", "4", "Wc")

GetSimilarCases("54", "56", correction = 1, MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD")
PlotClevPlant("54", "56", yValue = "CapShare", UseSimilar = TRUE, correction = 1, MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD")

PlotCompHist("1", "4")
PlotClevCF("1", "4", "Wc")
 
PlotClevCF("54", "56", yValue = "CapShare", UseSimilar = TRUE, correction = 1, MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD")
# Atualizar a função PlotCompHist para fazer um filtro por simulação após a análise de similaridade.
PlotCompHist("54", "56", yValue = "CapShare", UseSimilar = TRUE, correction = 1, 
             MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L) %>% 
               filter(Simulação %in% c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)), By = "Rel_SD")

PlotCompGeneric(PlotIds, UseSimilar = TRUE, correction = 1, 
                MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD", n = 5) %>% 
  ggplot() + geom_histogram(aes(weight = CapShare, fill = Type, x = CF.pos), bins = 10) + 
  facet_grid(idExec~SimiPan)

# Compara quando começa a entrar FV em A1 e B1.
PlotCompHist("54", "56", yValue = "CapShare", UseSimilar = TRUE, n = 30, correction = 1, 
             MR = mutate(MainResults, RelSD = W_SD / SumRC2L) %>% 
               filter(Simulação > 15, Simulação < 40), By = "RelSD")

# Compare using most used plot: CF x Variance in relation to capacity
ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% mutate(CF = SumWr/SumRC2L, VarianceCF = W_Variance/SumRC2L), 
       aes(x = VarianceCF, y = CF, group = Notes, color = Notes)) + 
  geom_line() + geom_point() + scale_color_brewer(palette = "Dark2")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% filter(Plant != "UTE_Inflex.") %>% mutate(CF = SumWr/SumRC2L, VarianceCF = W_Variance/SumRC2L), aes(x = VarianceCF, y = CF, group = Notes, color = str_sub(Notes, -21, -1))) + 
  geom_line() + geom_point() + scale_color_brewer(palette = "Dark2")


VariancePlant <- cov(SeriesWindSolar) %>% diag()
SDplant <- sqrt(VariancePlant)

# Gráfico custo x variância de cada usina usina -------------------------------------
# ggplot(left_join(PlantData, VariancePlant %>% enframe(value = "Variance"))) + geom_point(aes(x = Variance, y = CustoMWh, colour = Type)) + scale_y_log10()
# ggplot() + geom_point(left_join(PlantData, VariancePlant %>% enframe(value = "Variance")), mapping = aes(x = Variance, y = CustoMWh, colour = Type)) +
#   geom_line(filter(MainResults, idExec == idExe), mapping = aes(x = W_Variance, y = W_Cost)) + 
#   geom_point(filter(MainResults, idExec == idExe), mapping = aes(x = W_Variance, y = W_Cost)) + scale_y_log10()
ggplot() + geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% filter(Type %in% c("Wind", "PV")), 
                      mapping = aes(x = SD * 100, y = CustoMWh, colour = Type)) +
  geom_line(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), 
            mapping = aes(x = W_SD / SumRC2L * 100, y = W_Cost / SumWc, group = factor(idExec)))
ggplot() + geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% filter(Type %in% c("Wind", "PV")), 
                      mapping = aes(x = SD * 100, y = CF * 100, colour = Type)) +
  geom_line(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), 
            mapping = aes(x = W_SD / SumRC2L * 100, y = SumWc / SumRC2L * 100, color = OptType), size = 0.7, linetype = "4131") + 
  labs(x = "Normalised standard deviation (%)", y = "Capacity factor (%)")

# Calculate HHI (Herfindahl-Hirschman Index) ------------------------------------
# [@shahriariCapacityValueOptimal2018]
HHI <- group_by(PlantResults, idExec, Simulação) %>% 
  filter(str_detect(Plant, "Demanda", negate = TRUE)) %>% 
  summarise(HHIcap = sum(CapShare^2), HHIgen = sum(GenShare^2))
MainResults <- left_join(MainResults %>% select(!contains("HHI")), HHI, by = c("idExec", "Simulação"))
ggplot(left_join(left_join(ExecParam, MainResults), HHI), 
       aes(x = Simulação, y = HHIgen, group = factor(idExec), color = OptimParam)) + 
  geom_point() + geom_line()

# Calculate Shannon entropy ------------------------------------
Shannon <- group_by(PlantResults, idExec, Simulação) %>% 
  filter(str_detect(Plant, "Demanda", negate = TRUE)) %>% 
  summarise(Shannoncap = -sum(CapShare * log(CapShare, base = exp(1))), 
            Shannongen = -sum(GenShare * log(GenShare, base = exp(1))))
MainResults <- left_join(MainResults %>% select(!contains("Shannon")), Shannon, by = c("idExec", "Simulação"))
ggplot(left_join(left_join(ExecParam, MainResults), Shannon), 
       aes(x = Simulação, y = Shannongen, group = factor(idExec), color = OptimParam)) + 
  geom_point() + geom_line()
  
# Calculate geographic diversity ------------------------------------
library(geosphere)

pr <- group_by(left_join(PlantResults, PlantData, by = c("Plant" = "name")), idExec, Simulação) 
MainResults <- left_join(MainResults %>% select(-any_of("GeoDivIndex")), # Remove "GeoDivIndex" column if it already exists.
                         bind_cols(summarise(pr), GeoDivIndex = unlist(group_map(pr, ~CalcGeoDivIndex(.))) / 1000))

# Calculate non-geographic diversity ------------------------------------

# distancesSC <- readRDS("data/distancesSC-20200726.rds")
# distancesSC <- distancesSC / sqrt(nrow(SeriesComplete)) # Não lembro o motivo desta linha.
distanceWS <- dist(x = t(SeriesWindSolar))
MainResults <- left_join(MainResults %>% select(-any_of("DivIndex")), # Remove "DivIndex" column if it already exists.
                         bind_cols(summarise(pr), DivIndex = unlist(group_map(pr, ~CalcDivIndex(., DistMat = distanceWS)))))

ggplot(MainResults %>% filter(idExec %in% Selec)) + geom_line(aes(x = Simulação, y = HHIgen, color = factor(idExec)))

# Gráfico de diversidade
ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% 
         pivot_longer(c(HHIgen, HHIcap, GeoDivIndex, DivIndex), names_to = "diversiType", values_to = "diversiValue")) + 
  geom_line(aes(x = W_SD / SumRC2L * 100, y = diversiValue, color = OptimParam, linetype = AvailPlants)) + 
  facet_wrap(~diversiType, scales = "free_y")

# Balance -----------------------------------------------------------------

SeriesComplete <- mutate(SeriesComplete, DemandaOrig = Demanda, Demanda = -1)
SomeResults <- filter(MainResults, idExec %in% c(23, 26, 34))
balance <- map_dfr(1:nrow(SomeResults), ~CalcResul1Balance(SomeResults$idExec[.], 
                                                           SomeResults$Simulação[.]))

balance <- mutate(balance, Balance2 = ifelse(idExec == 26, Balance + 40000, Balance))

ggplot(balance %>% filter(timedate > as.Date("2016-01-01")), 
       aes(x = timedate, y = Balance)) + geom_line() + facet_grid(idExec ~ Simulação)
ggplot(balance, aes(x = factor(Simulação), y = Balance)) + 
  geom_boxplot(aes(fill = factor(idExec)))
ggplot(balance %>% filter(timedate > as.Date("2016-01-01")), 
       aes(x = factor(Simulação), y = Balance)) + geom_boxplot(aes(fill = factor(idExec)))

ggplot(left_join(ExecParam, MainResults) %>% 
         filter(idExec %in% c(26, 34)) %>% 
         mutate(W_Cost2 = ifelse(idExec == 34, 
                                 W_Cost - PlantData[PlantData$name == "UTE_Inflex.", "CustoMWh"][[1]] * 0.6, W_Cost)), 
       aes(x = W_Variance, y = W_Cost2, group = Notes, color = Notes)) + 
  geom_line() + geom_point() + scale_color_brewer(palette = "Dark2")

MainResults[MainResults$idExec %in% c(27, 36), c("idExec", "Simulação", "SumRC2L", "W_Variance", "W_Cost", 
                                                 "Cap_Variance", "Cap_Cost")]

## Cálculo "manual" da variância
# Cria novas colunas de demanda
SeriesComplete <- SeriesComplete %>% mutate(DemandaOrig = Demanda, DemandaFlat = -1)
cov(SeriesComplete[-1]) -> CovarMat1

SeriesWindSolar <- SeriesWindSolar %>% mutate(DemandaOrig = Demanda, DemandaFlat = -1)
cov(SeriesWindSolar) -> CovarMat1
Calc1Variance <- function(i, s, WithLoad = FALSE) {
  SR <- PlantResults[PlantResults$idExec == i & PlantResults$Simulação == s, c("Plant", "RelCaptoLoad", "Capacity")]
  if (!WithLoad) SR <- filter(SR, str_detect(Plant, "Demanda", negate = TRUE))
  CovarMat2 <- CovarMat1[deframe(SR["Plant"]), deframe(SR["Plant"])] # Select only columns and rows used.
  deframe(deframe(SR["RelCaptoLoad"]) %*% CovarMat2 %*% deframe(SR["RelCaptoLoad"]))
}

MainResults <- MainResults %>% group_by(idExec, Simulação) %>% mutate(W_Variance_no_Load = Calc1Variance(idExec, Simulação))

# No caso de demanda flat, compara valor calculado a partir do CVaR da geração com o valor "atingir meta".
# Calculado
(100000 / (100000 + map_dfr(1:51, ~CalcResul1Balance(31, .x, Series)) %>% 
             group_by(Sim) %>% arrange(Balance) %>% slice_head(prop = 0.05) %>% 
             summarise(mean(Balance))) %>% select(`mean(Balance)`) - 
    # Atingir meta
    MultiplierResults %>% filter(MultLoadType == "Flat", near(`Risk (%)`, 5), 
                                 idExec == 31, Sim %in% 1:51) %>% 
    select(Multiplier))

# gráficos distância e covariância ----------------------------------------
Correl <- correlate(SeriesWindSolar, use = "everything")
# Correlation histogram for load:
ggplot(Correl %>% focus(Demand) %>% 
         left_join(select(PlantData, name, Type), by = c("rowname" = "name"))) + 
  geom_histogram(aes(x = Demand, fill = Type, y = ..count../sum(..count..)))

plotcorrel <- stretch(Correl) %>% 
  left_join(select(PlantData, name, Type.x = Type), by = c("x" = "name")) %>% 
  left_join(select(PlantData, name, Type.y = Type), by = c("y" = "name")) %>% drop_na()

# Opção histograma
ggplot(plotcorrel) + geom_histogram(aes(x = r, fill = Type.y, y = after_stat(count/sum(count))), bins = 50) + 
  facet_wrap(~Type.x, scales = "free_y")
# Opção density
ggplot(plotcorrel) + geom_density(aes(x = r, fill = Type.y, y = after_stat(ndensity)), alpha = 0.5) + 
  facet_wrap(~Type.x, scales = "free_y")

# Distance scatterplot
distance <- as_cordf(distm(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, 
                                     c("long", "lat")]))
colnames(distance) <- c(colnames(distance[1]), 
                                 deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")]))
distance$rowname <- deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")])
distance <- stretch(distance) %>% mutate(dist = r / 1000, .keep = "unused")
# Compare only by the same type (wind to wind and PV to PV)
left_join(plotcorrel, distance) %>% filter(Type.x == Type.y) %>% 
  ggplot() + geom_point(aes(x = dist, y = r, color = Type.x)) 
 #+ stat_poly_eq(formula = 'y ~ x', aes(label = paste(..eq.label..)), parse = TRUE, label.y = c(0.07, 0)) # Optional, show statistics. Requires ggpmisc package.

by(df, df$Type.x, function(df) lm(r ~ dist, data= df)) # Show slope


# Calculate DR (Diversification Ratio) ------------------------------------
# https://seekingalpha.com/article/1033601-modern-portfolio-theory-2_0-the-most-diversified-portfolio
# Not working
#SDplant <- sapply(2:ncol(SeriesComplete), function(x) sd(deframe(SeriesComplete[x]))) %>% `names<-`(colnames(SeriesComplete[-1]))

MainResults <- MainResults %>% left_join(left_join(PlantResults %>% filter(str_detect(Plant, "Demanda", negate = TRUE)), 
                                                   enframe(SDplant, name = "Plant", value = "SD_ind")) %>% 
                                           mutate(numeratorCap = SD_ind * RelCaptoLoad, numeratorGen = SD_ind * Wc)) %>% 
  group_by(idExec, Simulação) %>%
  summarise(DRcap = sum(numeratorCap) / mean(sqrt(W_Variance_no_Load)), 
            DRgen = sum(numeratorGen) / mean(sqrt(W_Variance_no_Load)))

SeriesWindSolar <- SeriesWindSolar %>% mutate(DemandaFlat = -1, DemandaOrig = Demanda)
plan(multisession, workers = 4)
MainResults <- future_Add_column_multiplier(MainResults)

betas <- c(seq(0.99, 0.9, -0.01), 0.75, 0.5, 0.1, 0)
temp <- MainResults[MainResults$idExec %in% c(59, 60), 1:30]
for (i in 1:length(betas)) {
  temp <- future_bind_column_Var_CVaR(temp, betas[i], Serie = SeriesWindSolar)
  temp <- future_bind_column_multiplier(temp, betas[i])
}

plan(sequential)

left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% group_by(idExec) %>% select(idExec, Simulação, SumRC2L, ES, Notes, OptimParam, AvailPlants) %>% summarise(first(SumRC2L), nth(SumRC2L, 25), last(SumRC2L)) %>% mutate(`first(SumRC2L)` / first(`first(SumRC2L)`))

left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% 
  select(idExec, Simulação, SumRC2L, ES, Notes, OptimParam, AvailPlants) %>% 
  left_join(MainResults %>% filter(idExec == 1) %>% ungroup() %>% 
              select(Simulação, SumRC2LfixoReal = SumRC2L)) %>% 
  left_join(MainResults %>% filter(idExec == 30) %>% ungroup() %>% 
              select(Simulação, SumRC2LfixoFlat = SumRC2L)) %>% 
  mutate(propReal = SumRC2L / SumRC2LfixoReal, propFlat = SumRC2L / SumRC2LfixoFlat) %>% 
  ggplot() + geom_line(aes(x = Simulação, y = propReal, color = AvailPlants, group = idExec)) + 
  geom_line(aes(x = Simulação, y = propFlat, color = AvailPlants, group = idExec), linetype = 2)

# Geração por tipo de fonte -----------------------------------------------

GenType <- CalcGenTSeries(1, 2, Serie = bind_cols(OnlyTimedate, SeriesWindSolar)) %>% 
  pivot_longer(!c(idExec, Simulação, timedate), names_to = "Plant", values_to = "Generation") %>% 
  left_join(PlantData %>% select(name, Type), by = c("Plant" = "name")) %>% 
  group_by(idExec, Simulação, timedate, Type) %>% summarise(Gera = sum(Generation)) %>% 
  mutate(Type = replace_na(Type, "Load"))
GenType <- CalcGenTSeries(1, 51, Serie = bind_cols(OnlyTimedate, SeriesWindSolar)) %>% 
  pivot_longer(!c(idExec, Simulação, timedate), names_to = "Plant", values_to = "Generation") %>% 
  left_join(PlantData %>% select(name, Type), by = c("Plant" = "name")) %>% 
  group_by(idExec, Simulação, timedate, Type) %>% summarise(Gera = sum(Generation)) %>% 
  mutate(Type = replace_na(Type, "Load")) %>% bind_rows(GenType)

ggplot() + 
  geom_area(GenType %>% ungroup() %>% 
              filter(idExec == 1, Simulação == 51,
                     timedate > "2016-01-31", timedate < "2016-02-10", Type != "Load"), 
            mapping = aes(x = timedate, y = Gera, fill = Type)) + 
  geom_line(GenType %>% ungroup() %>% 
              filter(idExec == 1, Simulação == 51,
                     timedate > "2016-01-31", timedate < "2016-02-10", Type == "Load"), 
            mapping = aes(x = timedate, y = -Gera))

GenType %>% summarise(Balance = sum(Gera))
GenType %>% filter(idExec == 1, Simulação == 1) %>% summarise(Balance = sum(Gera))


# Cálculo de crédito de capacidade de acordo com a metodologia de  --------
# Zappa e van den Broek
MainResults[c("idExec", "Simulação", "SameRiskMulti_5%")] -> temp

plan(multisession, workers = 11)

CCtable <- future_map_dfr(1:nrow(temp), 
                   ~CalcResul1Balance(temp[[., 1]], temp[[., 2]], 
                                      bind_cols(OnlyTimedate, SeriesWindSolar), temp[[., 3]])) %>% 
  left_join(filter(PlantResults, str_detect(Plant, "Demanda")) %>% 
              select(idExec, Simulação, Plant, Capacity)) %>% 
  group_by(idExec, Simulação) %>% summarise(MaxResidual = -min(Balance), MaxLoad = max(Capacity)) %>% 
  mutate(Num = MaxLoad - MaxResidual) %>% 
  left_join(MainResults %>% select(idExec, Simulação, SumCapacity, `SameRiskMulti_5%`) %>% 
              mutate(NewCapacity = SumCapacity * `SameRiskMulti_5%`)) %>% 
  mutate(CC = Num / NewCapacity)

ggplot(left_join(ExecParam, left_join(MainResults, CCtable)) %>% filter(idExec %in% Selec),
       aes(x = W_SD * !!riskname * 100, y = CC,
           color = OptType, group = idExec,
           linetype = factor(Beta, levels = sort(unique(Beta), decreasing = TRUE)))) +
  geom_path() + theme_light() + scale_y_continuous(labels = scales::percent) +
  scale_x_log10() +
  theme(legend.position = "bottom") + facet_grid(~AvailPlants)


# Mostra a distribuição do balanço por simulação --------------------------

SeriesWindSolar <- SeriesWindSolar %>% mutate(DemandaOrig = Demanda, DemandaFlat = -1)

x <- left_join(ExecParam, MainResults) %>% 
  filter(idExec %in% Selec, Simulação %in% c(1, 25, 51)) %>% 
  select(idExec, Simulação, `SameRiskMulti_5%`)
balances <- map_dfr(1:nrow(x), 
                    ~CalcResul1Balance(x[[., "idExec"]], 
                                       x[[., "Simulação"]], 
                                       bind_cols(OnlyTimedate, SeriesWindSolar), 
                                       x[[., "SameRiskMulti_5%"]])) %>% 
  rename(BalanceSame = Balance)

balances <- left_join(balances, map_dfr(1:nrow(x), ~CalcResul1Balance(x[[., "idExec"]], x[[., "Simulação"]], 
                                                                      bind_cols(OnlyTimedate, SeriesWindSolar))) %>% 
                        rename(BalanceOrig = Balance))

MeanBalance <- left_join(balances, ExecParam) %>% group_by(OptType, AvailPlants, Simulação) %>% summarise(MeanBal = mean(BalanceSame))

ggplot(left_join(balances, ExecParam), aes(x = BalanceSame / 100000, color = factor(Simulação))) + 
  geom_density() + 
  geom_vline(aes(xintercept = MeanBal / 100000, color = factor(Simulação)), linetype = 2, MeanBalance) + 
  facet_grid(OptType~AvailPlants, labeller = labeller(OptType = ModelName, AvailPlants = LoadMName))

ggplot(left_join(balances, ExecParam) %>% group_by(OptType, AvailPlants, Simulação) %>% 
         mutate(MB = mean(BalanceOrig)), 
       aes(x = (BalanceOrig - MB) / 100000, color = factor(Simulação))) + 
  geom_density() + 
  facet_grid(OptType~AvailPlants)

balances %>% group_by(idExec, Simulação) %>% 
  summarise(percentile5 = quantile(BalanceSame, 0.05), 
            ShortageValue = mean(BalanceSame[BalanceSame < 0]), 
            ShortageProb = length(BalanceSame[BalanceSame < 0]) / n(), 
            PosiLowValue = mean(BalanceSame[BalanceSame >= 0 & BalanceSame < percentile5]), 
            PosiLowProb = length(BalanceSame[BalanceSame >= 0 & BalanceSame < percentile5]) / n(), 
            Low5Mean = mean(BalanceSame[BalanceSame < percentile5]), 
            Short = ShortageValue * ShortageProb, PosiLow = PosiLowValue * PosiLowProb)

balances %>% group_by(idExec, Simulação) %>% 
  summarise(percentile5 = quantile(BalanceSame, 0.05), 
            ShortageValue = mean(pmin(BalanceSame, 0)), 
            ShortageProb = length(BalanceSame[BalanceSame < 0]) / n(), 
            PosiLowValue = mean(pmin(pmax(BalanceSame, 0), percentile5) - pmin(BalanceSame - percentile5, 0) - percentile5), 
            PosiLowProb = length(BalanceSame[BalanceSame >= 0 & BalanceSame < percentile5]) / n(), 
            Low5Mean = mean(BalanceSame[BalanceSame < percentile5]), 
            Short = ShortageValue / ShortageProb, PosiLow = PosiLowValue / PosiLowProb)

## Se for por similaridade:
# Primeiro, ordena Selec
PlotCompGeneric(Selec, UseSimilar = TRUE, correction = 1, 
                MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD", n = 15) -> SimiData

x <- left_join(ExecParam, MainResults) %>% 
  filter(idExec %in% Selec, Simulação %in% unique(SimiData$Simulação)) %>% 
  select(idExec, Simulação, `SameRiskMulti_5%`)
balances <- map_dfr(1:nrow(x), 
                    ~CalcResul1Balance(x[[., "idExec"]], 
                                       x[[., "Simulação"]], 
                                       bind_cols(OnlyTimedate, SeriesWindSolar), 
                                       x[[., "SameRiskMulti_5%"]])) %>% 
  rename(BalanceSame = Balance)
# Roda o resto e termina com:
ggplot(right_join(balances, left_join(ExecParam, 
                                      SimiData %>% select(idExec, Simulação, 
                                                          idExecTo, SimiPan) %>% 
                                        distinct()) %>% 
                    select(idExec, Simulação, idExecTo, SimiPan, Codename, OptType, AvailPlants)) %>% 
         mutate(idExecTo = replace_na(idExecTo, Selec[1])) %>% drop_na(), 
       aes(x = BalanceSame / 100000, color = factor(SimiPan))) + 
  geom_density() + 
  facet_grid(OptType~AvailPlants, labeller = labeller(OptType = ModelName, AvailPlants = LoadMName))

    

# Gráfico alternativo para comparação de custo por mesmo risco.
MultiplierResults <- readRDS("results/MultiplierResults.rds")
ggplot(left_join(ExecParam, MainResults) %>% select(-any_of(colnames(MultiplierResults %>% ungroup() %>% select(-c(idExec, Simulação))))) %>% left_join(MultiplierResults) %>% filter(idExec %in% Selec), 
       aes(x = sqrt(W_Variance_no_Load) * !!riskname * 100, y = W_Cost * !!riskname, 
           linetype = MultLoadType, 
           color = AvailPlants)) + 
  geom_path() + theme_light() + 
  theme(legend.position = "bottom") + scale_y_log10(limits = c(NA, 800)) + 
  scale_x_log10(limits = c(NA, 300)) + facet_grid(~OptType) 
# Calcula a diferença percentual dos menores custos
left_join(ExecParam, MainResults) %>% 
  select(-any_of(colnames(MultiplierResults %>% ungroup() %>% select(-c(idExec, Simulação))))) %>% 
  left_join(MultiplierResults) %>% filter(idExec %in% Selec) %>% 
  mutate(PortCost = W_Cost * !!riskname) %>% group_by(OptType, AvailPlants, MultLoadType, idExec) %>% 
  summarise(minim = min(PortCost), .groups = "drop") %>% filter(MultLoadType == "Real") %>% 
  group_by(OptType) %>% summarise((nth(minim, 1) / nth(minim, 2) - 1) * 100)

# Lê dados do SIGA da ANEEL -----------------------------------------------
ANEEL_SIGA_file <- "~/Documentos/geral/Relatórios públicos/ANEEL/BD SIGA 01042021.xlsx"

read_xlsx(ANEEL_SIGA_file, skip = 1) %>% 
  filter(Fonte == "EOL") %>% group_by(UF) %>% summarise(sum(`Potência Fiscalizada (kW)`))

read_xlsx(ANEEL_SIGA_file, skip = 1) %>% 
  filter(Fonte == "EOL") %>% mutate(Região = case_when(UF %in% c("BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "NE",
                                                       TRUE ~ "Outros")) %>% 
  group_by(Fase, Região) %>% summarise(total = sum(`Potência Outorgada (kW)`)) %>% mutate(fator = total / sum(total))

# Verifica participação de eólica em cada portfólio -----------------------

PlantResults %>% mutate(Região = case_when(UF %in% c("BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL") ~ "NE",
                                           TRUE ~ "Outros")) %>% 
  group_by(idExec, Simulação, Região) %>% summarise(EOL_Share = sum(CapShare)) %>% right_join(
    left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec)) %>% filter(Região == "NE") %>% 
  ggplot(aes(x = Simulação, y = EOL_Share, color = Codename)) + geom_line()

PlantResults %>% mutate(Região = case_when(UF %in% c("BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL") ~ "NE",
                                           TRUE ~ "Outros")) %>% 
  group_by(idExec, Simulação, Região) %>% summarise(EOL_Share = sum(CapShare)) %>% right_join(
    left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec)) %>% filter(Região == "NE") %>% 
  group_by(Codename) %>% summarise(min(EOL_Share), mean(EOL_Share), max(EOL_Share))


# Correlação x distância por latitude e longitude -------------------------

distanceTotal <- distm(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used,
                                 c("long", "lat")])
distanceLat <- distm(bind_cols(long = 0, 
                               PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used,
                                         c("lat")]))
distanceLong <- sqrt(distanceTotal^2 - distanceLat^2) 

distanceTotal <- as_cordf(distanceTotal)
distanceLat <- as_cordf(distanceLat)
distanceLong <- as_cordf(distanceLong)

colnames(distanceTotal) <- c(colnames(distanceTotal[1]),
                             deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")]))
distanceTotal$term <- deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")])

colnames(distanceLat) <- c(colnames(distanceLat[1]),
                           deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")]))
distanceLat$term <- deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")])

colnames(distanceLong) <- c(colnames(distanceLong[1]),
                            deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")]))
distanceLong$term <- deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")])

distance <- stretch(distanceTotal) %>% mutate(distTot = r / 1000, .keep = "unused") %>% 
  left_join(stretch(distanceLat) %>% mutate(distLat = r / 1000, .keep = "unused")) %>% 
  left_join(distance <- stretch(distanceLong) %>% mutate(distLong = r / 1000, .keep = "unused"))

left_join(plotcorrel, distance) %>% distinct(r, distTot, .keep_all = TRUE) %>% filter(Type.x == Type.y) %>% 
  pivot_longer(c(distTot, distLat, distLong), names_to = "DistAxis", values_to = "Distance") %>% 
  ggplot(aes(x = Distance, y = r, fill = Type.x)) + geom_point(shape = 21, stroke = 0.1) + 
  geom_smooth(aes(color = Type.x)) + labs(x = "Distance (km)", y = "Pearson correlation", 
                                          fill = "Technology", color = "Technology") + 
  xlim(0, NA) + facet_grid(~DistAxis) + theme_light()
