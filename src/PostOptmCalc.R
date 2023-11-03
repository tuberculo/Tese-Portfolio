# Post solver calculation -------------------------------------------------
if (exists("MultiplierResults")) {
  MultiplierResultsOld <- filter(MultiplierResults, idExec <= LastidExec)
  MultiplierResults <- select(MainResults %>% filter(idExec > LastidExec), idExec, Sim)
} else {
  MultiplierResults <- select(MainResults, idExec, Sim)
}

Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = -1)

# Create columns Wc and Capacity based on Weight parameter
# Capacity: nominal capacity in MW. Wc and Wr: generation over load. RC2L: Capacity over load. GenShare: Generation over total generation (pu). CapShare: Capacity over total capacity (pu).
PlantResults <- left_join(PlantResults, select(PlantData, name, CF, CF.pos), by = c("Plant" = "name")) 
PlantResults <- mutate(left_join(PlantResults, select(ExecParam, idExec, Weight)), 
                       Capacity = case_when(!Weight ~ Value, Weight ~ Value * AveLoad / CF.pos), 
                       Wc = case_when(Weight ~ Value, !Weight ~ Value * CF.pos / AveLoad), .after = Value)
PlantResults <- mutate(PlantResults, Wr = Wc * CF / CF.pos, RelCaptoLoad = Wc / CF.pos, .after = Wc) 
MainResults <- left_join(select(MainResults, !any_of(c("SumWc", "SumWr", "SumRC2L", "SumCapacity"))), 
                         group_by(filter(PlantResults, Type != "Load"), idExec, Sim) %>% 
                           summarise(SumWc = sum(Wc), SumWr = sum(Wr), SumRC2L = sum(RelCaptoLoad), 
                                     SumCapacity = sum(Capacity)), by = c("idExec", "Sim"))
# Values both for weight and capacity. Keep original values 
MainResults <- mutate(left_join(MainResults, select(ExecParam, idExec, Weight)), 
                      W_Cost = case_when(!Weight ~ Cost / 8760 / AveLoad * 1e6, Weight ~ Cost), 
                      W_Expected_VaR = case_when(!Weight ~ Expected_VaR / AveLoad, Weight ~ Expected_VaR),
                      W_Variance = case_when(!Weight ~ Variance / (AveLoad^2), Weight ~ Variance),
                      W_SD = case_when(!Weight ~ Standard_deviation / AveLoad, Weight ~ Standard_deviation),
                      Cap_Cost = case_when(!Weight ~ Cost, Weight ~ Cost * 8760 * AveLoad / 1e6), 
                      Cap_Expected_VaR = case_when(!Weight ~ Expected_VaR, Weight ~ Expected_VaR * AveLoad),
                      Cap_Variance = case_when(!Weight ~ Variance, Weight ~ Variance * (AveLoad^2)),
                      Cap_SD = case_when(!Weight ~ Standard_deviation, Weight ~ Standard_deviation * AveLoad)
) %>% select(-Weight) %>% relocate(Cost, Expected_VaR, Standard_deviation, Variance, .after = last_col())

PlantResults <- left_join(MainResults, PlantResults) %>% 
  mutate(GenShare = Wc / SumWc, CapShare = RelCaptoLoad / SumRC2L, .after = RelCaptoLoad) %>% 
  select(colnames(PlantResults), CapShare, GenShare) %>% 
  relocate(CapShare, GenShare, .after = RelCaptoLoad)
PlantResults <- select(PlantResults, -Weight, -CF, -CF.pos)
MainResults <- CalcResulMCVaR(Serie = Series)
MainResults <- mutate(left_join(MainResults, select(ExecParam, idExec, Weight)), 
                      W_VaR = case_when(!Weight ~ VaR / AveLoad, Weight ~ VaR), 
                      W_CVaR = case_when(!Weight ~ CVaR / AveLoad, Weight ~ CVaR),
                      Cap_VaR = case_when(!Weight ~ VaR, Weight ~ VaR * AveLoad), 
                      Cap_CVaR = case_when(!Weight ~ CVaR, Weight ~ CVaR * AveLoad),
) %>% select(-Weight) %>% relocate(W_VaR, W_CVaR, .after = W_Expected_VaR) %>% relocate(Cap_VaR, Cap_CVaR, .after = Cap_Expected_VaR)

# Label scenarios
ExecParam <- mutate(ExecParam, OptType = factor(case_when(str_detect(ExecParam$Notes, "MinCost_IsoRisk") ~ "MinCost_IsoRisk",
                                                          str_detect(ExecParam$Notes, "MaxGen_IsoCap") ~ "MaxGen_IsoCap",
                                                          str_detect(ExecParam$Notes, "MinCost_IsoCap") ~ "MinCost_IsoCap",
                                                          str_detect(ExecParam$Notes, "MinCost_IsoGen") ~ "MinCost_IsoGen",
                                                          str_detect(ExecParam$Notes, "MaxGen_IsoCost") ~ "MaxGen_IsoCost",
                                                          str_detect(ExecParam$Notes, "MinCap_IsoGen") ~ "MinCap_IsoGen",)))

CovarMat1 <- cov(Series[-1])
# Calculate variance without demand.
MainResults <- MainResults %>% group_by(idExec, Sim) %>% mutate(W_Variance_no_Load = Calc1Variance(idExec, Sim))

SavePartialResults <- list(ExecParam = ExecParam, MainResults = MainResults, 
                           PlantResults = PlantResults, PlantDataUsed = PlantData,
                           Parameters = read_lines("src/parameters.R"))
saveRDS(SavePartialResults, "partialresults.rds")

#Calculate diversification indexes ----
pr <- group_by(left_join(PlantResults, PlantData, by = c("Plant" = "name", "Type")), idExec, Sim)
#Distances <- readRDS("data/distance do SeriesComplete-20220125.rds")
Distances <- dist(x = t(Series[-1]))

DivIndex <- bind_rows(
  ## For all plants
  ### HHI and Shannon entropy
  pr %>% 
    filter(Type != "Load") %>% 
    mutate(CapShare = CapShare / sum(CapShare), GenShare = GenShare / sum(GenShare)) %>% # Normalize
    summarise(HHI_Cap = sum(CapShare^2), HHI_Gen = sum(GenShare^2),
              Shannon_Cap = -sum(CapShare * log(CapShare)), 
              Shannon_Gen = -sum(GenShare * log(GenShare))) %>% 
    pivot_longer(-c(idExec, Sim), names_to = c("Index", "By"), names_sep = "_", values_to = "Value") %>% 
    mutate(WhichPlants = "All"),
  ### geographic and non-geographic diversification 
  group_modify(pr, 
               ~tibble(GeoDiv_Gen = CalcGeoDivIndex(., param = "GenShare") / 1000,
                       EucDiv_Gen = CalcDivIndex(., param = "GenShare", DistMat = Distances),
                       GeoDiv_Cap = CalcGeoDivIndex(., param = "CapShare") / 1000,
                       EucDiv_Cap = CalcDivIndex(., param = "CapShare", DistMat = Distances)))  %>% 
    pivot_longer(-c(idExec, Sim), names_to = c("Index", "By"), names_sep = "_", values_to = "Value") %>% 
    mutate(WhichPlants = "All", .after = By), 
  # For renewables only
  ### HHI and Shannon entropy
  pr %>% 
    filter(Type %in% c("Wind", "PV")) %>% 
    mutate(CapShare = CapShare / sum(CapShare), GenShare = GenShare / sum(GenShare)) %>% # Normalize
    summarise(HHI_Cap = sum(CapShare^2), HHI_Gen = sum(GenShare^2),
              Shannon_Cap = -sum(CapShare * log(CapShare)), 
              Shannon_Gen = -sum(GenShare * log(GenShare))) %>% 
    pivot_longer(-c(idExec, Sim), names_to = c("Index", "By"), names_sep = "_", values_to = "Value") %>% 
    mutate(WhichPlants = "Wind and PV"),
  ### Calculate geographic and non-geographic diversification 
  group_modify(pr %>% filter(Type %in% c("Wind", "PV")), 
               ~tibble(GeoDiv_Gen = CalcGeoDivIndex(., param = "GenShare") / 1000,
                       EucDiv_Gen = CalcDivIndex(., param = "GenShare", DistMat = Distances),
                       GeoDiv_Cap = CalcGeoDivIndex(., param = "CapShare") / 1000,
                       EucDiv_Cap = CalcDivIndex(., param = "CapShare", DistMat = Distances)))  %>% 
    pivot_longer(-c(idExec, Sim), names_to = c("Index", "By"), names_sep = "_", values_to = "Value") %>% 
    mutate(WhichPlants = "Wind and PV", .after = By)
) %>% arrange(idExec, Sim)

betas <- c(0.99, 0.95, 0.9)

options(future.globals.maxSize = 2800*1024^2)
#foption <- furrr_options(globals = c("ExecParam", "MainResults", "PlantResults", "foption"))
if (NumThreads == 1) {
  plan(sequential)
} else {
  plan(multisession, workers = NumThreads)
}

CalcList <- select(MainResults, idExec, Sim)

Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = Demand * MaxLoad / AveLoad)
MultiplierResultsObs <- left_join(
  future_pmap_dfr(CalcList, 
                  function(idExec, Sim) tibble(idExec, Sim, CalcResul1CVaR(i = idExec, s = Sim, b = betas)), 
                  .progress = TRUE),
  future_pmap_dfr(CalcList, 
                  function(idExec, Sim) tibble(idExec, Sim, `Risk (%)` = (1 - betas) * 100, 
                                               Multiplier = FindMultiplier(i = unique(idExec), s = unique(Sim), 
                                                                           b = betas, interval = c(0, 100), tol = 1e-12)), 
                  .progress = TRUE)
) %>% mutate(MultLoadType = "Real", .after = Sim)

Series <- Series %>% mutate(OrigDemand = -1 * AveLoad / MaxLoad, FlatDemand = -1)
MultiplierResultsFlat <- left_join(
  future_pmap_dfr(CalcList, 
                  function(idExec, Sim) tibble(idExec, Sim, CalcResul1CVaR(i = idExec, s = Sim, b = betas)), 
                  .progress = TRUE),
  future_pmap_dfr(CalcList, 
                  function(idExec, Sim) tibble(idExec, Sim, `Risk (%)` = (1 - betas) * 100, 
                                               Multiplier = FindMultiplier(i = unique(idExec), s = unique(Sim), 
                                                                           b = betas, interval = c(0, 100), tol = 1e-12)), 
                  .progress = TRUE)
) %>% mutate(MultLoadType = "Flat", .after = Sim)

plan(sequential)
Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = -1)

MultiplierResults <- bind_rows(MultiplierResultsObs, MultiplierResultsFlat)