#LastidExec <- max(ExecParam$idExec)
#MainResults <- MainResults %>% select(-any_of(c("HHIcap", "HHIgen", "ShannonCap", 
#                                              "ShannonGen", "GeoDivIndex", "DivIndex")))
# Replace "Demand" column by "OrigDemand" data in PlantData tibble 

# Run with observed load. 
rm(CovarMat)
PlantData <- mutate(PlantData, Used = if_else((Type %in% c(TypetoUse) | name == "Demand") & Considera,
                                 TRUE, FALSE))
RunNModes(paste0(paste(TypetoUse, collapse = ", "), "; real load"))
# Rename load column in results
PlantResults <- mutate(PlantResults, Plant = replace(Plant, Plant == "Demand", "OrigDemand"))

# Run with flat load.
Series <- Series %>% mutate(Demand = FlatDemand)
Y <- Y %>% mutate(Demand = FlatDemand)
rm(CovarMat)
MaxLoad <- AveLoad / -colMeans(Series["FlatDemand"])  # Valor médio da carga.
# Replace "Demand" column by "FlatDemand" data in PlantData tibble 

PlantData[PlantData$name == "FlatDemand", c("CF", "CF.pos")] <- list(-AveLoad/MaxLoad, AveLoad/MaxLoad)
PlantData[PlantData$name == "Demand", c("CF", "CF.pos")] <- list(-AveLoad/MaxLoad, AveLoad/MaxLoad)

RunNModes(paste0(paste(TypetoUse, collapse = ", "), "; flat load"))

# Roda cenário com custo reduzido de fotovoltaica
if (all(c("Wind", "PV") %in% TypetoUse) & ContrVersion == 0) {
  InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] <- 
    InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] / 2
  PlantData <- CalculateCosts(PlantData)
  RunNModes(paste0(paste(TypetoUse, collapse = ", "), "; flat load, PV half cost"),
            MinCost_IsoRisk = FALSE, MaxGen_IsoCap = FALSE, MinCost_IsoGen = TRUE, 
            MinCost_IsoCap = FALSE,MinCap_IsoGen = FALSE, MaxGen_IsoCost = FALSE)
  # Restaura os custos originais
  InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] <- 
    InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] * 2  
  PlantData <- CalculateCosts(PlantData)
}

# Rename load column in results
PlantResults <- mutate(PlantResults, Plant = replace(Plant, Plant == "Demand", "FlatDemand"))

# Restore Demand column and PlantData
MaxLoad <- AveLoad / -colMeans(Series["OrigDemand"])  # Valor máximo da carga.
PlantData[PlantData$name == "Demand", c("CF", "CF.pos")] <- list(-AveLoad/MaxLoad, AveLoad/MaxLoad)

Series <- Series %>% mutate(Demand = OrigDemand)
Y <- Y %>% mutate(Demand = OrigDemand)

ExecParam <- separate(ExecParam, Notes, into = c("AvailPlants", "OptimParam"), sep = " — ", remove = FALSE)
