options(encoding = "UTF-8")

library(tidyverse)
library(corrr)
library(lubridate)
library(lhs)
library(furrr)
library(geosphere)

source("src/Functions.R")
source("src/parameters.R")
Sys.setenv(PATH = paste(CplexPath, Sys.getenv("PATH"), sep = ":"))
Sys.setenv(LD_LIBRARY_PATH = paste(CplexPath, Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))

Series <- readRDS(SeriesFile)
PlantData <- readRDS(PlantDataFile) |> mutate(Considera = Used)

MaxLoad <- AveLoad / -colMeans(Series["Demand"])  

# Update PlantData --------------------------------------------------------
PlantData <- CalculateCosts(PlantData)

Notes <- ""
beta <- nsigma <- NULL
if (ByWeight) {
  ModelFile <- "src/MinCost_IsoRisk_W.mod"
  omega <- omega / AveLoad
} else {
  ModelFile <- "src/MinCost_IsoRisk.mod"
}
DataFile <- "Modelo.dat"

if (LoadResults) { # Load existing results
  list2env(readRDS(FiletoLoad), envir = .GlobalEnv)
  # Rename columns if using old names. Do nothing if it is already using new names.
  source("src/RenameResultsColumns.R")
}

LastidExec <- FirstNew - 1

# Run solver --------------------------------------------------------------
# Renomeia e cria colunas de demanda
Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = -1)
set.seed(99L)
Y <- GetSamples(Series, NVaRSamples, DUP, FALSE, FALSE)
PlantData <- bind_rows(PlantData, PlantData %>% filter(name == "Demand") %>% 
                         mutate(name ="OrigDemand", Used = FALSE, Considera = FALSE),
                       PlantData %>% filter(name == "Demand") %>% 
                         mutate(name = "FlatDemand", Used = FALSE, Considera = FALSE))

TypetoUse <- c("Wind", "PV")
ContrVersion <- 0 # Tipo de controláveis. (0 = nenhum, 1 = complementar, 2 = "binário")
source("src/RunScenarios.R")
SavePartialResults <- list(ExecParam = ExecParam, MainResults = MainResults, PlantResults = PlantResults)
saveRDS(SavePartialResults, paste0(ResultFile, "-nopostcalc", today(), "-A.rds"))

# Inclui armazenamento
ContrVersion <- 2 # Tipo de controláveis. (0 = nenhum, 1 = complementar, 2 = "binário")
TypetoUse <- c("Wind", "PV", "Storage")
source("src/RunScenarios.R")
saveRDS(SavePartialResults, paste0(ResultFile, "-nopostcalc", today(), "-B.rds"))
# Inclui termelétricas
TypetoUse <- c("Wind", "PV", "Thermal", "ThermalFlat", "Storage")
source("src/RunScenarios.R")
SavePartialResults <- list(ExecParam = ExecParam, MainResults = MainResults, PlantResults = PlantResults)
saveRDS(SavePartialResults, paste0(ResultFile, "-nopostcalc", today(), "-C.rds"))

# Cálculos após a otimização ------------------------------------------

source("src/PostOptmCalc.R")
# Save 
SaveResults <- list(ExecParam = ExecParam, MainResults = MainResults, 
                    PlantResults = PlantResults, MultiplierResults = MultiplierResults,
                    DivIndex = DivIndex, PlantDataUsed = PlantData,
                    Parameters = read_lines("src/parameters.R"),
                    Descrição = Descri)
saveRDS(SaveResults, paste0("results/Resultados", "-", today(), ".rds"))
