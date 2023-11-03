CplexPath <- "/opt/ibm/ILOG/CPLEX_Studio1210/opl/bin/x86-64_linux/"

Descri <-  "" #Descrição para o arquivo RDS.

SeriesFile <- "data/SeriesComplete.rds"
PlantDataFile <- "data/PlantData_v2.rds"
ResultFile <- "results/Resultados.rds"

# Threads -----------------------------------------------------------------
# Number of threads to use in functions "future_bind_column_Var_CVaR()" and "future_bind_column_multiplier()").
# This option has no impact on CPLEX.
# Set this value to 1, unless there is enough available RAM. 
NumThreads <- 1

# Costs -----------------------------------------------------------------
InputCosts <- tribble(~Type, ~Capex, ~OeM, ~VCost, ~rate, ~lifetime,
                         #       $/kW, $/(kW-y), $/MWh, per year, years
                         "Wind", 4800, 90, 0, 0.08, 20,
                         "PV", 3500, 50, 0, 0.08, 20,
                         "Thermal", 2700, 240, 439, 0.08, 20,
                         "ThermalFlat", 3800, 140, 272, 0.08, 20,
                         "Storage", 1500, 60, 0, 0.08, 20 # Storage power cost ($/kW)
)
#Total Storage Cost ($/kW) = Storage Energy Cost ($/kWh) * Storage Duration (hr) + Storage Power Cost ($/kW)
StoECost <- 1500 # Storage energy cost ($/kWh)

# Execution options -------------------------------------------------------
ByWeight <- FALSE
NRuns <- 51
NTimeslices <- 1
# Load existing results
LoadResults <- FALSE
FiletoLoad <- "results/Resultados.rds"
DescParam <- "" # Descrição dos parâmetros para o campo Notes.
# Risk parameters
isVaR <- FALSE # Usa VaR ou CVaR no risco calculado pela normal
NVaRSamples <- 3000
DUP <- 5
RiskNormFlag <- 0
RiskSampFlag <- 1
ListBeta <- c(0.95) # Número muito próximo de 1, em vez de um para não dar problema de divisão por 0 no nsigma e na restrição do CVaR.
Listomega <- c(0)
FirstNew <- 69 # Choose first new case to run post calculations. This value is overriden if RunScenario.R is executed.
 
# Load --------------------------------------------------------------------
#MaxLoad <- 73011.3 # Valor máximo da carga.
AveLoad <- 100000 # Valor médio da carga.

# Limits ------------------------------------------------------------------
#  Limits by group
PowerLimits <- tribble(~Type, ~MinPower, ~MaxPower,
                       "Wind", 0, 900000,
                       "PV", 0, 900000,
                       "Thermal", 0, 500000,
                       "Storage", 0, 500000
                       #                      ,"Group1", 0, 100000
                       #                      ,"Group2", 0, 100000
)
WeightLimits <- tribble(~Type, ~MinW, ~MaxW,
                        "Wind", 0, 10,
                        "PV", 0, 10,
                        "Thermal", 0, 10,
                        "Storage", 0, 10
                        #                      ,"Group1", 0, 100000
                        #                      ,"Group2", 0, 100000
)

