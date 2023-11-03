if (!exists("ExecParam")) {## Initialize TotalResults tibble.
  ExecParam <- tibble(idExec = numeric(), #
                      Notes = character(), # Generic notes
                      ControllableVersion = numeric(), # Which methodology was used for controllable technologies
                      Weight = logical(), # True if the model is by relative weight
                      Samples = numeric(), # How many samples were used to CVaR
                      NTimeslices = numeric(), # How many timeslices
                      NSimulations = numeric(), # How many points in the curve
                      Beta = numeric(), omega = numeric(), N_of_SD = numeric(), # Accepted risk, Expected Shortfall and how many standard deviations from mean
                      isSamp = logical(), isNorm = logical(), NormbyVaR = logical(), # Which constraints are active? Copula, normal dist and, if normal, by VaR or CVaR?
                      ExecTime = numeric()) 
  MainResults <- tibble()
  PlantResults <- tibble()
}
# Run optimization -----------------------------------------------------------------

x <- RunModel(Series, ModelFile, DataFile, NRuns, NTimeslices)

Results <- ReadOutput(x[[1]], Plants) # Valores numa só coluna.
ResultsRow <- pivot_wider(Results, names_from = Variable, values_from = Value) # Each row has a sim result.

idExe <- pmax(max(ExecParam$idExec) + 1, 1)
ExecParam <- add_row(ExecParam, idExec = idExe, NSimulations = NRuns, Beta = beta, 
                     omega = omega, Weight = ByWeight, NTimeslices = NTimeslices, 
                     ExecTime = as.numeric(x[[2]], units = "mins"), ControllableVersion = ContrVersion,
                     NormbyVaR = isVaR, N_of_SD = nsigma, isSamp = as.logical(RiskSampFlag), 
                     isNorm = as.logical(RiskNormFlag), Notes = Notes, Samples = nrow(Y) * RiskSampFlag)
MainResults <- bind_rows(MainResults, mutate(select(ResultsRow, -any_of(c(Plants, 'Beta', 'omega', 'N_of_SD'))), 
                                             idExec = idExe, .before = Sim)) # Remove columns that are supposed to be in ExecParam
PlantResultsTemp <- mutate(pivot_longer(select(ResultsRow, Sim, Plants), Plants, names_to = "Plant",
                                        values_to = "Value"), idExec = idExe, .before = Sim)
PlantResultsTemp <- left_join(PlantResultsTemp, select(PlantData %>% filter(Used), 
                                                       name, Type, UF, `Storage capacity`, CostMWh), 
                              by = c("Plant" = "name"))
PlantResults <- bind_rows(PlantResults, PlantResultsTemp)

