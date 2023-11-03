# Functions -----------------------------------------------------------------

# Define a função para exportar os dados para o CPLEX (arquivo .dat)
# Parâmetros: x: variável a ser exportada; Name: nome da variável a ser exportada; Arquivo: nome do arquivo criado; string: colocar verdadeiro se for nome dos ativos; continua: se apaga o arquivo existente ou continua no final.
ExportOPL <- function(x, Name, File, string = FALSE, continua = TRUE, AsArray = FALSE){
  if (is.vector(x)) {
    if (string) {
      if (is.numeric(x)) {
        texto <- paste(c(paste(Name, "= { "), paste(x, collapse = ", ")," };"), collapse = "")
      } 
      else {
        texto <- paste(c(paste(Name, "= { \""), paste(x, collapse = "\", \""),"\" };"), collapse = "")
      }
    }
    else if (NROW(x) == 1 & !AsArray) {
      texto <- paste(c(paste(Name, "= "), paste(x, collapse = ", "),";"), collapse = "")
    }
    else {
      texto <- paste(c(paste(Name, "= [ "), paste(x, collapse = ", ")," ];"), collapse = "")
    }
  } else {
    if (!is.array(x)) {
      x <- as.matrix(x)
    }
    D <- dim(x)
    L <- length(D) #Número de dimensões da matriz
    valor0 <- paste(c(paste(Name, "= [ ")), collapse = "")
    M <- expand.grid(lapply(D,seq)[L:1]) # Todas as combinações.
    M <- M[L:1] # Reinverte a ordem. Desta forma, fica 1 dimensão na primeira coluna e crescendo mais lentamente.
    N <- expand.grid(lapply(D,seq)[(L - 1):1]) # Todas as combinações, na sequência inversa, sem a última dimensão..
    N <- N[(L - 1):1] # Reinverte a ordem. Desta forma, fica 1 dimensão na primeira coluna e crescendo mais lentamente.
    N <- rbind(N[1,], N, N[nrow(N),]) # Insere uma linha acima e uma abaixo da matriz M
    N[1,] <- 0 # Preenche a primeira linha com 0.
    N[nrow(N),] <- 0  # Preenche a última linha com 0.
    y <- matrix(x[as.matrix(M)], ncol = (D[L]), byrow = TRUE) # Transforma em matriz, última dimensão vira coluna
    # Count how many times the dimension change
    NOpenBrackets  <- rowSums((N - lag(N, 1)) != 0, na.rm = TRUE)
    NCloseBrackets <- rowSums((lead(N, 1) - N) != 0, na.rm = TRUE)
    texto <- sapply(2:(nrow(N) - 1), function(i) paste0(paste0(rep("[", NOpenBrackets[i]), collapse = ""), paste(y[i - 1, ], collapse = ", "), paste0(rep("]", NCloseBrackets[i]), collapse = ""), ","))
    texto <- c(valor0, texto, paste("];", collapse = "")) 
  }
  write.table(texto, File, append = continua, quote = FALSE, row.names = FALSE, col.names = FALSE)
}

# Define a função para executar o Cplex.
# N: número de iterações; fat: fator da progressão.
Optimize <- function(N = 100, ModFile = "portfolio.mod", DatFile = "portfolio.dat"){
  if (file.exists("outputOPL.txt")) file.remove("outputOPL.txt")
  SD <- 99999999999
  VConFlag <- 0 #Deactivate constraints related to variance.
  # Roda minimização da variância para obter o seu menor valor  
  CostFlag <- 0
  print(paste0("Minimize variance", " - ", date()))
  system(sprintf("oplrun -e -D SD_Max=%f -D CostFlag=%d -D VConFlag=%d -D RiskNormFlag=%d -D RiskSampFlag=%d %s %s", 
                 SD, CostFlag, VConFlag, RiskNormFlag, RiskSampFlag, ModFile, DatFile, sep = ""))  
  # Roda minimização do custo para obter o maior valor de variância
  CostFlag <- 1
  print(paste0("Minimize cost", " - ", date()))
  system(sprintf("oplrun -e -D SD_Max=%f -D CostFlag=%d -D VConFlag=%d -D RiskNormFlag=%d -D RiskSampFlag=%d %s %s",
                 SD, CostFlag, VConFlag, RiskNormFlag, RiskSampFlag, ModFile, DatFile, sep = ""))
  Limits <- ReadOutput("outputOPL.txt")
  assign("Pre_resul", Limits, envir = .GlobalEnv)
  file.remove("outputOPL.txt")
  Limits <- select(filter(Limits, Variable == "Standard_deviation"), Value) 
  SD <- as.double(Limits[1,] * 1.00001) # Add a small tolerance to the first iteration to avoid infeasibeality. 
  #M <- N - 1
  Passo <- as.double((Limits[2,] - SD)/(N - 1)) # Uniform increments in max SD constraint.
  VConFlag <- 1 # Activate constraints related to variance
  for (i in 1:N) {
    cat("Iteration:", i, "- Standard deviation:", SD, sep = " ")
    system(sprintf("oplrun -e -D SD_Max=%f -D CostFlag=%d -D VConFlag=%d -D RiskNormFlag=%d -D RiskSampFlag=%d %s %s",
                   signif(SD, 6), CostFlag, VConFlag, RiskNormFlag, RiskSampFlag, ModFile, DatFile, sep = ""))
    SD <- SD + Passo # Incrementa o desvio padrão
    Variancia <- SD^2 # Modelo OPL recebe variância
  }
  if (!dir.exists("out")) dir.create("out")
  nome <- paste0("out/OptOut - ","N=", N, " - ", DatFile, " - ", format(Sys.time(), "%Y%m%d_%H-%M-%S"), ".txt")
  file.rename("outputOPL.txt", nome)
  print(paste0("Results saved on file: ", nome))
  nome
}

Otimi_change_beta <- function(N = 100, ModFile = "portfolio.mod", DatFile = "portfolio.dat"){
  if (file.exists("outputOPL.txt")) file.remove("outputOPL.txt")
  #Variancia <- 9999999999999
  FlagCost <- 1
  FlagVari <- 0
  beta <- c(0.999, 0.9975, 0.995, 0.99)
  beta <- c(seq(0,0.975, by = 0.025), beta)
  iteration <- 0
  for (i in beta){
    iteration <- iteration + 1
    cat("Iteração:", iteration, "- beta:", i, sep = " ")
    system(sprintf("oplrun -e -D beta=%f -D VarMax=%f -D FlagCost=%f -D FlagVari=%f %s %s", i, Variancia, FlagCost, FlagVari, ModFile, DatFile, sep=""))
  }
  nome <- paste0("saida - ","N=", N, " - changeCVaRbeta", DatFile, ".txt")
  file.rename("outputOPL.txt", nome)
  cat("Resultados salvos no arquivo: ", nome)
  nome
}


# Define a função para ler as saídas do Cplex e criar um tibble.
# Arquivo: arquivo a ser lido. PlantsName: vetor com o nome das usinas.
#Código está bem ruim mas funciona. Refazer se tiver tempo.
ReadOutput <- function(OutFile, PlantsName = Plants){
  # Read CPLEX results file.
  te1 <- c(read_file(OutFile))
  # Remove brackets, double spaces and line breaks.
  te2 <- gsub("[][]", "", gsub("[ ]{2,}"," ",gsub("\n","",gsub("\r","",te1))))
  te3 <- read.delim(text = te2, header = F, sep = "*") 
  te4 <- te3 
  te4 <- as_tibble(lapply(te4, function(x) gsub(";","\n", x))) # Turn ";" into line break
  te4[,ncol(te4)] <- NULL # Remove last column (it is empty).
  Nomes_variáveis <- as_tibble(read.delim(text = as.character(te4[ncol(te4)]), 
                                          header = FALSE, sep = "\n")) %>% 
    separate(V1, c("names", "value"))
  Dimen <- dim(Nomes_variáveis)
  Nomes_variáveis <- Nomes_variáveis[,1] # Get names from first column
  te <- NULL
  for (i in 1:(ncol(te4))) {
    te5 <- read.delim(text = as.character(te4[i]), header = F, sep = "\n") %>% 
      separate(V1, c("names", "value"), sep = " ", extra = "merge")
    te5 <- add_column(te5, Sim = i, .before = 1) # Insert Sim number.
    te6a <- select(slice(te5, 1:(NROW(te5) - 1)), 1:3) 
    te6a <- spread(te6a, "names", "value")
    VariáveisSaída <- colnames(te6a[-1]) 
    te6b <- slice(te5, n()) %>% select(-"names") %>% 
      separate(value, into = PlantsName, sep = " ") # Get plants results. 
    te7 <- left_join(te6a ,te6b, "Sim")
    te8 <- as_tibble(gather(te7, VariáveisSaída, PlantsName, key = "Variable", value = "Value"))
    te <- bind_rows(te, te8)
  }
  te$Value <- parse_double(te$Value)
  te
}

PrepareData <- function(Serie = SeriecomUTE, T. = 1, LoadCol = "Demand") {
  # Create variables: Usinas, FC, Cost, MaxPower, MinPower, Groups, GroupLHS, CovarMat

  # Extract names, costs and CF.
  PData <- PlantData[PlantData$Used == TRUE, c("name", "CostMWh", "CF", "CF.pos")] 
  Plants <<- PData[[1]] # Extract names.
  Cost <<- deframe(PData[c(1,2)])
  CF <<- deframe(PData[c(1,3)])
  CF.pos <<- deframe(PData[c(1,4)])
  
  f <- 1 + ByWeight # If "ByWeight" f <- 2
  MaxPower <<- PlantData[PlantData$Used == TRUE, c("PMax", "WMax")][[f]]
  MinPower <<- PlantData[PlantData$Used == TRUE, c("PMin", "WMin")][[f]]
  
  Groups <<- colnames(select(filter(PlantData, Used == TRUE), contains("Group"))) # Get groups names
  GroupLHS <<- t(select(filter(PlantData, Used == TRUE), contains("Group"))) # Which plant is in each group.
  Carga <<- AveLoad
  Serie <- Serie[c("timedate", PlantData[PlantData$Used == TRUE, c("name")][[1]])]
  DuraPer <- 24/T.
  Serie <- group_by(mutate(Serie, HOUR = hour(timedate), timeslice = floor(HOUR / DuraPer)), timeslice)
  CFs <- summarise(Serie, across(c(-timedate, -HOUR), mean)) # Average capacity factor per timeslice.
  Load <<- -select(CFs, all_of(LoadCol)) * MaxLoad # Average load per timeslice.
  # If CovarMat already exists with the same parameters, skip.
  if (!exists("CovarMat")) CovarMat <- 0
  if (all(CovarMat[[1]] == c(ByWeight, T.))) {
    print(paste0("Skipping covariance calculation", " - ", date()))
  } else {
    print(paste0("Start covariance calculation", " - ", date()))
    CovarMat <- group_map(select(Serie, -timedate, -HOUR), ~cov(.)) # Calcula matriz de covariância 
    CovarMatN <- map(1:T., ~t(t(CovarMat[[.]])/CF.pos)/CF.pos) # Normalised
    print(paste0("End covariance calculation", " - ", date()))
    CovarMat <- aperm(array(unlist(CovarMat), dim = c(nrow(CovarMat[[1]]), ncol(CovarMat[[1]]), length(CovarMat))),
                          c(3, 1, 2)) # Convert to 3D array, timeslice first.
    CovarMatN <- aperm(array(unlist(CovarMatN), dim = c(nrow(CovarMatN[[1]]), ncol(CovarMatN[[1]]), length(CovarMatN))),
                      c(3, 1, 2)) # Convert to 3D array, timeslice first.
    CovarMat <<- list(c(Weight = ByWeight, NTimeslices = T.), CovarMat, CovarMatN) # Insert metadata
  }
  CFs <<- select(CFs, -timeslice)
}

MakeDatFile <- function(DatFile, T. = T.) {
  # Export all variables needed to run the model in a .dat file.
  print(paste0("Begin file export", " - ", date()))
  ExportOPL(Plants, "Plants", DatFile, TRUE, FALSE)
  ExportOPL(Cost, "Cost", DatFile)
  ExportOPL(1:T., "Timeslice", DatFile, TRUE)
  ExportOPL(Groups, "Groups", DatFile, TRUE) #  Groups names.
  ExportOPL(GroupLHS, "GroupLHS", DatFile)  #  Left-hand side of group restrictions.
  if (ByWeight) {
    ExportOPL(t(t(CFs) / CF.pos), "RelCF", DatFile)
    ExportOPL(deframe(select(WeightLimits, MaxW)), "MaxWGroup", DatFile)  #  Right-hand side of group max power restrictions.
    ExportOPL(deframe(select(WeightLimits, MinW)), "MinWGroup", DatFile)  #  Right-hand side of group min power restrictions.
    ExportOPL(MaxPower, "MaxW", DatFile)
    ExportOPL(MinPower, "MinW", DatFile)
    ExportOPL(CovarMat[[3]], "CovarianceN", DatFile)
  } else {
    ExportOPL(MaxLoad, "MaxLoad", DatFile)
    ExportOPL(CF, "CF", DatFile)
    ExportOPL(CF.pos, "CFpos", DatFile)
    ExportOPL(deframe(select(PowerLimits, MaxPower)), "MaxPGroup", DatFile)  #  Right-hand side of group max power restrictions.
    ExportOPL(deframe(select(PowerLimits, MinPower)), "MinPGroup", DatFile)  #  Right-hand side of group min power restrictions.
    ExportOPL(MaxPower, "MaxPot", DatFile)
    ExportOPL(MinPower, "MinPot", DatFile)
    ExportOPL(CFs, "CFs", DatFile)
    ExportOPL(CovarMat[[2]], "Covariance", DatFile)
  }
  
  # Matrix with samples. Divided by plant CF if it is byWeight.
  tempY <- matrix(0, nrow = 2, ncol = length(Plants))
  nameY <- "Y"
  if (RiskSampFlag) {
    tempY <- Y[PlantData[PlantData$Used == TRUE, c("name")][[1]]] # Select plants used in the corresponding order.
  }
  if (ByWeight) {
    tempY <- t(t(tempY)/CF.pos)
    nameY <- "Y_N"
  }
  ExportOPL(nrow(tempY), "Mtot", DatFile, FALSE)
  ExportOPL(tempY, nameY, DatFile, FALSE, TRUE)
  ExportOPL(omega, "omega", DatFile, FALSE, TRUE)
}

RunModel <- function(InputData = SeriecomUTE, ModFile = "portfolio.mod",
          DatFile = "portfolio.dat", NSimul = 10, T. = 1, byBeta = FALSE, ...) {
# Entrada: tabela com dados temporais, arquivo .mod, arquivo .dat, número simulações, número de períodos, demanda média, demanda máxima)
  PrepareData(Serie = InputData, T. = T., ...)
  gc()
  MakeDatFile(DatFile, T. = T.)
  print(paste0("Call solver", " - ", date()))
    if (byBeta) {
    x <- Otimi_change_beta(NSimul, ModFile, DatFile)
  } else {
    ExportOPL(beta, "beta", DatFile, FALSE, TRUE)
    ExportOPL(nsigma, "nsigma", DatFile, FALSE, TRUE)
    it <- Sys.time()
    x <- Optimize(NSimul, ModFile, DatFile)
    ft <- Sys.time()
    x <- list(x, ft - it)
  }
}

CalcFRC <- function(rate = 0.08, LifeTime = 20){
  rate * (1 + rate)^LifeTime / ((1 + rate)^LifeTime - 1 )
}

AddThermPP <- function(z = Series, PlantNames = PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, "name"][[1]], 
                       add_follow = TRUE, add_peak = TRUE, add_flat = TRUE, TH = 0.1) { 
  AllNames <- colnames(z)
  # Insert thermal power plants (complementary, load following and flat)
  if (add_follow) {
    z <- mutate(z, across(PlantNames, ~(max(.) - pmax(., 0)), .names = "UTEcomp_{col}")) 
    PlantData <- add_row(PlantData, name = colnames(z)[!(colnames(z) %in% AllNames)], 
                         Type = "Thermal", Used = TRUE, 
                         UF = select(filter(PlantData, name %in% PlantNames), UF)[[1]], 
                         Município = select(filter(PlantData, name %in% PlantNames), Município)[[1]])
  }
  AllNames <- colnames(z)
  if (add_peak) {
    z <- mutate(z, across(PlantNames, ~((TH - pmin(., TH)) / TH), .names = "UTEpeak_{col}")) 
    PlantData <- add_row(PlantData, name = colnames(z)[!(colnames(z) %in% AllNames)], 
                         Type = "Thermal", Used = TRUE, 
                         UF = select(filter(PlantData, name %in% PlantNames), UF)[[1]], 
                         Município = select(filter(PlantData, name %in% PlantNames), Município)[[1]])
  }
  # Load following UTE.
  if (add_follow) {
    z <- mutate(z, UTEcomp_Demand = (Demand - max(Demand))/(min(Demand) - max(Demand)))
    PlantData <- add_row(PlantData, name = "UTEcomp_Demand", Type = "Thermal", Used = TRUE)
  }
  if (add_peak) {
    z <- mutate(z, UTEpeak_Demand = ((TH - 1) - pmin(Demand, TH - 1)) / TH)
    PlantData <- add_row(PlantData, name = "UTEpeak_Demand", Type = "Thermal", Used = TRUE)
  }
#  The inverse function is z * (max(Demand) -min(Demand)) + min(Demand). 
#  So, if UTEcomp_Demand capacity is equal to (max(Demand) -min(Demand)) * MaxLoad and
#  UTEinflex. capacity is equal to min(Demand) * MaxLoad, the demand is completely satisfied.
  
  # Base power plant
  if (add_peak) {
    z <- mutate(z, "UTE_Inflex." = 1) 
    PlantData <- add_row(PlantData, name = "UTE_Inflex.", Type = "ThermalFlat", Used = TRUE)
  }
  # Move a demanda para a última coluna
  z <- relocate(z, Demand, .after = last_col())
  
  Plants <- colnames(z[-1])
  UsinasUTE <- Plants[grep("UTE", Plants)]
  #Make these variables global..
  assign("Plants", Plants, envir = .GlobalEnv)
  assign("UsinasUTE", UsinasUTE, envir = .GlobalEnv)
  assign("PlantData", PlantData, envir = .GlobalEnv)
  z
}
  
RestriTerm <- function(x = SeriecomUTE, MaxRampMinu = 0.6, TempoPartidaH = 2, PatMin = 0.25){ #Altera as séries das termelétricas considerando as restrições de rampa. Futuramente: geração mínima e tempo de partida.  
  MaxRampHora <- MaxRampMinu * 60 / 100
  IndicesUTE <- match(PlantData[grep("Therm", PlantData$Type), ]$name, colnames(x))
  res <- numeric(nrow(x))
  NumChanges <- numeric()
  MeanChange <- numeric()
  #Insere as restrições horárias
  for (j in IndicesUTE) {
    res <- deframe(x[,j])
    for (i in 2:nrow(x)) {
      res[i] <- min(max(res[i] - res[i - 1], -MaxRampHora), MaxRampHora) + res[i - 1]
      #if(i%%100000 == 0){print(i)}
    }
    NumChanges[j] <- sum(res - deframe(x[,j]) != 0)
    MeanChange[j] <- sum(res - deframe(x[,j])) / NumChanges[j]
    print(paste0(match(j, IndicesUTE), "/", NROW(IndicesUTE), " - ", colnames(x[j]), " - Changes: ", round(NumChanges[j] / nrow(x) * 100, 1), " % - Mean change: ", round(MeanChange[j], 3)))
    x[,j] <- res
  }
  #print(colMeans(cbind(NumChanges, MeanChange), na.rm = TRUE))
  #assign("MeanChanges", cbind(NumChanges, MeanChange), envir = .GlobalEnv)
  x
  #assign("res", res, envir = .GlobalEnv)
}  

CalculateCosts <- function(PD = PlantData) {
  # Remove existing data
  PD <- select(PD, -any_of(c("Capex", "OeM", "VCost", "FixedCost", "CostMWh", "rate", "lifetime")))
  PD <- left_join(PD, InputCosts, by = c("Type"))
  PD <- mutate(PD, FixedCost = (((Capex + StoECost * replace_na(`Storage capacity`, 0)) * 
                                   CalcFRC(rate, lifetime) + OeM) * 1000) / (CF.pos * 8760), 
               CostMWh = FixedCost + VCost)
  # Define cost for "load plant" as equal to 0.
  PD[PD$Type == "Load", "CostMWh"] <- 0
  PD
}

GetSamples <- function(x = SeriesComplete, n = 1000, dup. = 5, AcceptLowerN = FALSE, UsefastLHS = FALSE) { 
  # Use Latin Hypercube Sampling to get evenly distributed samples in terms of years, months and hours.
  i <- 1
  FirstYear <- year(min(x$timedate))
  nYears <- ceiling(difftime(max(x$timedate), min(x$timedate))/365)
  if (n <= 15000 & !UsefastLHS) { # improvedLHS is too slow for large n, force random if n is greater than 15000.
    LHSsample <- improvedLHS(n, 3, dup = dup.) 
  } else {
    LHSsample <- randomLHS(n, 3)
  }
  repeat {
    distInt <- mutate(as_tibble(t(t(LHSsample) * c(12, 24, nYears))) %>% `colnames<-`(c("Month", "Hour", "Year")), Month = ceiling(Month), Hour = floor(Hour), Year = floor(Year))
    # Remove lines that does not have a match.
    distInt <- filter(distInt, !(Year == (nYears - 1) & Month > month(max(x$timedate))))
    # Count number of lines to resample.
    N_Resample <- n - nrow(distInt)
    if (N_Resample == 0 | AcceptLowerN) {
      break
    }
    print(paste0("Resampling. Iteration: ", i, " - ", date()))
    i <- i + 1
    LHSsample <- augmentLHS(LHSsample, N_Resample)
  }
  distInt <- count(distInt, Month, Hour, Year)
  x <- mutate(x, Month = month(timedate), Hour = hour(timedate), Year = year(timedate))
  # Get samples from each combination of month, hour and year in distInt. 
  Sample_in_group <- function(z) {
    S <- x[x$Month == distInt[[z, 1]] & x$Hour == distInt[[z, 2]] & x$Year == (distInt[[z, 3]] + FirstYear), ]
    S <- S[sample.int(nrow(S), distInt[[z, 4]], TRUE), ]
  }
  Y <- lapply(1:nrow(distInt), Sample_in_group) %>% tibble() %>% unnest(cols = c(.))
  Y <- select(Y, -timedate, -Year, -Hour, -Month)
}

AddStor <- function(S = Series, MaxStorHours = 4, efi = 0.85, RollAverWindow = MaxStorHours * 15, TimeStepsAhead = MaxStorHours * 8, MaxPower = 100){
  #  MaxStorHours is equivalent hours of storage available. 
  # RollAverWindow defines how many hours in the future are considered to the 
  # rolling average, that value is the "goal" to the storage system.
  # TimeStepsAhead is how many hours are used to avoid filling the storage capacity.
  rm(R, pos = 1)
  for (j in 2:ncol(S)) {
    #print(paste0("Begin:",  j - 1, "/", ncol(S) - 1, " - ", Sys.time()))
    v <- deframe(S[,j])
    StoLevel <- MaxStorHours/2 #Storage level begins at half.
    B <- 0 # Storage "generation", can be a negative value
    Loss <- 0 #  Loss due to efficiency < 1.
    # Average value. The goal is that the aggregated generation is close to this value.
    Average <- rollmean(v, RollAverWindow, fill = mean(v), align = "left")
    #Average <- rep(mean(v), NROW(v))  
    Diff <- v - Average  # Difference to average.
    temp <- bind_cols(gener = v, dif = Diff, aver = Average)
    temp <- bind_cols(temp, as_tibble(sapply(1:TimeStepsAhead, function(x) rollsum(-Diff, x, align = "left", fill = "extend"))))
    temp <- mutate(temp, maxfut = apply(select(temp, starts_with("V")), 1, max))
    # Find out where is the maximum value. This is faster without tibble and with transposed matrix.
    temp2 <- temp %>% select(starts_with("V")) %>% t()
    MaxFut <- select(temp, maxfut)[[1]]
    temp <- bind_cols(temp, as_tibble(unlist(lapply(1:ncol(temp2), function(x) which(temp2[,x] == MaxFut[x])[1]))))
    #temp <- rename(temp, nMax = value)
    nMax <- temp$value
    rm(temp, temp2)
    #print(paste0("Before for:",  j - 1, "/", ncol(S) - 1, " - ", Sys.time()))
    for (i in 2:nrow(S)) {
      if (Diff[i] <= 0) { #charge
        #discharge. Compare maximum amount that should be stored in the future to the available in the storage.
        # If it is greater, divide by the number of time steps to the maximum.
        if (MaxFut[i] <= StoLevel[i - 1]) {nMax[i] <- 1}
        B[i] <- min(-Diff[i], StoLevel[i - 1] / nMax[i], MaxPower)
        StoLevel[i] <- StoLevel[i - 1] - B[i]
        Loss[i] <- 0
      } else {#charge
        if (MaxFut[i] >= (StoLevel[i - 1] - MaxStorHours)) {nMax[i] <- 1}
        B[i] <- max(-Diff[i], (StoLevel[i - 1] - MaxStorHours) / nMax[i], -MaxPower) / efi
        StoLevel[i] <- StoLevel[i - 1] - B[i] * efi
        Loss[i] <- -B[i] * (1 - efi)
      }
    }
    print(paste0("Status:",  j - 1, "/", ncol(S) - 1, " - ", Sys.time()))
    R1 <- tibble(B, StoLevel, Loss)
    R1 <- bind_cols(S[1], R1)
    R1 <- pivot_longer(R1, c(B, StoLevel, Loss), names_to = "Variable", values_to = paste0("STO_", MaxStorHours, "h_", colnames(S[j])))
    if (exists("R")) {
      R <- left_join(R, R1, by = c("timedate", "Variable"))
    } else R <- R1
  }
  R
}

AddStorOP <- function(x, MaxStorHours = 4, efi = 0.85, 
            ModFile = "storage.mod", DatFile = "storage.dat", hide = TRUE) {
  if (file.exists("StorageOutput.txt")) file.remove("StorageOutput.txt")
  if (file.exists("Storage.sav")) file.remove("Storage.sav")
  Ttot <- NROW(x)
  env <- openEnvCPLEX()
  probSto <- initProbCPLEX(env)
  chgProbNameCPLEX(env, probSto, "StorageOpt")
  for (i in 2:ncol(x)) { 
    z <- x[[i]]
    ExportOPL(Ttot, "Ttot", DatFile, , FALSE)
    CF <- mean(z)
    ExportOPL(CF, "CF", DatFile)
    ExportOPL(z, "G", DatFile)
    ExportOPL(MaxStorHours, "StoragetoCapRatio", DatFile)
    ExportOPL(efi, "efi", DatFile)
    system(sprintf("oplrun -e Storage.sav %s %s", ModFile, DatFile, sep = ""), ignore.stdout = hide)
    print(paste0("Data file for ", colnames(x[i]), " created - #", i, ".  ", date()))
    
    readCopyProbCPLEX(env, probSto, "Storage.sav")
    # Execute optimization
    status <- qpoptCPLEX(env, probSto)
    if (status == 0) {
      print(paste0("Problem solved ", date()))
    } else print(paste0("Optimization error ", date()))
    #solutionCPLEX(env, probSto)$x
    # Import results and order them.
    VariablesResults <- tibble(Variable = getColNameCPLEX(env, probSto, 0, getNumColsCPLEX(env, probSto) - 1), 
                               Value = solutionCPLEX(env, probSto)$x)
    VariablesResults <- VariablesResults %>% 
      separate(Variable, c("Variable", "Index"), sep = "#", convert = TRUE)
    # Add timedate column and rename values
    VariablesResults <- VariablesResults %>% 
      left_join(select(x, timedate) %>% mutate(Index = (1:nrow(x)) - 1)) %>% 
      relocate(timedate) %>% rename_with(~paste0("STOb_", MaxStorHours, "h_", colnames(x[i])), .cols = Value)
    if (exists("TotalVariablesResults")) {
      TotalVariablesResults <- left_join(TotalVariablesResults, VariablesResults, by = c("timedate", "Variable", "Index"))
    } else TotalVariablesResults <- VariablesResults
  }
  TotalVariablesResults
}

ReadONSGenData <- function(FileName) {
  ONSData <- read_csv(FileName)
  ONSData <- select(ONSData, `Data Dica Comp`, `Selecione Comparar GE Comp 3`, `Selecione Tipo de GE Comp 3`)
  colnames(ONSData) <- c("Date" , "Plant_Name", "Generation")
  ONSData$Date <- parse_date_time(ONSData$Date, orders = "dmYHM")
  ONSData
}

ReadONSLoadData <- function(FileName) {
  DaysPerYear <- 365
  HoursPerDay <- 24
  loadcurve <- read_csv2(FileName)
  loadcurve$`Data Escala de Tempo 1 CDH Simp 4_1` <- NULL
  loadcurve$Subsistema <- NULL
  colnames(loadcurve) <- c("Date", "Value")
  loadcurve$Date <- parse_datetime(loadcurve$Date, "%d/%m/%Y %H:%M:%S", locale = locale(tz = "America/Sao_Paulo"))
  loadcurve[which(loadcurve$Value == max(loadcurve$Value)),] #Valor máximo
  
  #mean before and after the spike
  meanBA <- (loadcurve[which(loadcurve$Value == max(loadcurve$Value)) - 1, 2] + loadcurve[which(loadcurve$Value == max(loadcurve$Value)) + 2, 2])/2
  loadcurve[c(which(loadcurve$Value == max(loadcurve$Value)), which(loadcurve$Value == max(loadcurve$Value)) + 1),2] <- meanBA
  loadcurve <- mutate(ungroup(loadcurve), max_rollYear = rollmax(loadcurve$Value, DaysPerYear * HoursPerDay, align = "center", fill = "extend"))
  loadcurve <- mutate(ungroup(loadcurve), medmax = rollmean(loadcurve$max_rollYear, DaysPerYear * HoursPerDay, align = "center", fill = "extend"))
  
  loadcurve <- mutate(loadcurve, PerUnit = Value/medmax)
}

Import_series_from_list <- function(filename) {
  load(filename, verbose = TRUE)
  SeriesWindKat <- as_tibble(map(1:nrow(windparks), ~statpowlist[[.]][,2]), .name_repair = "unique")
  colnames(SeriesWindKat) <- windparks$name #  Get windparks names.
  
  SeriesWindKat <- SeriesWindKat[, !sapply(SeriesWindKat, function(x) mean(is.na(x))) > 0.1] #  Remove columns that have more than 10 % of NA values.
  
  SeriesWindKat <- add_column(SeriesWindKat, timedate = statpowlist[[1]][,1], .before = TRUE)
  
  SeriesWindKatNorm <- as_tibble(cbind(SeriesWindKat[1], t(t(SeriesWindKat[-1])/sapply(SeriesWindKat[-1],
                      max, na.rm = TRUE))), .name_repair = "minimal") #normaliza a série, mantém a primeira coluna
  SeriesWindKatNorm
}

Remove_by_cumulative <- function(S = Series, LimMaxAbsDiff = 0.5) {
# Remove if difference between cumulative distribution of the series and the uniform distribution is greater than LimMaxAbsDiff.
  lineargrow <- 1:nrow(S)/nrow(S)
  MaxAbsDiff <- sapply(S[-1], function(x) max(abs(ecdf(x)(lineargrow) - lineargrow)))
  S <- S[c(TRUE, MaxAbsDiff < LimMaxAbsDiff)]
}

Remove_by_correlation <- function(S = Series, LimMaxCorr = 0.99) {
# Remove if correlation is greater than LimMaxCorr.
  correl <- cor(S[-1])
  i <- 0
  j <- 0
  rmlist <- NULL
  
  for (i in 1:(NCOL(correl) - 1)) {
    for (j in (i + 1):NCOL(correl)) {
      if (correl[i,j] > 0.99) { rmlist <- c(rmlist, j) }
    }
  }
  
  rmlist <- unique(rmlist)
  S <- S %>% select(-(rmlist + 1)) 
  S
}

CalcResul1CVaR <- function(i, s, b = beta, Serie = Series, multiplier = 1) {
  Selection <- PlantResults$idExec == i & PlantResults$Sim == s
  PR <- PlantResults[Selection,]
  weight <- filter(ExecParam, idExec == i)$Weight
  Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
  bal <- as.matrix(Serie[deframe(PR[PR$Type != "Load", "Plant"])]) %*% 
    (deframe(PR[PR$Type != "Load", Colu]) * multiplier) +
    as.matrix(Serie[deframe(PR[PR$Type == "Load", "Plant"])]) %*% 
    (deframe(PR[PR$Type == "Load", Colu])) 

  out <- tibble(`Risk (%)` =  (1 - b) * 100, VaR = quantile(bal, 1 - b, type = 1))
  mutate(out, CVaR = CalcCVaR(bal, b, UpperTail = FALSE))
}

CalcResulMCVaR <- function(result = MainResults, ...) { # Calculate with beta value used in optimization
  func1 <- function(x) {
    if (x %% 100 == 0) print(paste0("line: ", x, "/", nrow(result)))
    bind_cols(idExec = result$idExec[x], Sim = result$Sim[x], 
              CalcResul1CVaR(result$idExec[x], 
                             result$Sim[x], 
                             left_join(result, select(ExecParam, idExec, Beta), by = "idExec")$Beta[x],
                             ...))
  }
  res <- map_dfr(1:nrow(result), func1)
  result[c("VaR", "CVaR")] <- res[c("VaR", "CVaR")]
  result
}
# Find value to multiply installed capacity to achieve a given level of risk.
FindMultiplier <- function(i, s, bs = 0.95, Serie = Series, ...) {
  SubsettoCalc1CVaR <- function(i, s, Serie) {
    Selection <- PlantResults$idExec == i & PlantResults$Sim == s
    PR <- PlantResults[Selection,]
    weight <- filter(ExecParam, idExec == i)$Weight
    Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
    SerMatrix <- as.matrix(Serie[deframe(PR[PR$Type != "Load", "Plant"])])
    CapVector <- deframe(PR[PR$Type != "Load", Colu])
    SerMatrixLoad <- as.matrix(Serie[deframe(PR[PR$Type == "Load", "Plant"])])
    CapVectorLoad <- deframe(PR[PR$Type == "Load", Colu])
    list(SerMatrix = SerMatrix, CapVector = CapVector, 
         SerMatrixLoad = SerMatrixLoad, CapVectorLoad = CapVectorLoad)
  }
  CalcResul1CVaRMatrix <- function(MatX, MatY, multiplier = 1, b = beta) {
    bal <- multiplier * MatX + MatY
    CalcCVaR(bal, b, UpperTail = FALSE)
  }
  SData <- SubsettoCalc1CVaR(i, s, Serie)
  MatX <- with(SData,  SerMatrix %*% CapVector)
  MatY <- with(SData, SerMatrixLoad %*% (CapVectorLoad))
  sapply(bs, function(bs) 
    optimize(function(x) abs(CalcResul1CVaRMatrix(MatX, MatY, b = bs, 
                                                  multiplier = x)), 
             ...)$minimum)
}

# Bind a column to MainResults containing the multiplier
# Deprecated
future_bind_column_multiplier <- function(df, b = 0.95, cname = "SameRiskMulti", foption = furrr_options(), ...) {
  print(paste0("CalcMultiplier: ", b * 100, "%", " - ", date()))
  cname <- paste0(cname, "_", (1 - b) * 100, "%")
  df <- select(df, -any_of(!!cname))
  bind_cols(df, 
            !!cname := future_map_dbl(1:nrow(df), 
                                      function(n) {
                                        if (n %% 1000 == 1) print(paste0("line: ", n, "/", nrow(df), " - ", date()))
                                        FindMultiplier(i = df[[n, "idExec"]],
                                                       s = df[[n, "Sim"]], b = b, ...)
                                      }, .progress = TRUE, .options = foption))
}
# Bind columns to MainResults containing values of VaR and CVaR for a given beta.
#Deprecated
future_bind_column_Var_CVaR <- function(df, b = 0.95, varname = "VaR", cvarname = "CVaR", foption = furrr_options(), ...) {
  print(paste0("CalcVaRCVaR: ", b * 100, "%", " - ", date()))
  varname <- paste0(varname, "_", (1 - b) * 100, "%")
  cvarname <- paste0(cvarname, "_", (1 - b) * 100, "%")
  res <- future_map_dfr(1:nrow(df), 
                 function(n) {
                   if (n %% 1000 == 1) print(paste0("line: ", n, "/", nrow(df), " - ", date()))
                   CalcResul1CVaR(i = df[[n, "idExec"]],
                                  s = df[[n, "Sim"]], b = b, ...)
                 }, .progress = TRUE, .options = foption)
  res <- rename(res, !!varname := VaR, !!cvarname := CVaR)
  df <- select(df, -any_of(c(!!varname, !!cvarname)))
  bind_cols(df, res) 
}

GetSimilarCases <- function(x1, x2, correction = 1, By = "W_Variance", MR = MainResults) {
  #This function finds the most similar variance value in the other curve.
  MR <- ungroup(MR) # Ungroup to in order to row_number function below work correctly.
  V <- filter(MR, idExec == x1) %>% bind_rows(filter(MR, idExec == x2)) %>% # Filter only required cases (idExec)
    mutate(aux = case_when(idExec == x2 ~ .data[[By]] / correction^2, TRUE ~ .data[[By]])) %>% # Apply variance correction
    select(idExec, Sim, .data[[By]], aux)
  V <- mutate(V, row = row_number()) 
  d <- as.matrix(dist(V$aux, diag = TRUE, upper = TRUE)) 
  # Find out minimum distance values from one idExec compared to the other
  MinValues1 <- sapply(filter(V, idExec == x2) %>% pull(row), function(x) min(d[filter(V, idExec == x1) %>% pull(row), x]))
  MinValues2 <- sapply(filter(V, idExec == x1) %>% pull(row), function(x) min(d[filter(V, idExec == x2) %>% pull(row), x]))
  # Create a new tibble with a column (SimiliarTo) showing the most similar variance in the other case.
  V1 <- left_join(V, as_tibble(match(MinValues1, d) %>% 
                                 arrayInd(dim(d), useNames = TRUE)), by = c("row")) %>% 
    drop_na() %>% mutate(diff = MinValues1, idExecTo = x1)
  V2 <- left_join(V, as_tibble(match(MinValues2, d) %>% 
                                 arrayInd(dim(d), useNames = TRUE)), by = c("row" = "col")) %>% 
    rename(col = row.y) %>% drop_na() %>% mutate(diff = MinValues2, idExecTo = x2)
  V <- bind_rows(V1, V2) %>% mutate(SimilarTo = Sim[col]) %>% 
    select(idExec, idExecTo, Sim, SimilarTo, .data[[By]], aux, diff)
  arrange(V, diff) %>% distinct(idExec, SimilarTo, .keep_all = TRUE) %>% arrange(idExec, Sim)
}

PlotScatPlant <- function(x1, x2, UseSimilar = FALSE, ...) {
  equiv <- filter(MainResults, idExec == x1) %>% mutate(SimilarTo = Sim) %>% select(Sim, SimilarTo)
  if (UseSimilar) { # Group by similarity or by simulation order?
    equiv <- GetSimilarCases(x1, x2, ...) %>% filter(idExec == x1) %>% select(Sim, SimilarTo)
  }
  
  PlantResults %>% filter(idExec %in% c(x1, x2)) %>% 
    mutate(SimiPan = case_when(idExec == x1 ~ match(Sim, equiv$Sim) %>% # Create column with similarity panels.
                                 replace(values = equiv$SimilarTo[.]),
                               TRUE ~ Sim)) %>% drop_na(SimiPan) %>% 
    select(idExec, Sim, SimiPan,  Plant, RelCaptoLoad) %>% 
    pivot_wider(c("SimiPan", "Plant"), names_from = idExec, values_from = RelCaptoLoad) %>% drop_na() %>% 
    mutate(diff = .data[[x1]] - .data[[x2]]) %>% 
    ggplot() + geom_point(aes(x = .data[[x2]], y = .data[[x1]])) + geom_abline(slope = 1) + facet_wrap(~SimiPan)
}

PlotClevPlant <- function(x1, x2, yValue = "Wc", UseSimilar = FALSE, ...) {
  equiv <- filter(MainResults, idExec == x1) %>% mutate(SimilarTo = Sim) %>% select(Sim, SimilarTo)
  if (UseSimilar) { # Group by similarity or by simulation order?
    equiv <- GetSimilarCases(x1, x2, ...) %>% filter(idExec == x1) %>% select(Sim, SimilarTo)
  }
  ggplot(PlantResults %>% filter(idExec %in% c(x1, x2), !(Plant %in% "UTE_Inflex."),
                                 str_detect(Plant, "Demand", negate = TRUE), .data[[yValue]] > 0.0001) %>% 
           mutate(SimiPan = case_when(idExec == x1 ~ match(Sim, equiv$Sim) %>% # Create column with similarity panels.
                                        replace(values = equiv$SimilarTo[.]),
                                      TRUE ~ Sim)) %>% drop_na(SimiPan) %>% 
           pivot_wider(c("SimiPan", "Plant"), names_from = idExec, values_from = all_of(yValue)) %>% drop_na()) +
    geom_point(aes(x = Plant, y = .data[[x1]]), color = "green") + 
    geom_point(aes(x = Plant, y = .data[[x2]]), color = "red") + 
    geom_segment(aes(x = Plant, xend = Plant, y = .data[[x1]], yend = .data[[x2]])) + 
    facet_wrap(~SimiPan) + labs(x = "Plant name", y = yValue)
}

PlotCompHist <- function(x1, x2, yValue = "Wc", UseSimilar = FALSE, n = 6, bi = 30, xPlot = "CF.pos", ...) {
  equiv <- filter(MainResults, idExec == x1) %>% mutate(SimilarTo = Sim) %>% select(Sim, SimilarTo)
  if (UseSimilar) { # Group by similarity or by simulation order?
    equiv <- GetSimilarCases(x1, x2, ...) %>% filter(idExec == x1) %>% select(Sim, SimilarTo)
  }
  dataplot <- filter(left_join(MainResults, PlantResults) %>% ungroup() %>% 
                       left_join(PlantData, by = c("Plant" = "name", "Type", "Storage capacity", "UF")), 
                     idExec %in% c(x1, x2), str_detect(Plant, "Demand", negate = TRUE), # Remove load.
                     !(Plant %in% c("UTE_Inflex."))) %>% # Remove flat power plant
    mutate(SimiPan = case_when(idExec == x1 ~ match(Sim, equiv$Sim) %>%  # Create column with similarity panels.
                                 replace(values = equiv$SimilarTo[.]),
                               TRUE ~ Sim)) %>% 
    drop_na(SimiPan) %>% filter(SimiPan %in% equiv$SimilarTo) 
  u <- unique(dataplot$SimiPan)
  l <- length(u)
  if (l > n) { # Select a subset of "simulations" if there are too much.
    SelectedSimiPan <- u[round(seq(from = 1, to = l, 
                                   by = (l - 1) / (n - 1)))]
    dataplot <- filter(dataplot, SimiPan %in% SelectedSimiPan)
  }
  ggplot(dataplot) + 
    geom_histogram(aes(x = .data[[xPlot]], fill = Type, weight = .data[[yValue]]), bins = bi) + facet_grid(idExec~SimiPan) 
}

PlotCompGeneric <- function(IDs, UseSimilar = FALSE, n = 6, ...) {
  equiv <- filter(MainResults, idExec == IDs[1]) %>% mutate(SimilarTo = Sim) %>% 
    select(idExec, Sim, SimilarTo)
  if (UseSimilar) { # Group by similarity or by simulation order?
    for (i in 2:length(IDs)) {
      equiv <- bind_rows(equiv, GetSimilarCases(IDs[1], IDs[i], ...) %>% 
                           filter(idExecTo == IDs[1])) 
    }
  }
  dataplot <- filter(left_join(MainResults, PlantResults) %>% ungroup() %>% 
                       left_join(PlantData, by = c("Plant" = "name", "Type", "Storage capacity", "UF")), 
                     idExec %in% IDs, Type != "Load", # Remove load.
                    # !(Plant %in% c("UTE_Inflex."))
                     ) # Remove flat power plant 
  
  dataplot <- left_join(dataplot, equiv, by = c("idExec", "Sim")) %>% rename(SimiPan = SimilarTo) %>% 
    drop_na(SimiPan)
  u <- dataplot %>% arrange(SimiPan) %>% drop_na(idExecTo) %>% distinct(SimiPan) %>% deframe() # Unique values in order
  l <- length(u)
  if (l > n) { # Select a subset of "simulations" if there are too many of them.
    SelectedSimiPan <- u[round(seq(from = 1, to = l,
                                   by = (l - 1) / (n - 1)))]
    dataplot <- filter(dataplot, SimiPan %in% SelectedSimiPan)
  } else {
    dataplot <- filter(dataplot, SimiPan %in% u) # Remove unused SimiPan rows.
  }
  dataplot
}

PlotClevCF <- function(x1, x2, yValue = "Wc", UseSimilar = FALSE, ...) {
  # This function is similar to PlotClevPlant, but aggregates by CF, 
  # instead of showing each plant individually. It is the diference in the bars from PlotCompHist.
  equiv <- filter(MainResults, idExec == x1) %>% mutate(SimilarTo = Sim) %>% select(Sim, SimilarTo)
  if (UseSimilar) { # Group by similarity or by simulation order?
    equiv <- GetSimilarCases(x1, x2, ...) %>% filter(idExec == x1) %>% select(Sim, SimilarTo)
  }
  z <- filter(left_join(MainResults, PlantResults) %>% 
                left_join(PlantData, by = c("Plant" = "name", "Type", "Storage capacity", "UF")), 
              idExec %in% c(x1, x2), str_detect(Plant, "Demand", negate = TRUE), # Remove load.
              !(Plant %in% c("UTE_Inflex."))) %>% # Remove flat power plant
    mutate(SimiPan = case_when(idExec == x1 ~ match(Sim, equiv$Sim) %>%  # Create column with similarity panels.
                                 replace(values = equiv$SimilarTo[.]),
                               TRUE ~ Sim)) %>% 
    drop_na(SimiPan) %>% filter(SimiPan %in% equiv$SimilarTo)
  p1 <- ggplot(filter(z, idExec == x1)) + geom_histogram(aes(x = CF.pos, weight = .data[[yValue]])) + facet_grid(~SimiPan) 
  p2 <- ggplot(filter(z, idExec == x2)) + geom_histogram(aes(x = CF.pos, weight = .data[[yValue]])) + facet_grid(~SimiPan) 
  # Get data points from histogram
  pd1 <- layer_data(p1)
  pd2 <- layer_data(p2)
  pd <- full_join(select(pd1, y, x, PANEL), select(pd2, y, x, PANEL), by = c("x", "PANEL"), suffix = c("1", "2"))
  
  ggplot(pd) +
    geom_point(aes(x = x, y = y1), color = "greenyellow", alpha = 0.5) + 
    geom_point(aes(x = x, y = y2), color = "darkorange2", alpha = 0.5) + 
    geom_segment(aes(x = x, xend = x, y = y1, yend = y2, alpha = 0.4)) + 
    facet_wrap(~PANEL) + labs(x = "CF", y = yValue)
}

CalcResul1Balance <- function(i, s, Serie = SeriesComplete, m = 1, n = 1) {
  # m é o multiplicador
  # n é o fator para a demanda, útil para desconsiderar a demanda (n = 0)
  Selection <- PlantResults$idExec == i & PlantResults$Sim == s
  PR <- PlantResults[Selection,]
  weight <- filter(ExecParam, idExec == i)$Weight
  Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
  PR <- mutate(PR, Capacity = if_else(str_detect(Plant, "Demand"), 
                                      .data[[Colu]] * n, .data[[Colu]] * m))
  bal <- as.matrix(Serie[deframe(PR[, "Plant"])]) %*% (deframe(PR[, Colu]))
  bind_cols(idExec = i, Sim = s, Serie[1], Balance = as.vector(bal))
}

Calc1Variance <- function(i, s, WithLoad = FALSE) {
  SR <- PlantResults[PlantResults$idExec == i & PlantResults$Sim == s, c("Plant", "RelCaptoLoad", "Capacity")]
  if (!WithLoad) SR <- filter(SR, str_detect(Plant, "Demand", negate = TRUE))
  CovarMat2 <- CovarMat1[deframe(SR["Plant"]), deframe(SR["Plant"])] # Select only columns and rows used.
  deframe(deframe(SR["RelCaptoLoad"]) %*% CovarMat2 %*% deframe(SR["RelCaptoLoad"]))
}

RunNModes <- function(SeriesDesc = "", MinCost_IsoRisk = TRUE, MaxGen_IsoCap = TRUE,
                      MinCost_IsoGen = TRUE, MinCost_IsoCap = FALSE,
                      MinCap_IsoGen = FALSE, MaxGen_IsoCost = FALSE,
                      RNF = RiskNormFlag, RSF = RiskSampFlag) {
  rm(CovarMat, pos = ".GlobalEnv")
  RiskNormFlag <<- RNF
  RiskSampFlag <<- RSF
    # Run CVaR model
  if (MinCost_IsoRisk) {
    ModelFile <<- "src/MinCost_IsoRisk.mod"
    for (i in 1:length(ListBeta)) {
      beta <<- ListBeta[i]
      if (isVaR) {
        nsigma <<- qnorm(beta)
      } else {
        nsigma <<- dnorm(qnorm(beta))/(1 - beta)
      }
      for (j in 1:length(Listomega)) {
        omega <<- Listomega[j]
        Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5), " - beta=", beta, " - omega=", omega)
        print(paste0("Running: ", Notes))
        source("src/RunAndGetResults.R")
      }
    }
  }

  beta <<- 0.95
  nsigma <<- dnorm(qnorm(beta))/(1 - beta)
  omega <<- 0
  RiskNormFlag <<- 0
  RiskSampFlag <<- 0
    # Run Trad model
  if (MaxGen_IsoCap) {
    ModelFile <<- "src/MaxGen_IsoCap.mod"
    Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5))
    print(paste0("Running: ", Notes))
    source("src/RunAndGetResults.R")
  }
  # Run Cost model
  if (MinCost_IsoGen) {
    ModelFile <<- "src/MinCost_IsoGen.mod"
    Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5))
    print(paste0("Running: ", Notes))
    source("src/RunAndGetResults.R")
  }
  # Run MinCost_IsoCap model
  if (MinCost_IsoCap) {
    ModelFile <<- "src/MinCost_IsoCap.mod"
    Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5))
    print(paste0("Running: ", Notes))
    source("src/RunAndGetResults.R")
  }
  # Run MinCap_IsoGen model
  if (MinCap_IsoGen) {
    ModelFile <<- "src/MinCap_IsoGen.mod"
    Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5))
    print(paste0("Running: ", Notes))
    source("src/RunAndGetResults.R")
  }
  # Run MaxGen_IsoCost model
  if (MaxGen_IsoCost) {
    ModelFile <<- "src/MaxGen_IsoCost.mod"
    Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5), " R$ 500")
    PlantData[PlantData$Type == "Load", "CostMWh"] <<- -500
    print(paste0("Running: ", Notes))
    source("src/RunAndGetResults.R")
    ModelFile <<- "src/MaxGen_IsoCost.mod"
    Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5), " R$ 300")
    PlantData[PlantData$Type == "Load", "CostMWh"] <<- -300
    print(paste0("Running: ", Notes))
    source("src/RunAndGetResults.R")
    PlantData[PlantData$Type == "Load", "CostMWh"] <<- 0
  }
  # Restore flags
  RiskNormFlag <<- RNF
  RiskSampFlag <<- RSF
}

CalcGeoDivIndex <- function(x, param = "GenShare", correction = FALSE) { # Correction: if TRUE, does not consider the distance of a point to itself (0).
  x <- x %>% filter(Type != "Load") # Remove load
  disti <- distm(x[c("long", "lat")]) # Matrix with distance.
  weights <- deframe(x[c(param)]) / sum(deframe(x[c(param)]), na.rm = TRUE) # Normalize weights
  produto <- tcrossprod(weights) # Matrix with the crossproduct of param.
  tot <- sum(disti * produto, na.rm = TRUE)
  tot <- ifelse(correction, tot * (nrow(x) / (nrow(x) - 1)), tot)
  tot
}

CalcDivIndex <- function(x, param = "GenShare", DistMat = distancesSC, correction = FALSE,
                         ALPHA = 1, BETA = 1, Stirling = FALSE) { # Correction: if TRUE, does not consider the distance of a point to itself (0).
  # Stirling: Formulate exactly as Stirling (2007).                           
  x <- x %>% filter(Type != "Load") # Remove load
  PlantList <- x$Plant
  disti <- as.matrix(DistMat)[PlantList, PlantList] # Matrix with distance.
  weights <- deframe(x[c(param)]) / sum(deframe(x[c(param)]), na.rm = TRUE) # Normalize weights
  produto <- tcrossprod(weights) # Matrix with the crossproduct of param.
  if (Stirling) {
    disti <- disti * upper.tri(disti)
    produto <- produto * upper.tri(produto)
  }
  tot <- sum(disti^ALPHA * produto^BETA, na.rm = TRUE)
  tot <- ifelse(correction, tot * (nrow(x) / (nrow(x) - 1)), tot)
  tot
}

CalcGenTSeries <- function(i, s, Serie = SeriesComplete) {
  Selection <- PlantResults$idExec == i & PlantResults$Sim == s
  PR <- PlantResults[Selection,]
  weight <- filter(ExecParam, idExec == i)$Weight
  Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
  GenTS <- as_tibble(t(t(as.matrix(Serie[deframe(PR[, "Plant"])])) * deframe(PR[, Colu])))
  bind_cols(idExec = i, Sim = s, Serie[1], GenTS)
}

CalcCumDens <- function(vec, quant = 0.05) { # Calculate density value when cumulative probabilty is at least "quant"
  Dens <- density(vec)
  Area <- cumsum(diff(Dens$x) * na.omit(Dens$y + lag(Dens$y))/2)
  #cumsum(diff(dens$x) * dens$y)
  res <- Dens$y[sum(Area <= quant) + 2]
  res
}

CalcCVaR <- function(x, probs = 0.5, UpperTail = TRUE) {
  # Based on http://www-iam.mathematik.hu-berlin.de/~romisch/SP01/Uryasev.pdf
  sapply(probs, function(prob) {
    if (!UpperTail) x <- -x
    VaR <- quantile(x, prob, type = 1)
    psi <- mean(x <= VaR)
    lambda <- (psi - prob) / (1 - prob)
    CVaRp <- mean(x[x > VaR])
    if (is.nan(CVaRp)) {
      lambda <- 1
      CVaRp <- 0
    }
    (lambda * VaR + (1 - lambda) * CVaRp) * 
      ifelse(UpperTail, 1, -1)
  })
}

CVaRsort <- function(x, probs = 0.5, UpperTail = TRUE) {
  names(probs) <- paste0(probs * 100, "%")
  sapply(probs, function(y) mean(sort(x, decreasing = UpperTail)[1:((1 - y) * length(x))]))
}

# Esta função calcula, para cada nível de eficiência da bateria (eta) e a cada partamar de estabilização (pata), 
# qual a variação mínima de geração deve existir para que a bateria atue. 
limN <- function(eta, pata) { 
  2 * (1 - eta) * (1 - pata) / (1 + eta) + (1 - 2 * eta + eta^2) / (1 + 2 * eta + eta^2)
} 