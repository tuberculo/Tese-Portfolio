{string} Plants = ...;
{string} Groups = ...;
float GroupLHS[Groups][Plants] = ...;
float MaxPGroup[Groups] = ...;
float MinPGroup[Groups] = ...;
int Mtot = ...;
range M = 1..Mtot;
{int} Timeslice = ...;
float Cost[Plants] = ...;
float CFpos[Plants] = ...; // "positive" capacity factor
float CF[Plants] = ...;  // Capacity factor
float CFs[Timeslice][Plants] = ...;
float Covariance[Timeslice][Plants][Plants] = ...;
float SD_Max = ...;
float MaxLoad = ...;
int CostFlag = ...;  // Variáveis dummy para encontrar os menores valores de custo e variância
int VConFlag = ...;
int RiskNormFlag = ...;
int RiskSampFlag = ...;
float beta = ...;
float nsigma = ...;
float Y[M, Plants] = ...;
float MaxPot[Plants] = ...;
float MinPot[Plants] = ...;
float omega = ...;

/******************************************************************************
 * MODEL DECLARATIONS
 ******************************************************************************/
range float FloatRange = 0.0..infinity;
dvar float Pot[Plants] in FloatRange;  // Potência de cada usina
dvar float Z[M] in FloatRange;
dvar float SD[Timeslice] in FloatRange;
dvar float alfa in -infinity..infinity;

/******************************************************************************
 * MODEL
 ******************************************************************************/
dexpr float Obj =
  CostFlag * (sum(i in Plants : i != "Demand") Pot[i] * CF[i]) +
  (CostFlag - 1) * (sum(i,j in Plants, t in Timeslice) Covariance[t][i][j] * Pot[i] * Pot[j]);
// Custo em milhões de $ por ano.

maximize Obj;

subject to {
  LoadValue:
    Pot["Demand"] == MaxLoad; // Potência da "usina Carga" é igual à demanda máxima.
  forall (t in Timeslice) {
    DefineSD:
      SD[t]^2 >= (sum(i,j in Plants) (Covariance[t][i][j] * Pot[i] * Pot[j]));
    LimitSD:
      (VConFlag) * (SD[t]) <= SD_Max; //Não usa esta restrição se estiver minimizando a variância.
    RiskNorm:
      (RiskNormFlag) * ((sum (i in Plants) (Pot[i] * CFs[t][i])) - nsigma * SD[t]) >= 0;
    ConstantCapacity:
      (sum (i in Plants : i != "Demand") (Pot[i])) == MaxLoad;  //
  }
  RiskSamp: (RiskSampFlag) * (alfa - (1/((1 - beta) * Mtot)) * (sum(m in M) Z[m])) >= omega; //CVaR;  //alfa é o valor do VaR no ponto ótimo (variável de decisão), beta é o percentil, M é o total de amostras (J é usado geralmente)
  forall (m in M)
    defineZ:
    (RiskSampFlag) * (alfa - sum (i in Plants) (Y[m, i] * Pot[i])) <= Z[m] ; //Y é a geração em pu da usina i na amostra m.
  
  forall (i in Plants : i != "Demand") {
    MaxPowerEachPlant:
      Pot[i] <= MaxPot[i] ;
    MinPowerEachPlant:
      Pot[i] >= MinPot[i] ;
  }

  forall (i in Groups) {
    MaxPowerGroup:
      (sum (j in Plants) GroupLHS[i][j] * Pot[j]) <= MaxPGroup[i] ;
    MinPowerGroup:
      (sum (j in Plants) GroupLHS[i][j] * Pot[j]) >= MinPGroup[i] ;
  }
}

float PortfCost = sum(i in Plants : i != "Demand") Pot[i] * Cost[i] * CFpos[i] * 8.76 / 1000;
float MeanGen = sum(i in Plants : i != "Demand") Pot[i] * CF[i]; // in MWyear
float PortfVariance = max(t in Timeslice) sum(i,j in Plants) Covariance[t][i][j] * Pot[i] * Pot[j];
float SD_Total = (maxl(PortfVariance, 0))^(1/2);
float diff_SD = max(t in Timeslice) (SD[t]) - SD_Total;

execute DISPLAY {
  writeln("Cost               : ", PortfCost);
  writeln("Mean Generation    : ", MeanGen);
  writeln("Variance           : ", PortfVariance);
  writeln("Standard deviation : ", SD_Total);
  writeln("Number of SD       : ", nsigma);
  writeln("Beta               : ", beta);
  writeln("Expected VaR       : ", alfa);
  writeln("SD slack           : ", diff_SD);
}

execute
{
var f=new IloOplOutputFile("../outputOPL.txt", true);
f.write("Cost ",PortfCost, ";");
f.write("MeanGen ", MeanGen, ";");
f.write("Variance ",PortfVariance, ";");
f.write("Standard_deviation ",SD_Total, ";");
f.write("N_of_SD ", nsigma, ";");
f.write("Beta ", beta, ";");
f.write("Expected_VaR ", alfa, ";");
f.write("Capacity ", Pot);

f.writeln("*");
f.close();
}
