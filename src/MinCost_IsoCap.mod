{string} Usinas = ...;
{string} Groups = ...;
float GroupLHS[Groups][Usinas] = ...;
float MaxPGroup[Groups] = ...;
float MinPGroup[Groups] = ...;
int Mtot = ...;
range M = 1..Mtot;
{int} Timeslice = ...;
float Custo[Usinas] = ...;
float CFpos[Usinas] = ...; // "positive" capacity factor
float CF[Usinas] = ...;  // Capacity factor
float CFs[Timeslice][Usinas] = ...;
float Covariancia[Timeslice][Usinas][Usinas] = ...;
float SD_Max = ...;
//float Carga[Timeslice] = ...;
float CargaMax = ...;
int CostFlag = ...;  // Variáveis dummy para encontrar os menores valores de custo e variância
int VConFlag = ...;
int RiskNormFlag = ...;
int RiskCopFlag = ...;
float beta = ...;
float nsigma = ...;
float Y[M, Usinas] = ...;
float MaxPot[Usinas] = ...;
float MinPot[Usinas] = ...;
float ES = ...;

/******************************************************************************
 * MODEL DECLARATIONS
 ******************************************************************************/
range float FloatRange = 0.0..infinity;
dvar float Pot[Usinas] in FloatRange;  // Potência de cada usina
dvar float Z[M] in FloatRange;
dvar float SD[Timeslice] in FloatRange;
dvar float alfa in -infinity..infinity;

/******************************************************************************
 * MODEL
 ******************************************************************************/
dexpr float Objetivo =
  CostFlag * (sum(i in Usinas : i != "Demanda") Pot[i] * Custo[i] * CFpos[i]) * 8.76 / 1000 +
  (1 - CostFlag) * (sum(i,j in Usinas, t in Timeslice) Covariancia[t][i][j] * Pot[i] * Pot[j]);
// Custo em milhões de $ por ano.

minimize Objetivo;

subject to {
  LoadValue:
    Pot["Demanda"] == CargaMax; // Potência da "usina Carga" é igual à demanda máxima.
  forall (t in Timeslice) {
    DefineSD:
      SD[t]^2 >= (sum(i,j in Usinas) (Covariancia[t][i][j] * Pot[i] * Pot[j]));
    LimitSD:
      (VConFlag) * (SD[t]) <= SD_Max; //Não usa esta restrição se estiver minimizando a variância.
    RiskNorm:
      (RiskNormFlag) * ((sum (i in Usinas) (Pot[i] * CFs[t][i])) - nsigma * SD[t]) >= 0;
    ConstantCapacity:
      (sum (i in Usinas : i != "Demanda") (Pot[i])) == CargaMax;  //
  }
  RiskCopula: (RiskCopFlag) * (alfa - (1/((1 - beta) * Mtot)) * (sum(m in M) Z[m])) >= ES; //CVaR;  //alfa é o valor do VAR no ponto ótimo (variável de decisão), beta é o percentil, M é o total de amostras (J é usado geralmente)
  forall (m in M)
    defineZ:
    (RiskCopFlag) * (alfa - sum (i in Usinas) (Y[m, i] * Pot[i])) <= Z[m] ; //Y é a geração em pu da usina i na amostra m.
  
  forall (i in Usinas : i != "Demanda") {
    MaxPowerEachPlant:
      Pot[i] <= MaxPot[i] ;
    MinPowerEachPlant:
      Pot[i] >= MinPot[i] ;
  }

  forall (i in Groups) {
    MaxPowerGroup:
      (sum (j in Usinas) GroupLHS[i][j] * Pot[j]) <= MaxPGroup[i] ;
    MinPowerGroup:
      (sum (j in Usinas) GroupLHS[i][j] * Pot[j]) >= MinPGroup[i] ;
  }
}

float CustoTotal = sum(i in Usinas : i != "Demanda") Pot[i] * Custo[i] * CFpos[i] * 8.76 / 1000;
float MeanGen = sum(i in Usinas : i != "Demanda") Pot[i] * CFpos[i]; // in MWyear
float VarianciaTotal = max(t in Timeslice) sum(i,j in Usinas) Covariancia[t][i][j] * Pot[i] * Pot[j];
float DP_Total = (maxl(VarianciaTotal, 0))^(1/2);
float diff_SD = max(t in Timeslice) (SD[t]) - DP_Total;

execute DISPLAY {
  writeln("Cost               : ", CustoTotal);
  writeln("Mean Generation    : ", MeanGen);
  writeln("Variance           : ", VarianciaTotal);
  writeln("Standard deviation : ", DP_Total);
  writeln("Number of SD       : ", nsigma);
  writeln("Beta               : ", beta);
  writeln("Expected VaR       : ", alfa);
  writeln("SD slack           : ", diff_SD);
  //  writeln("Potência      : ", Solution);
  //  writeln("Z             : ", Z)
}

execute
{
var f=new IloOplOutputFile("../saidaOPL.txt", true);
f.write("Cost ",CustoTotal, ";");
f.write("MeanGen ", MeanGen, ";");
f.write("Variance ",VarianciaTotal, ";");
f.write("Standard_deviation ",DP_Total, ";");
f.write("N_of_SD ", nsigma, ";");
f.write("Beta ", beta, ";");
f.write("Expected_VaR ", alfa, ";");
f.write("Capacity ", Pot);
//f.write("Z ", Z, ";");
//f.write("Y ", Y, ";");

f.writeln("*");
f.close();
}
