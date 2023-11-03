{string} Usinas = ...;
{string} Groups = ...;
float GroupLHS[Groups][Usinas] = ...;
float MaxWGroup[Groups] = ...;
float MinWGroup[Groups] = ...;
int Mtot = ...;
range M = 1..Mtot;
{int} Timeslice = ...;
float Custo[Usinas] = ...;
float RelCF[Timeslice][Usinas] = ...; // Timeslice CF over "positive" CF
float CovarianciaN[Timeslice][Usinas][Usinas] = ...;
float SD_Max = ...;
int CostFlag = ...;  // Variáveis dummy para encontrar os menores valores de custo e variância
int VConFlag = ...;
int RiskNormFlag = ...;
int RiskCopFlag = ...;
float beta = ...;
float nsigma = ...;
float Y_N[M, Usinas] = ...;
float MaxW[Usinas] = ...;
float MinW[Usinas] = ...;
float ES = ...;

/******************************************************************************
 * MODEL DECLARATIONS
 ******************************************************************************/
range float FloatRange = 0.0..infinity;
dvar float W[Usinas] in FloatRange;
dvar float Z[M] in FloatRange;
dvar float SD[Timeslice] in FloatRange;
dvar float alfa in -infinity..infinity;

/******************************************************************************
 * MODEL
 ******************************************************************************/
dexpr float Objetivo =
  CostFlag * (sum(i in Usinas : i != "Demanda") W[i] * Custo[i]) +
  (1 - CostFlag) * (sum(i,j in Usinas, t in Timeslice) CovarianciaN[t][i][j] * W[i] * W[j]);
// Custo em $/MWh.

minimize Objetivo;

subject to {
  LoadValue: 
    W["Demanda"] == 1; // Potência da "usina Carga" é igual à 1.
  forall (t in Timeslice) {
    DefineSD: 
      SD[t]^2 >= (sum(i,j in Usinas) (CovarianciaN[t][i][j] * W[i] * W[j]));
    LimitSD: 
      (VConFlag) * (SD[t]) <= SD_Max; //Não usa esta restrição se estiver minimizando a variância.
    RiskNorm: 
      (RiskNormFlag) * (sum (i in Usinas) (RelCF[t][i] * W[i]) - nsigma * SD[t]) >= 0;
//    MeanDemand: 
//      (sum (i in Usinas) (RelCF[t][i] * W[i])) >= 0;  // 
  }
  RiskCopula: (RiskCopFlag) * (alfa - (1/((1 - beta) * Mtot)) * (sum(m in M) Z[m])) >= ES; //CVaR;  //alfa é o valor do VAR no ponto ótimo (variável de decisão), beta é o percentil, M é o total de amostras (J é usado geralmente)
  forall (m in M)
    defineZ:
      (RiskCopFlag) * (alfa - sum (i in Usinas) (-Y_N[m, i] * W[i])) <= Z[m] ; //Y é a geração em pu da usina i na amostra m.
  
  forall (i in Usinas : i != "Demanda") {
    MaxWeightEachPlant: 
      W[i] <= MaxW[i] ;
    MinWeightEachPlant: 
      W[i] >= MinW[i] ;
  }
  
  forall (i in Groups) {
    MaxWeightGroup: 
      (sum (j in Usinas) GroupLHS[i][j] * W[j]) <= MaxWGroup[i] ;
    MinWeightGroup: 
      (sum (j in Usinas) GroupLHS[i][j] * W[j]) >= MinWGroup[i] ;
  }
}

float CustoTotal = sum(i in Usinas : i != "Demanda") W[i] * Custo[i] ;
float VarianciaTotal = max(t in Timeslice) sum(i,j in Usinas) CovarianciaN[t][i][j] * W[i] * W[j];
float DP_Total = (maxl(VarianciaTotal, 0))^(1/2);
float diff_SD = max(t in Timeslice) (SD[t]) - DP_Total;

execute DISPLAY {
  writeln("Cost               : ", CustoTotal);
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
f.write("Variance ",VarianciaTotal, ";");
f.write("Standard_deviation ",DP_Total, ";");
f.write("N_of_SD ", nsigma, ";");
f.write("Beta ", beta, ";");
f.write("Expected_VaR ", alfa, ";");
f.write("Weight ", W);
//f.write("Z ", Z, ";");
//f.write("Y ", Y, ";");

f.writeln("*");
f.close();
}
