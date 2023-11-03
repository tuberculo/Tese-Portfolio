int Ttot = ...;
range T = 1..Ttot;
float G[T] = ...;
float CF = ...; //Average capacity factor
float StoragetoCapRatio = ...;
float Bmax = 0.01; 
float MaxStoLev = Bmax * StoragetoCapRatio;
float efi = ...;

execute PARAMS {
cplex.tilim = 0;
//cplex.exportModel("lpex1.lp");
//cplex.writeSolution("solteste.sol");
}

/******************************************************************************
 * MODEL DECLARATIONS
 ******************************************************************************/

range float FloatRange = 0.0..infinity;
dvar float B[T] in -1..1;
dvar float StoLev[T] in FloatRange;
dvar float Loss[T] in FloatRange;

/******************************************************************************
 * MODEL
 ******************************************************************************/

dexpr float Obj =
// Minimize cost given by the square complement of combined generation. 
  sum(t in T) (1 - B[t] - G[t])^2;

minimize Obj;

subject to {
  forall (t in T : t != 1)
    StorageOperation: B[t] == (StoLev[t-1] - StoLev[t]) - Loss[t];
  forall (t in T : t != 1)
    LossConstraint: Loss[t] >= (StoLev[t] - StoLev[t-1]) * (1 - efi);
  forall (t in T) {
    MaxStorageLevel: StoLev[t] <= MaxStoLev; //
    MaxDischarge: B[t] <= Bmax;
    MaxCharge: B[t] >= -Bmax;
  }
  StoLev[1] == MaxStoLev/2;
  StoLev[Ttot] == MaxStoLev/2;
  B[1] == 0;
  Loss[1] == 0;
}
