set ORIG;   
set DEST;   

param supply {ORIG} >= 0;   
param demand {DEST} >= 0;   


param cost {ORIG,DEST} >= 0;   
var Trans {ORIG,DEST} >= 0;    

minimize Total_Cost:
   sum {i in ORIG, j in DEST} cost[i,j] * Trans[i,j];

minimize Pref_of {i in ORIG}: sum {j in DEST} cost[i,j] * Trans[i,j];

subject to Supply {i in ORIG}:
   sum {j in DEST} Trans[i,j] = supply[i];

subject to Demand {j in DEST}:
   sum {i in ORIG} Trans[i,j] = demand[j];
