set ORIG;   
set DEST;   

set LINKS within {ORIG,DEST};

param supply {ORIG} >= 0;   
param demand {DEST} >= 0;   


param cost {LINKS} >= 0;   
var Trans {LINKS} >= 0;    

minimize Total_Cost:
   sum {(i,j) in LINKS} cost[i,j] * Trans[i,j];

subject to Supply {i in ORIG}:
   sum {(i,j) in LINKS} Trans[i,j] = supply[i];

subject to Demand {j in DEST}:
   sum {(i,j) in LINKS} Trans[i,j] = demand[j];

