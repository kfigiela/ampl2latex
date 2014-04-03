set ORIG;   
set DEST;   
set PROD;   

set orig {PROD} within ORIG;
set dest {PROD} within DEST;
set links {p in PROD} = orig[p] cross dest[p];

param supply {p in PROD, orig[p]} >= 0; 
param demand {p in PROD, dest[p]} >= 0; 

param limit {ORIG,DEST} >= 0;

param cost {p in PROD, links[p]} >= 0;  
var Trans {p in PROD, links[p]} >= 0;   

minimize Total_Cost:
   sum {p in PROD, (i,j) in links[p]} cost[p,i,j] * Trans[p,i,j];

subject to Supply {p in PROD, i in orig[p]}:
   sum {j in dest[p]} Trans[p,i,j] = supply[p,i];

subject to Demand {p in PROD, j in dest[p]}:
   sum {i in orig[p]} Trans[p,i,j] = demand[p,j];


