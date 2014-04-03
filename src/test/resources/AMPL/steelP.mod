set ORIG;   
set DEST;   
set PROD;   

param rate {ORIG,PROD} > 0;     
param avail {ORIG} >= 0;        
param demand {DEST,PROD} >= 0;  

param make_cost {ORIG,PROD} >= 0;        
param trans_cost {ORIG,DEST,PROD} >= 0;  

var Make {ORIG,PROD} >= 0;       
var Trans {ORIG,DEST,PROD} >= 0; 

minimize Total_Cost:
   sum {i in ORIG, p in PROD} make_cost[i,p] * Make[i,p] +
   sum {i in ORIG, j in DEST, p in PROD}
			trans_cost[i,j,p] * Trans[i,j,p];

subject to Time {i in ORIG}:
   sum {p in PROD} (1/rate[i,p]) * Make[i,p] <= avail[i];

subject to Supply {i in ORIG, p in PROD}:
   sum {j in DEST} Trans[i,j,p] = Make[i,p];

subject to Demand {j in DEST, p in PROD}:
   sum {i in ORIG} Trans[i,j,p] = demand[j,p];
