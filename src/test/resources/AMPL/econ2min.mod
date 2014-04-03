set PROD;   
set ACT;   

param cost {ACT} > 0;     
param demand {PROD} >= 0;  
param io {PROD,ACT} >= 0;  

param level_min {ACT} > 0;  
param level_max {ACT} > 0;  

var Level {j in ACT} >= level_min[j], <= level_max[j];

minimize Total_Cost:  sum {j in ACT} cost[j] * Level[j];

subject to Demand {i in PROD}:
   sum {j in ACT} io[i,j] * Level[j] >= demand[i];
