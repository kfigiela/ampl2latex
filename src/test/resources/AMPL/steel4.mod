set PROD;   
set STAGE;  

param rate {PROD,STAGE} > 0; 
param avail {STAGE} >= 0;    
param profit {PROD};         

param commit {PROD} >= 0;    
param market {PROD} >= 0;    

var Make {p in PROD} >= commit[p], <= market[p]; 

maximize Total_Profit: sum {p in PROD} profit[p] * Make[p];

               

subject to Time {s in STAGE}:
   sum {p in PROD} (1/rate[p,s]) * Make[p] <= avail[s];

               
               
