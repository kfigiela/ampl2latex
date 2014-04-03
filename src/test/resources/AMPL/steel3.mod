set PROD;  

param rate {PROD} > 0;     
param avail >= 0;          
param profit {PROD};       

param commit {PROD} >= 0;  
param market {PROD} >= 0;  

var Make {p in PROD} >= commit[p], <= market[p]; 

maximize Total_Profit: sum {p in PROD} profit[p] * Make[p];

               

subject to Time: sum {p in PROD} (1/rate[p]) * Make[p] <= avail;

               
               
