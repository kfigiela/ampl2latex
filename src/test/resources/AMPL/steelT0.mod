set PROD;     
param T > 0;  

param rate {PROD} > 0;         
param avail {1..T} >= 0;       
param profit {PROD,1..T};      
param market {PROD,1..T} >= 0; 

var Make {p in PROD, t in 1..T} >= 0, <= market[p,t];
			       

maximize Total_Profit:
   sum {p in PROD, t in 1..T} profit[p,t] * Make[p,t];

	

subject to Time {t in 1..T}:
   sum {p in PROD} (1/rate[p]) * Make[p,t] <= avail[t];

	
	
