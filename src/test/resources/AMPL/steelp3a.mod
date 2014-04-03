set PROD;      
param T > 0;   

param rate {PROD} > 0;          
param inv0 {PROD} >= 0;         
param commit {PROD,1..T} >= 0;  
param market {PROD,1..T} >= 0;  

param avail_min {1..T} >= 0;                 
param avail_max {t in 1..T} >= avail_min[t]; 
param time_penalty {1..T} > 0;

param prodcost {PROD} >= 0;     
param invcost {PROD} >= 0;      
param revenue {PROD,1..T} >= 0; 

var Make {PROD,1..T} >= 0;                  
var Inv {PROD,0..T} >= 0;                   
var Sell1 {p in PROD, t in 1..T} 
   >= 0, <= market[p,t]-commit[p,t];        
var Sell0 {p in PROD, t in 1..T}
   >= 0, <= commit[p,t];                    

var Use1 {t in 1..T} >= 0, <= avail_min[t];
var Use2 {t in 1..T} >= 0, <= avail_max[t]-avail_min[t];

maximize Total_Profit: 
   sum {p in PROD, t in 1..T} 
     (revenue[p,t]*(commit[p,t]+Sell1[p,t]-Sell0[p,t]) -
      prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t]) -
   sum {t in 1..T} time_penalty[t] * Use2[t] -
   sum {p in PROD, t in 1..T} 1000000*Sell0[p,t];

               

subject to Time {t in 1..T}:  
   sum {p in PROD} (1/rate[p]) * Make[p,t] = Use1[t] + Use2[t];

               
               

subject to Init_Inv {p in PROD}:  Inv[p,0] = inv0[p];

               

subject to Balance {p in PROD, t in 1..T}:
   Make[p,t] + Inv[p,t-1] = (commit[p,t]+Sell1[p,t]-Sell0[p,t]) + Inv[p,t];

               
               
