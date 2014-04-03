set PROD;     
param T > 0;  

param rate {PROD} > 0;          
param inv0 {PROD} >= 0;         
param avail {1..T} >= 0;        
param market {PROD,1..T} >= 0;  

param prodcost {PROD} >= 0;     
param invcost {PROD} >= 0;      
param revenue {PROD,1..T} >= 0; 

var Make {PROD,1..T} >= 0;      
var Inv {PROD,0..T} >= 0;       
var Sell {p in PROD, t in 1..T} >= 0, <= market[p,t]; 

maximize Total_Profit:
   sum {p in PROD, t in 1..T} (revenue[p,t]*Sell[p,t] -
      prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t]);

               

subject to Time {t in 1..T}:
   sum {p in PROD} (1/rate[p]) * Make[p,t] <= avail[t];

               
               

subject to Init_Inv {p in PROD}:  Inv[p,0] = inv0[p];

               

subject to Balance {p in PROD, t in 1..T}:
   Make[p,t] + Inv[p,t-1] = Sell[p,t] + Inv[p,t];

               
               
