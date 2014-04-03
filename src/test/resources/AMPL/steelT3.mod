set PROD;         
set AREA {PROD};  
param T > 0;      

param rate {PROD} > 0;          
param inv0 {PROD} >= 0;         
param avail {1..T} >= 0;        
param market {p in PROD, AREA[p], 1..T} >= 0;  
                                

param prodcost {PROD} >= 0;     
param invcost {PROD} >= 0;      
param revenue {p in PROD, AREA[p], 1..T} >= 0; 
                                

var Make {PROD,1..T} >= 0;      
var Inv {PROD,0..T} >= 0;       
var Sell {p in PROD, a in AREA[p], t in 1..T}   
                    >= 0, <= market[p,a,t];

maximize Total_Profit:
   sum {p in PROD, t in 1..T} 
      (sum {a in AREA[p]} revenue[p,a,t]*Sell[p,a,t] -
         prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t]);

           

subject to Time {t in 1..T}:
   sum {p in PROD} (1/rate[p]) * Make[p,t] <= avail[t];

           
           

subject to Init_Inv {p in PROD}:  Inv[p,0] = inv0[p];

           

subject to Balance {p in PROD, t in 1..T}:
   Make[p,t] + Inv[p,t-1]
      = sum {a in AREA[p]} Sell[p,a,t] + Inv[p,t];

           
           
