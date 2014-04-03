param roll_width > 0;         
 
set WIDTHS;                   
param orders {WIDTHS} > 0;    

param nPAT integer >= 0;      
set PATTERNS = 1.. nPAT;      

param nbr {WIDTHS,PATTERNS} integer >= 0;
         

var Cut {PATTERNS} integer >= 0;   

minimize Number:                   
   sum {j in PATTERNS} Cut[j];   

subject to Fill {i in WIDTHS}:
   sum {j in PATTERNS} nbr[i,j] * Cut[j] >= orders[i];

                                   
                                   





param price {WIDTHS} default 0.0;

var Use {WIDTHS} integer >= 0;

minimize Reduced_Cost:  
   1 - sum {i in WIDTHS} price[i] * Use[i];

subject to Width_Limit:  
   sum {i in WIDTHS} i * Use[i] <= roll_width;
