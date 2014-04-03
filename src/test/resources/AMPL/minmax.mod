
set PEOPLE;
set PROJECTS;

param supply {PEOPLE} >= 0;   
param demand {PROJECTS} >= 0; 



param cost {PEOPLE,PROJECTS} >= 0;   
param limit {PEOPLE,PROJECTS} >= 0;  
                                     

var M;
var Assign {i in PEOPLE, j in PROJECTS} >= 0, <= limit[i,j];

minimize Max_Cost: M;

subject to M_def {i in PEOPLE}:
   M >= sum {j in PROJECTS} cost[i,j] * Assign[i,j];

subject to Supply {i in PEOPLE}:
   sum {j in PROJECTS} Assign[i,j] = supply[i];

subject to Demand {j in PROJECTS}:
   sum {i in PEOPLE} Assign[i,j] = demand[j];
