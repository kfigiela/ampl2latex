set INGREDIENTS "I | All possible ingredients for the cat food";
set REQUIREMENTS "R | Requirements for a can of cat food";

param Cost "c | The cost (per gram) of the ingredients" {i in INGREDIENTS};
param Contributes "q | The contribution per g of ingredients to requirements" {r in REQUIREMENTS, i in INGREDIENTS};

param Lower "r^{min} | Minimal required amount of requirement" {REQUIREMENTS} default -Infinity;
param Upper "r^{max} | Maximal required amount of requirement" {r in REQUIREMENTS} >= Lower[r], default Infinity;

var Amount "A | is amount (g) of each ingredient used" {i in INGREDIENTS} >= 0;

minimize TotalCost "is to minimise the cost per can": sum {i in INGREDIENTS} Cost[i] * Amount[i];

subject to MeetRequirements "checks if solution meets the nutritional requirements" {r in REQUIREMENTS} :
  Lower[r] <= sum {i in INGREDIENTS} Contributes[r, i] * Amount[i] <= Upper[r];
