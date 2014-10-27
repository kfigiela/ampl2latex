set INPUT;    # inputs
set OUTPUT;   # outputs

param cost {INPUT} > 0;
param in_min {INPUT} >= 0;
param in_max {j in INPUT} >= in_min[j];

param out_min {OUTPUT} >= 0;
param out_max {i in OUTPUT} >= out_min[i];

param io {OUTPUT,INPUT} >= 0;

var X {j in INPUT} >= in_min[j], <= in_max[j];

minimize Total_Cost:
	sum {i in ORIG, j in DEST}
		<<limit1[i,j], limit2[i,j];
			rate1[i,j], rate2[i,j], rate3[i,j]>> Trans[i,j];

subject to Outputs {i in OUTPUT}:
   out_min[i] <= sum {j in INPUT} io[i,j] * X[j] <= out_max[i];
