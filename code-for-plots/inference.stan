// used to infer x for a toy 1d simulator
// assuming N(0,1) noise
functions {
	real f (real x) {
	  real result;
	  result = 0.6 * sin(2 * pi() * x) + 2*cos(3 * pi() * x) + sin(4 * pi() * x) + 2 * x;
	  return(result);
	}
}


data {
  real y_obs; // observed value
  real sigma; // noise of y_obs
}


parameters {
  real<lower = 0, upper = 1> x; // value to be inferred
}

model{
  y_obs ~ normal(f(x), sigma);
  
  x ~ uniform(0,1);
}
