data {
  real y;
  real<lower = 0> sigma; // sd(y)
}

parameters {
  real x;
}

model {
  //prior
  x ~ uniform(0, 1);
  // llhood
  y ~ normal(sin(4 * pi() * x), sigma);
}
