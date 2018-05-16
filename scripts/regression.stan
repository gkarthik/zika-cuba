data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}

/* y = alpha + (beta * gamma * x) */
parameters {
  real alpha;
  real beta;
  real<lower=0,upper=1> gamma;
  real<lower=0> sigma;
}

model {
  y ~ normal(alpha + beta * gamma * x, sigma);
}
