## slice sampler for 2VN normal
conditional_normal = function(index, x_curr, m, v) {
  ## index will be one of (1, 2)
  mu = m[-index]
  v_mat  = v[-index, -index]
  cross_cov = v[index, -index]
  
  res = list()
  
  res$m = m[index] + cross_cov * (1/v_mat) * (x_curr[-index] - mu)
  res$v = v[index, index] - (cross_cov^2)/v_mat
  
  res
}

slice_sample_2vn = function(n, x_init, m, v) { 
  
  samples = matrix(ncol = 2, nrow = n)
  y0_trace = matrix(NA, ncol  = 2, nrow = n)
  x_curr = x_init
  
  for(i in 1:n) {
    
    for(j in 1:2) {
      moments = conditional_normal(j, x_curr, m, v)
      #print(paste0("moments = ", moments))
      log_phi_curr = -0.5*((x_curr[j] - moments$m)^2)/moments$v
      log_y0 = log(runif(1)) + log_phi_curr
      y0_trace[i, j]  = log_y0
      #print(paste0("y0 = ",  log_y0))
      unif_lims = moments$m + c(-1, 1) * sqrt(moments$v * (-2 * log_y0))
      #print(paste0("unif_lims = ", unif_lims))
      x_new = runif(1, min = min(unif_lims), max = max(unif_lims))
      #print(paste0("x_new = ", x_new))
      x_curr[j] = x_new
    }
    
    samples[i, ] = x_curr
    
  }
  list(samples  = samples,
       y0 = y0_trace)
  
}

mm = c(0,5)
cv = matrix(c(1, 0.8, 0.8, 1), ncol = 2)
my_ss = slice_sample_2vn(10000, c(-1000,2000), mm, cv)
samples= my_ss$samples
par(mfrow = c(2,2))
plot(ts(samples[,1]))
plot(ts(samples[,2]))
hist(samples[,1], prob = TRUE)
hist(samples[,2], prob = TRUE)
xx = seq(-10, 10, length = 10^4)
lines(xx, dnorm(xx, mean = 5))
plot(samples, type = "l")
acf(samples)

