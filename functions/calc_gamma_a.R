# calc_gamma_a
# Calculate the gamma_a vector to be used in the missingness model 
#
# p: number of auxiliary variables
# gam: non-zero coefficient of variables in missingness model

calc_gamma_a <- function(p, gam){
  
  nblocks <- 3 # number of blocks of auxiliary variables in correlation matrix
  g <- p %/% nblocks # number of auxiliary variables in each group
  rem <- p %% nblocks # remainder (if p not exactly divisible by nblocks) These go into low correlation group
  gamma_a <- gam * c(rep(c(rep(0, floor(g/2)), rep(1, ceiling(g/2))), nblocks), rep(0, rem))
  
}
