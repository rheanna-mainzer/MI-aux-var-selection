calc_betax <- function(cor){
  
  rho_xy <- cor["X", "Y"] 
  rho_yz <- cor["Y", "Z"]
  rho_xz <- cor["X", "Z"]
  
  # Calculate beta_x using conditional probability formula
  (rho_xy - rho_yz * rho_xz) / (1 - rho_xz^2)
  
}