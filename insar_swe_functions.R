# functions used in InSAR SWE analysis
# jack tarricone
# july 18, 2022

depth_from_phase <-function(delta_phase, inc_angle, perm, wavelength = 23.8403545){
  
  delta_z = (-delta_phase * wavelength) / (4 * pi * (cos(inc_angle) - sqrt(perm - sin(inc_angle)^2)))
  
}

