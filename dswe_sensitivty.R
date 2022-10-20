# workshopping sensivity analysis
# jack tarricone
# october 20, 2022

library(terra)

depth_from_phase <-function(delta_phase, inc_angle, perm, wavelength = 23.8403545){
  
  delta_z = (-delta_phase * wavelength) / (4 * pi * (cos(inc_angle) - sqrt(perm - sin(inc_angle)^2)))
  
}


swe_from_phase <-function(delta_phase, inc_angle, perm, density, wavelength = 23.8403545){
  
  delta_swe = ((-delta_phase * wavelength) * density *  (1/(4 * pi * (cos(inc_angle) - sqrt(perm - sin(inc_angle)^2)))))
  
}

# create function
guneriussen_perm <-function(density){
  
  # convert density to g/cm^3
  perm <- 1 + 1.6 * (density/1000) + 1.8 * (density/1000)^3
  return(perm)
}

# calculate
snow_density <- 300 # kg m^-3
perm <-guneriussen_perm(density = snow_density)
print(paste0("permittivy: ", signif(perm,3)))

# single test
dswe <-swe_from_phase(delta_phase = 1,
                      inc_angle = .5,
                      perm = perm,
                      density = snow_density/1000)


# generate norm distribution of density values
density_dist <-rnorm(100000, mean = 300, sd = 60)
hist(density_dist, breaks = 100) # quick hist
head(density_dist)
mean(density_dist)

# convert to perm
perm_dist <-guneriussen_perm(density_dist)
hist(perm_dist, breaks = 100)
mean(perm_dist)

# generate SWE distribution
swe_dist_density <-swe_from_phase(delta_phase = 1,
                          inc_angle = .75,
                          perm = perm_dist,
                          density = density_dist/1000)
# plot
hist(swe_dist_density, breaks = 100)
mean(swe_dist_density)
sd(swe_dist_density)

# calculate standard error
se <-sd(swe_dist)/sqrt(length((swe_dist)))


###########
# generate inc angle
###########

# bring in rast
jpl_inc <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/jpl_inc_rad.tiff")
hist(jpl_inc, breaks = 100)
inc_df <-as.data.frame(jpl_inc)

# calc stats
inc_mean <-as.numeric(global(jpl_inc, mean, na.rm = TRUE))
inc_sd <-as.numeric(global(jpl_inc, sd, na.rm = TRUE))

# creat dist
inc_dist <-rnorm(100000, mean = inc_mean, sd = inc_sd)
hist(inc_dist, breaks = 100)

# generate SWE distribution
swe_dist_inc <-swe_from_phase(delta_phase = 1,
                          inc_angle = inc_dist,
                          perm = perm,
                          density = snow_density/1000)

hist(swe_dist_inc, breaks = 100)

#######
# phase dist
######

# bring in rast
phase_rast <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw_corrected_feb12-19.tif")
hist(phase_rast, breaks = 100)

# calc stats
phase_mean <-as.numeric(global(phase_rast, mean, na.rm = TRUE))
phase_sd <-as.numeric(global(phase_rast, sd, na.rm = TRUE))

# creat dist
phase_dist <-rnorm(100000, mean = phase_mean, sd = phase_sd)
hist(phase_dist, breaks = 100)

# generate SWE distribution
swe_dist_phase <-swe_from_phase(delta_phase = phase_dist,
                                inc_angle = .75,
                                perm = perm,
                                density = snow_density/1000)

hist(swe_dist_phase, breaks = 100)
