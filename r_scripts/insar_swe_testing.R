look_angle <-(28.01+68.9)/2
density <- .33
di_elc <- 1.7
wL <- 23.8403545

constant <-cos(look_angle) - sqrt(di_elc - sin((look_angle)^2))

phase_change = ((4*pi)/wL)*(-constant)(deltaSWE)
deltaSWE = phase_change*(wL/(4*pi))*(-constant)
final_constant <-(wL/(4*pi))*(-constant)
                                 