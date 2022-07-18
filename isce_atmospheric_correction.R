####### atmospheric correction #######
# for the feb 12th - feb 19th jemez data
# updated jan 28th
# updated june 30th for isce data


library(terra)
library(ggplot2)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")
list.files() #pwd

# path length raster 
plv_km <-rast("plv_km.tif")
plv_km
plot(plv_km)

# bring in coherence raster for masking
cor <-rast("cor_feb19-26.tif")
cor[cor == 0] <-NA

# create masking raster with smallest possible extent
cor_mask <-cor
plv_km_crop <-resample(plv_km, cor_mask)
cor_mask_v2 <-mask(cor_mask, plv_km_crop, maskvalue=NA)
cor_mask_v2[cor_mask_v2==0] <- -NA
cor_mask_v2[cor_mask_v2>0] <-999
plot(cor_mask_v2)
#writeRaster(cor_mask_v2, "cor_mask.tif")

##############
### bring in all the insar data 
##############

# unw
unw_raw <-rast("/Users/jacktarricone/Desktop/jemez_geoloc_rough/unw_gecoded_feb12-26_v3.tif")
values(unw_raw)[(unw_raw)==0]<-NA
unw_raw
plot(unw_raw, add = TRUE)


#########################################
## resample and crop to one size ########
#########################################

# resample look vector to unwrapped phase
plv_resamp <-resample(plv_km, unw_raw, method = "bilinear")
ext(plv_resamp) <-ext(unw_raw) # set extent as same as unw
plv_resamp

# test plot
plot(plv_km)
plot(unw_raw, add = TRUE)

######
# create rast of approx distance in range for plotting
# while this works bc it's almost north south, this method is needed for
# rasters that are other orientations 

# convert number rows to distance in range direciton
ncols <-ncol(unw_raw) 
nrows <-nrow(unw_raw)
range_distance <- global(fun = max, na.rm = T, plv_km) - global(fun = min, na.rm = T, plv_km)
range_distance <-range_distance[1,1] # create int
range_distance

# calc pixel size in meters
appx_km_per_pixel <-range_distance/ncols
appx_km_per_pixel

# create vector of range distance starting at 0
range_vector <-rev(seq(0,range_distance,appx_km_per_pixel))
range_vector_km <-range_vector[-c(4174)] # delete one extra row

# create matrix that is same size as all other rasters
range_mat <-matrix(range_vector_km, nrow=7795, 
                   ncol=length(range_vector_km), byrow=TRUE) # make matrix, byrow TRUE

# and transform to raster
range_rast <-rast(range_mat)
crs(range_rast) <-crs(unw_raw)
ext(range_rast) <-ext(unw_raw)
plot(range_rast)
range_rast

#### crop down to largest size possible with all overlapping pixels
# create new rast, set non NA values to 0 for unw
unw_non_na <-unw_raw
values(unw_non_na)[!is.na(unw_non_na[])] = 1
plot(unw_non_na)

# same thing for plv
plv_resamp_non_na <-plv_resamp

# crop plv with unw, this leaves only the cells that exist in both data sets for plotting
#plv_crop1 <-terra::mask(plv_resamp_non_na, unw_non_na, maskvalues=NA)
plv_unw_mask <-terra::mask(plv_resamp_non_na, unw_non_na, maskvalues=NA)
plot(plv_unw_mask)

# test plot, looks good
plot(plv_resamp)
plot(unw_raw, add = TRUE)
plot(plv_unw_mask, add = TRUE)

# check data, good
unw_raw
plv_unw_mask


# mask both unw and plv with the mask
# unw_masked <-terra::mask(unw_raw, plv_unw_mask, maskvalues=NA)
# plot(unw_masked)
# 
# plv_masked <-terra::mask(plv_resamp, plv_unw_mask, maskvalues=NA)
# plot(plv_masked, add = TRUE)


########################################
## bring in the no snow mask ###########
########################################

# using the snow mask, only analyze pixels that have no snow to check for atmospheric delay
# we do this because we're assuming there is some snow signal combine with atm signal in no pixels
# by doing just these, in theory we're just focusing on the atmospheric protion

#################### snow mask

snow_mask_raw <-rast("landsat_fsca_2-18.tif")
plot(snow_mask_raw)

# clip edges off no snow mask to make it same size as plv and unw
snow_mask <-mask(snow_mask_raw, unw_raw, maskvalue = NA)
plot(snow_mask)

#### snow unw and plv

# snow unw
snow_unw <-mask(unw_raw, snow_mask, maskvalue = NA)
plot(snow_unw)

# snow plv
snow_plv <-mask(plv_unw_mask, snow_unw, maskvalue = NA)
plot(snow_plv)


### convert no snow plv and unw rasters to dataframes, rename data columns

# unw
unw_df <-as.data.frame(snow_unw, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(unw_df)[4] <- "unwrapped_phase"
head(unw_df)
hist(unw_df$unwrapped_phase, breaks = 100) #quick hist to check

#plv
plv_df <-as.data.frame(snow_plv, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(plv_df)[4] <- "plv_km"
head(plv_df)
hist(plv_df$plv_km, breaks = 100) #quick hist to check

# range distance
####### not actually using this
range_df <-as.data.frame(range_rast, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(range_df)[4] <- "range_distance_km"
head(range_df)
hist(range_df$range_distance_km, breaks = 100) #quick hist to check

#bind last column on for future plot
plotting_df<-cbind(unw_df, plv_df$plv_km)
head(plotting_df)
colnames(plotting_df)[5] <- "plv_km"
#colnames(snow_unw_plv_df)[6] <- "range_distance_km"
head(plotting_df)

# save the data frame for making more plots in the future
#data.table::fwrite(snow_unw_plv_df, "/Users/jacktarricone/ch1_jemez_data/feb12-26_no_snow_unw_plv_df.csv")

# plot unw vs plv
# call stat smooth function for trend line
stat_smooth_func2 <- function(mapping = NULL, data = NULL,
                              geom = "smooth", position = "identity",
                              ...,
                              method = "auto",
                              formula = y ~ x,
                              se = TRUE,
                              n = 80,
                              span = 0.75,
                              fullrange = FALSE,
                              level = 0.95,
                              method.args = list(),
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              xpos = NULL,
                              ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq1 <- substitute(italic(y) == a + b %.% italic(x), 
                                              list(a = format(coef(m)[[1]], digits = 3), 
                                                   b = format(coef(m)[[2]], digits = 3),
                                                   r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq1))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)


#######################################
# run linear model to plot trend line #
#######################################
lm_fit <-lm(plotting_df$unwrapped_phase ~ plotting_df$plv_km)
summary(lm_fit)


########################################
########### unw vs plv #################
########################################

theme_set(theme_light(base_size =12))

p9 <-ggplot(plotting_df, aes(plv_km, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "firebrick") +
  stat_smooth_func2(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(slope = coef(lm_fit)[[2]], intercept = coef(lm_fit)[[1]], size = 1)+
  #scale_y_continuous(breaks = seq(-5,6,2))+
  labs(#title = "Unwrapped Phase vs. Radar Look Vector Length 2/12-2/26 Pair",
       x = "Radar Look Vector Length (km)",
       y = "Unwrapped Phase (radians)")+
  scale_x_reverse()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

print(p9)

setwd("/Users/jacktarricone/ch1_jemez_data/plots/")
# ggsave(p9,
          # file = "feb12-26_nosnow_vs_plv_no_title.png",
          # width = 5, 
          # height = 5,
          # dpi = 400)


### correct unw data using path length and the linear estimation we generated

path_length_correction <-function(unw, plv){
   atm_corrected <-unw - ((plv * coef(lm_fit)[[2]]) + coef(lm_fit)[[1]])
   return(atm_corrected)
}

# save original
unw_corrected <-path_length_correction(unw_raw, plv_unw_mask)
plot(unw_corrected)
writeRaster(unw_corrected, "unw_corrected_feb12-26.tif")

# snow mask
snow_unw_corrected <-mask(unw_corrected, snow_mask)
plot(snow_unw_corrected)
writeRaster(snow_unw_corrected, "snow_unw_corrected_feb12-26.tif")


###################
# test plot with corrected data
###################

unw_corrected_df <-as.data.frame(unw_corrected, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(unw_corrected_df)[4] <- "unwrapped_phase"
head(unw_corrected_df)

p13 <-ggplot(unw_corrected_df, aes(x, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "grey90", high = "red") +
  #stat_smooth_func2(geom="text",method="lm",hjust=0,parse=TRUE) +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(slope = coef(lm_fit)[[2]], intercept = coef(lm_fit)[[1]], size = 1)+
  #scale_y_continuous(breaks = seq(-5,15,5))+
  #scale_x_continuous(breaks = seq(10,30,5))+
  labs(title = "Jemez Unwrapped Phase Corrected",
       x = "Longitude (degrees)",
       y = "Unwrapped Phase (radians)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
print(p13)


ggsave(p13,
       file = "jemez_phase_corrected_12-26.png",
       width = 6, 
       height = 4,
       dpi = 400)
