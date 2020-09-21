########################################
## Helper function to be parallelized ##
## and calculate district-level C     ##
########################################

calcDistrictCarbon <- function(polys, rast, rast_rmse, i) {
  
  shp <- polys[i, ]
  rast_crop <- crop(rast, shp)
  rast_buff <- crop(rast, st_buffer(shp, 0.015))
  rast_mean <- mean(getValues(rast_crop), na.rm = T)
    
  if(is.finite(rast_mean)) {
    
    #---------
    # Simulate raster data using a spatial trend model and stationary isotropic covariance model
    
    model <- RMtrend(mean = rast_mean) +    # Trend model using mean of district
      RMstable(alpha = 1, var = rast_rmse^2)   # Stationary isotropic covariance model that uses raster model variance
    
    sims <- stack()
    
    #---------
    # Run through different algorithms to simulate data based on district size.
    
    if(ncell(rast_buff) > 5000000) {
      
      for(m in 1:40) {
        
        splt <- splitRaster(rast_buff, 2, 2)
        splt_mean1 <- mean(getValues(splt[[1]]), na.rm = T)
        
        if(is.finite(splt_mean1)) {
          
          model <- RMtrend(mean = splt_mean1) + RMstable(alpha = 1, var = rast_rmse^2)
          sim <- geostatsp::RFsimulate(model, x = splt[[1]], n = 1)
          
        } else {
          
          sim <- splt[[1]]
          
        }
        
        mrg <- sim
        
        for(j in 2:4) {
          
          splt_mean <- mean(getValues(splt[[j]]), na.rm = T)
          
          if(is.finite(splt_mean)) {
            
            model <- RMtrend(mean = splt_mean) + RMstable(alpha = 1, var = rast_rmse^2)
            sim <- geostatsp::RFsimulate(model, x = splt[[j]], n = 1)
            sim_msk <- mask(sim, splt[[j]])
            mrg <- merge(mrg, sim_msk)
            
          } else {
            
            sim <- splt[[j]]
            mrg <- merge(mrg, sim)
            
          }
          
        }
        
        sims <- stack(sims, mrg)
        
      }
      
    } else if(ncell(rast_buff) > 1500000) {
      
      for(j in 1:20) {
        
        sim <- geostatsp::RFsimulate(model, x = rast_buff, n = 2)
        sims <- stack(sims, sim)
        
      }
      
    } else {
      
      for(j in 1:8) {
        
        sim <- geostatsp::RFsimulate(model, x = rast_buff, n = 5)
        sims <- stack(sims, sim)
        
      }
      
    }
    
    sims <- crop(sims, extent(rast_crop))
    
    #---------
    # Calculate means and store in data frame.
    
    means <- c()
    
    for(k in 1:40) {
      
      means <- c(means, mean(getValues(mask(sims[[k]], rast_crop)), na.rm = T))
      
    }
    
    print(paste0(shp$ADM2_EN, " - ", i))
    vals <- c(shp$ADM2_EN, means)
    
  } else {
    
    print(paste0(shp$ADM2_EN, " - ", i))
    vals <- c(shp$ADM2_EN, rep(NA, 40))
    
  }
  
  rm(rast_crop, rast_buff, rast_mean, sims)
  gc()
  
  return(vals)
  
}
