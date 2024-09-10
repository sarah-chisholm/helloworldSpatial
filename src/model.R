library(rsyncrosim)      # Load SyncroSim R package
library(terra)           # Load terra package
myScenario <- scenario()  # Get the SyncroSim scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load RunControl datasheet to be able to set timesteps
runSettings <- datasheet(myScenario, name = "helloworldSpatial_RunControl", returnInvisible = TRUE)

# Set timesteps - can set to different frequencies if desired
timesteps <- seq(runSettings$MinimumTimestep, runSettings$MaximumTimestep)

# Load scenario's input datasheet from SyncroSim library into R dataframe
myInputDataframe <- datasheet(myScenario,
                              name = "helloworldSpatial_InputDatasheet")

# Extract model inputs from complete input dataframe
mMean <- myInputDataframe$mMean
mSD <- myInputDataframe$mSD

# Load raster input 
rasterMap <- datasheetSpatRaster(myScenario,
                                 datasheet = "helloworldSpatial_InputDatasheet",
                                 column = "InterceptRasterFile")

# Setup empty R dataframe ready to accept output in SyncroSim datasheet format
myOutputDataframe <- data.frame(Iteration = numeric(0), 
                                Timestep = numeric(0), 
                                y = numeric(0),
                                OutputRasterFile = character(0))

# For loop through iterations
for (iter in runSettings$MinimumIteration:runSettings$MaximumIteration) {
  
  # Extract a slope value from normal distribution
  m <- rnorm(n = 1, mean = mMean, sd = mSD)
  
  # Use each cell in the raster as the intercept in linear equation
  rastList <- c()
  for (t in timesteps){
    tempRasterMap <- app(rasterMap, function(b) m * t + b)
    rastList <- c(rastList, tempRasterMap)
  }
  newRasterMaps <- terra::rast(rastList)
  
  # The y value will be the sum of all the cells in each raster
  y <- global(newRasterMaps, "sum")$sum
  
  # Add the new raster for this timestep/iteration to the output
  newRasterNames <- file.path(paste0(transferDir, 
                                     "/rasterMap_iter", iter, "_ts",
                                     timesteps, ".tif"))
  writeRaster(newRasterMaps, filename = newRasterNames, overwrite = TRUE)
  
  # Store the relevant outputs in a temporary dataframe
  tempDataframe <- data.frame(Iteration = iter,
                              Timestep = timesteps,
                              y = y,
                              OutputRasterFile = newRasterNames)
  
  # Copy output into this R dataframe
  myOutputDataframe <- addRow(myOutputDataframe, tempDataframe)
}

# Save this R dataframe back to the SyncroSim library's output datasheet
saveDatasheet(myScenario,
              data = myOutputDataframe,
              name = "helloworldSpatial_IntermediateDatasheet")
