### Title:    Code to Run the Real Data PMD Study (on the Lisa Cluster)
### Author:   Kyle M. Lang
### Created:  2016-SEP-06
### Modified: 2019-FEB-18

args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

## Parameterize cluster job:
startRep    <- as.numeric(args[1])
stopRep     <- as.numeric(args[2])
clusterSize <- as.numeric(args[3])

## Parameterize the imputation:
nImps <- as.numeric(args[4])
nIter <- as.numeric(args[5])

## Parameterize read/write paths:
dataDir      <- args[6]
outDir       <- args[7]
dataFileName <- args[8] # Original data file

nReps <- 1000 # Most reps we could want

source("initScript.R")

data <- readRDS(paste0(dataDir, "populationData.rds"))
data <- data[[ifelse(parms$compCase, "cc", "full")]]

## Run in parallel:
cl <- makeCluster(clusterSize, type = "MPI")

clusterCall(cl = cl, fun = source, file = "helperFunctions.R")
clusterCall(cl      = cl,
            fun     = applyLib,
            pkgList = c("rlecuyer", "mice", "mitools", "lavaan")
            )

parLapply(cl    = cl,
          X     = c(startRep : stopRep),
          fun   = doRep,
          data  = data,
          parms = parms)

stopCluster(cl)
