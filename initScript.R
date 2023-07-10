### Title:    Initialize the Simluation's Dependencies
### Author:   Kyle M. Lang
### Created:  2018-JAN-26
### Modified: 2019-FEB-20

set.seed(235711)

library(mice)
library(mitools)
library(psych)
library(lavaan)
library(rlecuyer)
library(parallel)

source("screenFit.R")
source("subroutines.R")
source("createFacets.R") # Group items into facets
source("createBlocks.R") # Create A-, B-, and C-Block assignments

## Create the output directory, if necessary:
if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

## Define simulation parameters:
parms <- list()
parms$nVec       <- seq(500, 100, -100)
parms$comCase    <- useCc
parms$useEc      <- useEc
parms$nImps      <- nImps
parms$miceIter   <- nIter
parms$outDir     <- outDir
parms$verbose    <- FALSE
parms$impMeth    <- "pmm"
parms$blockList  <- readRDS(paste0(dataDir, "blockList.rds"))
parms$parcelList <- readRDS(paste0(dataDir, "facetList.rds"))
parms$thresh     <- 0.75
parms$alpha      <- 0.05
parms$mySeed     <- 235711
parms$maxStreams <- nReps
parms$strict     <- strict 

## Define the model:
parms$mod1 <- ifelse(parms$useEc,
                     "## Define latent variables:
                      care  =~ l11*careP1 + l21*careP2 + l31*careP3
                      task  =~ l12*taskP1 + l22*taskP2 + l32*taskP3
                      ego   =~ l13*egoP1  + l23*egoP2  + l33*egoP3
        
                      ## Estimate and label item intercepts:
                      careP1 ~ t1*1
                      careP2 ~ t2*1
                      careP3 ~ t3*1

                      taskP1 ~ t4*1
                      taskP2 ~ t5*1
                      taskP3 ~ t6*1

                      egoP1 ~ t7*1
                      egoP2 ~ t8*1
                      egoP3 ~ t9*1
        
                      ## Estimate latent means:
                      care ~ 1
                      task ~ 1
                      ego  ~ 1

                      ## Set scales via effects coding constraints:
                      l11 == 3 - l21 - l31
                      l12 == 3 - l22 - l32
                      l13 == 3 - l23 - l33
        
                      t1  == 0 - t2  - t3
                      t4  == 0 - t5  - t6
                      t7  == 0 - t8  - t9",
                     
                     "## Define latent variables:
                      care =~ careP1 + careP2 + careP3
                      task =~ taskP1 + taskP2 + taskP3
                      ego  =~ egoP1  + egoP2  + egoP3"
                     )

## Clean the raw data to produce population data for resampling:
cleanData(dataDir = dataDir, fileName = dataFileName)
