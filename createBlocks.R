### Title:    Created Planned Missing Blocks
### Author:   Kyle M. Lang
### Created:  2016-SEP-06
### Modified: 2019-FEB-20

xInform <- unlist(xList, use.names = FALSE)
xN      <- length(xInform)

blockList <- list()
for(rp in 1 : nReps) {
    ## Trivial X-Block (only demographics):
    trivX <- createAbcBlocks(facets = facetList, xItems = NULL)
    
    ## Informative X-Block:
    infoX <- createAbcBlocks(facets = facetList, xItems = xInform)
    
    ## Random X-Block:
    randX <- createAbcBlocks(facets = facetList,
                             xItems = sample(
                                 unlist(facetList, use.names = FALSE),
                                 xN)
                             )
    
    ## Aggregate assignment conditions:
    blockList[[rp]] <- list(trivX = trivX, infoX = infoX, randX = randX)
}

## Save the block assignments:
saveRDS(blockList, file = paste0(dataDir, "blockList.rds"))
