### Title:    Group Items into Facets
### Author:   Kyle M. Lang
### Created:  2016-SEP-06
### Modified: 2018-JAN-26


## Construct list of facet membership:
numList1 <- list(
    c1 = c(2, 5, 9, 13, 12),
    c2 = c(1, 6, 8, 11),
    c3 = c(3, 4, 7, 10),
    
    t1 = c(2, 5, 6, 11, 15, 16),
    t2 = c(3, 10, 12, 13),
    t3 = c(1, 7, 9, 14),
    
    e1 = c(2, 6, 8, 11, 13),
    e2 = c(4, 7, 10, 12, 15),
    e3 = c(1, 5, 14)
)

facetList <- list()
for(n in 1 : length(numList1)) {
    if(length(grep("c", names(numList1[n]))) > 0)
        facetList[[n]] <- paste0("care", numList1[[n]])
    else if(length(grep("t", names(numList1[n]))) > 0)
        facetList[[n]] <- paste0("task", numList1[[n]])
    else
        facetList[[n]] <- paste0("ego", numList1[[n]])
}

names(facetList) <- c("careP1", "careP2", "careP3",
                      "taskP1", "taskP2", "taskP3",
                      "egoP1", "egoP2", "egoP3")

saveRDS(facetList, paste0(dataDir, "facetList.rds"))

## Informed X-block assignment:
numList2 <- list(
    c = c(2, 3, 9, 13),
    t = c(6, 11, 12, 15, 16),
    e = c(4, 8, 10, 12, 14)
)

xList <- list()
for(n in 1 : length(numList2)) {
    if(length(grep("c", names(numList2[n]))) > 0)
        xList[[n]] <- paste0("care", numList2[[n]])
    else if(length(grep("t", names(numList2[n]))) > 0)
        xList[[n]] <- paste0("task", numList2[[n]])
    else
        xList[[n]] <- paste0("ego", numList2[[n]])
}

names(xList) <- c("care", "task", "ego")
