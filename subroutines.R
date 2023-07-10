### Title:    Subroutines for Real Data PMD Study
### Author:   Kyle M. Lang
### Created:  2016-SEP-06
### Modified: 2019-FEB-20

###--------------------------------------------------------------------------###

## Wrapper for 'try' to streamline exception checking:
tryl <- function(x) {
    out <- try(x)
    err <- class(out) == "try-error"
    list(err = err, out = out)
}

###--------------------------------------------------------------------------###

## Randomly split a vector into "p" sub-vectors:
splitFun <- function(x,
                     p,
                     names   = letters[1 : p],
                     permute = TRUE,
                     strict  = TRUE)
{
    if(strict & p > length(x)) stop("You have requested 'p' > 'x'")
    if(length(names) != p)     stop("Length of 'names' does not match 'p'")

    if(permute) x <- sample(x)

    ## Special case: More partitions than data elements
    if(p >= length(x)) {
        outList                           <- as.list(rep(NA, p))
        names(outList)                    <- names
        outList[sample(names, length(x))] <- x

        return(outList)
    }

    n <- round(length(x) / p)

    m <- n * (p - 1)
    y <- x[1 : m]
    z <- setdiff(x, y)

    y <- matrix(y, nrow = n)

    outList <- list()
    for(j in 1 : (p - 1)) outList[[names[j]]] <- y[ , j]

    outList[[names[p]]] <- z

    outList
}

###--------------------------------------------------------------------------###

## Parcel raw items:
parcelData <- function(data, parcelList) {
    data.frame(
        lapply(parcelList,
               FUN  = function(x, data) rowMeans(data[ , x]),
               data = data)
    )
}

###--------------------------------------------------------------------------###

## Assign items to A-, B-, and C-Blocks, given a designated X-Block:
createAbcBlocks <- function(facets, xItems = NULL) {
    if(!is.null(xItems))
        facets <- lapply(facets, function(x, y) setdiff(x, y), y = xItems)

    ## ABC Random:
    tmp <- unlist(facets, use.names = FALSE)
    tmp <- splitFun(tmp, 3)

    random <- list(a = tmp$a, b = tmp$b, c = tmp$c)

    ## Facets in ABC:
    within <-
        list(a = unlist(facets[grep("P1", names(facets))], use.names = FALSE),
             b = unlist(facets[grep("P2", names(facets))], use.names = FALSE),
             c = unlist(facets[grep("P3", names(facets))], use.names = FALSE)
             )

    ## Facets between ABC:
    tmp <- na.omit(unlist(lapply(facets, splitFun, p = 3, strict = FALSE)))

    between <- list(a = as.character(tmp[grep("\\.a", names(tmp))]),
                    b = as.character(tmp[grep("\\.b", names(tmp))]),
                    c = as.character(tmp[grep("\\.c", names(tmp))])
                    )

    list(random = random, within = within, between = between)
}

###--------------------------------------------------------------------------###

## Clean the population data:
cleanData <- function(dataDir, fileName) {
    ## Read data and drop unnecessary columns:
    dat1    <- read.csv(paste0(dataDir, fileName))
    rawData <- dat1[ , -grep("as\\d|Respondent|enjoy|own", colnames(dat1))]

    ## Remove tiny race category (i.e., race = 3):
    flag <- with(rawData, !is.na(race) & race != 3)
    rawData <- rawData[flag, ]

    ## Cast nominal variables as factors:
    rawData$gender <- as.factor(rawData$gender)
    rawData$race   <- as.factor(rawData$race)

    ## Write the cleaned population data to disk:
    dataList <- list(full = rawData, cc = na.omit(rawData))
    saveRDS(dataList, paste0(dataDir, "populationData.rds"))
}

###--------------------------------------------------------------------------###

## Assign planned missing:
imposePmd <- function(data, blockList) {
    ## Calculate number of rows for each form:
    n <- round(nrow(data) / 3)

    ## Assign missing according to form:
    data[1 : n,                    blockList$a] <- NA
    data[(n + 1) : (2 * n),        blockList$b] <- NA
    data[(2 * n + 1) : nrow(data), blockList$c] <- NA

    ## Return incomplete data:
    data
}

###--------------------------------------------------------------------------###

runMice <- function(missData, parms) {
    methVec <- rep(parms$impMeth, ncol(missData))

    methVec[colnames(missData) == "race"]   <- "polyreg"
    methVec[colnames(missData) == "gender"] <- "logreg"

    tryl(
        mice(data      = missData,
             m         = parms$nImps,
             maxit     = parms$miceIter,
             method    = methVec,
             printFlag = parms$verbose)
    )
}

###--------------------------------------------------------------------------###

## Multiply impute missing values:
imputeData <- function(missData, parms) {
    miceOut <- runMice(missData, parms)

    if(!miceOut$err) {
        impList <- list()
        for(m in 1 : miceOut$out$m) impList[[m]] <- complete(miceOut$out, m)

        out <- list(fail = FALSE,
                    imps = lapply(impList,
                                  FUN = parcelData,
                                  parcelList = parms$parcelList)
                    )
    } else {
        out <- list(fail = TRUE, imps = miceOut$out)
    }
    out
}

###--------------------------------------------------------------------------###

## Fit analysis model to imputed data:
fitModel <- function(dataObj, parms) {
    if(class(dataObj) == "list") {
        fit <- tryl(
            lapply(dataObj,
                   FUN = function(x, mod)
                       lavaan(model          = mod,
                              data           = x,
                              auto.fix.first = FALSE,
                              std.lv         = !parms$useEc,
                              auto.var       = TRUE,
                              auto.cov.lv.x  = TRUE,
                              meanstructure  = TRUE,
                              int.ov.free    = TRUE),
                   mod = parms$mod1)
        )

        ## Return early if lavaan crashes:
        if(fit$err) return(list(checks = NA, fail = 2, fit = fit))

        ## Check convergence/admissibility of solution, and remove failed reps:
        res <- screenImpObj(fitList = fit$out,
                            thresh  = parms$thresh,
                            alpha   = parms$alpha,
                            strict  = parms$strict)

        if(res$fail)
            res$rel <- NA
        else {
            res$rel <- calcImpRel(res$fit)
            res$fit <- MIcombine(res$fit)
        }
    }
    else {
        fit <- tryl(
            lavaan(model          = parms$mod1,
                   data           = dataObj,
                   auto.fix.first = FALSE,
                   std.lv         = !parms$useEc,
                   auto.var       = TRUE,
                   auto.cov.lv.x  = TRUE,
                   meanstructure  = TRUE,
                   int.ov.free    = TRUE)
        )

        ## Return early if lavaan crashes:
        if(fit$err) return(list(checks = NA, fail = 2, fit = fit))

        ## Check convergence/admissibility of solution:
        check <- checkObj(fit = fit$out, alpha = parms$alpha)
        keys  <- ifelse(parms$strict, "pd$|conv$", "sig$|conv$")
        res   <- list(checks = sapply(check, as.numeric),
                      fail   = any(check[grep(keys, names(check))])
                      )

        if(res$fail)
            res$fit <- res$rel <- NA
        else {
            res$rel <- calcRel(fit$out)
            res$fit <- fit$out
        }
    }

    res$fail <- as.numeric(res$fail)

    res
}

###--------------------------------------------------------------------------###

## Do all the computation for a single design cell:
runCond <- function(data, parms, blocks = NULL) {
    ## Impose missing data:
    if(!is.null(blocks)) data <- imposePmd(data = data, blockList = blocks)

    ## Impute missing values (if any):
    if(any(is.na(data))) {
        tmp  <- imputeData(missData = data, parms = parms)
        data <- tmp$imps
        flag <- tmp$fail
    }
    else
        data <- parcelData(data = data, parcelList = parms$parcelList)

    ## If we have data, fit the analysis model and return the fitted object:
    check <- is.data.frame(data) || !flag
    if(check)
        fitModel(dataObj = data, parms = parms)
    else
        list(checks = NA, fail = 3, fit = NA, rel = NA)
}

###--------------------------------------------------------------------------###
## After running 'runCond' the 'fail' field is coded as follows:
## 0 = Admissible solution :)
## 1 = Inadmissable solution (i.e., non-convergence, Heywood, non-PD Matrix)
## 2 = lavaan crashed
## 3 = mice crashed
###--------------------------------------------------------------------------###

## Run a single Monte Carlo replication:
doRep <- function(rp, data, parms) {
    ## Setup the PRNG:
    .lec.SetPackageSeed(rep(parms$mySeed, 6))
    if(!rp %in% .lec.GetStreams())
        .lec.CreateStream(c(1 : parms$maxStreams))
    .lec.CurrentStream(rp)

    ## Extract the appropriate block assignments:
    blocks <- parms$blockList[[rp]]

    ## Resample the data:
    data <- data[sample(1 : nrow(data), parms$nVec[1], replace = TRUE), ]

    for(n in parms$nVec) {
        ## Subsample the data:
        nDrop <- nrow(data) - n
        if(nDrop > 0) data <- data[-sample(1 : nrow(data), nDrop), ]

        fitList <- list()

        ## Run the raw data 'control' condition:
        fitList$rawFit <- runCond(data = data, parms = parms)

        ## Run the missing data 'experimental' conditions
        ## NOTE: The outer 'lapply' loops over X-block conditions, and the inner
        ##       'lapply' loops over ABC-block conditions.
        fitList$missFit <-
            lapply(blocks,
                   function(y, d1, p1)
                       lapply(y,
                              function(x, d0, p0)
                                  runCond(data = d0, parms = p0, blocks = x),
                              d0 = d1,
                              p0 = p1),
                   d1 = data,
                   p1 = parms)

        ## Write the fitted model object to disk:
        saveRDS(fitList,
                file =
                    paste0(parms$outDir,
                           "fitList_n", n,
                           "_", ifelse(parms$comCase, "comCase", "allCase"),
                           "_", ifelse(parms$useEc, "ec", "ff"),
                           "_", ifelse(parms$strict, "strictHw", "signifHw"),
                           "_rep", rp,
                           ".rds")
                )
    }
    rp # Return rep index
}

###--------------------------------------------------------------------------###

## Keep un-expected, fatal errors from crashing entire run:
safetyWrap <- function(rp, data, parms)
    try(doRep(rp = rp, data = data, parms = parms))

###--------------------------------------------------------------------------###

## Broadcast the library function of a list of packages:
applyLib <- function(pkgList)
    lapply(pkgList, library, character.only = TRUE, logical = TRUE)


###--------------------------------------------------------------------------###

getReps <- function(x)
    sort(
        as.numeric(
            unlist(
                strsplit(
                    substr(x, start = 28, stop = 100L),
                    ".rds")
            )
        )
    )

###--------------------------------------------------------------------------###

## Compute latent reliability from a fitted lavaan object:
calcRel <- function(obj) {
    tmp <- inspect(obj, "coef")

    la <- tmp[["lambda"]]
    th <- diag(tmp[["theta"]])
    ps <- diag(tmp[["psi"]])

    out <- c()
    for(v in colnames(la)) {
        tmp <- la[ , v]
        tmp <- tmp[tmp > 0]

        out[v] <-
            sum(tmp)^2 * ps[v] / (sum(tmp)^2 * ps[v] + sum(th[names(tmp)]))
    }

    out
}

###--------------------------------------------------------------------------###

## Pool squared correlations using Fisher's R to Z transformation:
poolR2 <- function(x) {
    if(is.list(x) & !is.data.frame(x)) x <- do.call(rbind, x)
    fisherz2r(colMeans(fisherz(sqrt(x))))^2
}

###--------------------------------------------------------------------------###

## Compute (pooled) latent reliability for MI models:
calcImpRel <- function(x) poolR2(lapply(x, calcRel))
