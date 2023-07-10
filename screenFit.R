### Title:    Routines to Screen a Fitted Lavaan Object
### Author:   Kyle M. Lang
### Created:  2018-MAR-16
### Modified: 2019-FEB-18

###--------------------------------------------------------------------------###

## Test a fitted lavaan object for Heywood cases:
testHw <- function(fit, what, alpha = 0.05) {
    s  <- inspect(fit, "coef")[[what]]
    hw <- diag(s) < 0.0
    
    if(any(hw)) {
        cn <- colnames(s)
        se <- sqrt(diag(vcov(fit)))[paste0(cn, "~~", cn)][hw]
        
        z   <- diag(s)[hw] / se
        sig <- pnorm(z) < alpha
    }
    else {
        sig <- FALSE
    }
    
    list(tech = any(hw), sig = any(sig))
}

###--------------------------------------------------------------------------###

## Test a fitted lavaan object for not positive definite parameter matrices:
checkNpd <- function(fit, what)
    any(eigen(inspect(fit, "coef")[[what]])$values <= 0.0)

###--------------------------------------------------------------------------###

## Check a fitted lavaan object for in-admissability:
checkObj <- function(fit, alpha = 0.05) {
    pars <- c("psi", "theta")
    out  <- lapply(pars,
                   function(x, fit, alpha = alpha)
                       list(hw = testHw(fit = fit, what = x, alpha = alpha),
                            pd = checkNpd(fit = fit, what = x)
                            ),
                   fit   = fit,
                   alpha = alpha)
    names(out) <- pars
    c(unlist(out), conv = !inspect(fit, "converged"))
}

###--------------------------------------------------------------------------###

## Check a list of fitted lavaan objects for admissability:
screenImpObj <- function(fitList, alpha = 0.05, thresh = 0.75, strict = FALSE) {
    outList <- list()
    for(m in 1 : length(fitList))
        outList[[m]] <- checkObj(fit = fitList[[m]], alpha = alpha)
    
    check <- do.call(rbind, outList)
    keys  <- ifelse(strict, "pd$|conv$", "sig$|conv$") 
    admit <- rowSums(check[ , grep(keys, colnames(check))]) == 0
    
    fail <- mean(admit) < thresh

    out <- list(checks = colSums(check), fail = fail)
    if(fail) out$fit = NA 
    else     out$fit = fitList[admit]
    
    out
}
