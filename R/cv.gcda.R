cv.gcda <-
function(X, y, gr = NULL, gr1 = NULL, gr2 = NULL,
    Alpha = seq(0,1,l=4), Alpha1 = seq(0,1,l=4), Alpha2 = seq(0,1,l=4), type = "linear") {
    if (type != "linear" & type != "quadratic")
        stop("Type as to be either \"linear\" or \"quadratic\" ")
    if (type == "linear") {
        obj <- cv.gcdalin(X, y, gr, Alpha)
    }
    else {
        obj <- cv.gcdaquad(X, y, gr1, gr2, Alpha1, Alpha2)
    }
    list(error = obj$er, alphaopt = obj$alphaopt, alphasopt = obj$alphasopt)
}

