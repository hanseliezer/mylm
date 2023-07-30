#' Fit a linear model
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class) a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’.
#'
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the
#' variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment
#' from which lm is called.
#'
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#'
#' @export
mylm <- function(formula, data=NULL, subset=NULL) {

  # check if first argument is of class 'formula'; seems that any non-quoted string is a
  # formula as long as it has a ~ in it, so can only test if it's of different type like
  # numeric
  if (!rlang::is_formula(formula)) {
    stop("Invalid formula entered.")
  }
  
  # if no data is given, all variables in formula must be found in environment
  if (is.null(data)) {
    if (!all(all.vars(formula) %in% ls(envir=.GlobalEnv))) {
      stop("Dataset not given, and variables stated in formula not found in environment.")
    }
  # only evaluate when data is given
  } else {
    # only evaluate if subset is not null
    if (!is.null(subset)) {
      # only accept subset if they supply row numbers or a logical vector
      if (!class(subset) %in% c('numeric', 'integer', 'logical')) {
        stop("Subset invalid: must be numeric or logical.")
      }

      # subset of a data should have smaller size than the data; supplying row numbers bigger than height of dataset
      # will not throw error, but return empty rows, so preemptively stop if length of data subset is longer
      if (nrow(data[subset, ]) > nrow(data)) {
        stop("Invalid subset: results in larger dataset.")
      } else {
        data <- data[subset, ]
      }
    }
  }

  # if no data is given, rely entirely on formula
  if (is.null(data)) {
    # eval(as.name()) tells R to find an object with given name
    yname <- eval(as.name(formula[[2]]))
    yvec <- yname
    # model.matrix can accept formula and automatically look them up in environment
    xmat <- model.matrix(formula)
  } else {
    yname <- as.character(formula[[2]])
    yvec <- data[, yname]
    xmat <- model.matrix(formula, data=data)
  }
  df.residual <- nrow(xmat) - ncol(xmat)
  
  # inverse of (X^T X)
  xxinv <- solve(t(xmat) %*% xmat)
  # find beta estimates: (X^T X)^-1 X^T y
  coef <- as.vector(xxinv %*% t(xmat) %*% yvec)
  # match the betas with the column names
  names(coef) <- colnames(xmat)
  # calculate y based on beta estimates
  yfit <- as.vector(xmat %*% coef)
  # give names to each element in fitted values with row numbers of the dataset, in style of lm()
  names(yfit) <- rownames(data)
  # residuals are simply difference between fitted y and true y
  residuals <- yvec - yfit
  # same as fitted values
  names(residuals) <- rownames(data)
  # residual standard error
  sigma <- sqrt(sum(residuals^2) / df.residual)
  # variance-covariance matrix
  vcov <- sigma^2 * xxinv

  mylmobject <- list(call=match.call(),
                     formula=formula,
                     data=data,
                     yname=yname,
                     coef=coef,
                     sigma=sigma,
                     vcov=vcov,
                     npar=ncol(xmat),
                     df.residual=df.residual,
                     residuals=residuals,
                     fitted.values=yfit)

  class(mylmobject) <- "mylm"
  return(mylmobject)
}

#' Codes for marking p-values
#'
#' @param pvals Vector of p-values
#'
#' @export
signif_codes <- function(pvals) {
  s <- ifelse(pvals>0.1, " ",
              ifelse(pvals>0.05, ".",
                     ifelse(pvals>0.01, "*",
                            ifelse(pvals>0.001, "**","***"))))
  return(s)
}


#' Summary method for mylm
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
summary.mylm <- function(object,...) {
  mylmobj_summary <- object
  class(mylmobj_summary) <- "summary.mylm"
  return(mylmobj_summary)
}

#' Print method for mylm
#'
#' @param x object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
print.mylm <- function(x,...) {
  with(x, {
    cat("Linear model mylm object\n")
    cat("\nCall:\n")
    print(call)
    cat("\nCoefficients:\n")
    print(coef)
    cat("\n")
  })
  invisible()
}

#' Print method for summary(mylm)
#'
#' @param x object of class "summary.mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
print.summary.mylm <- function(x,...) {
  with(x, {
    cat("Linear model mylm object\n")
    cat("\nCall:\n")
    print(call)
    se <- sqrt(diag(vcov))
    n <- nrow(data)
    coeftab <- data.frame(Estimate=coef,
                          "Std. Error"=se,
                          "t value"=coef/se,
                          pvals=2*pt(abs(coef/se), df.residual, lower.tail=FALSE))
    coeftab$s <- signif_codes(coeftab$pvals)
    names(coeftab) <- c("Estimate","Std. Error", "t value", "Pr(>|t|)"," ")
    rss <- sum(residuals^2)
    tss <- sum((data[,yname]-mean(data[,yname]))^2)
    r.squared <- 1 - rss/tss
    adj.r.squared <- 1 - (1-r.squared)*(n-1)/(df.residual)
    fstat <- ((tss-rss)/(npar-1))/(rss/df.residual)
    pvalue <- pf(fstat, npar-1, df.residual, lower.tail=FALSE)
    cat("\nCoefficients:\n")
    print(coeftab)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    cat("\n")
    cat("Residual standard error: ", sigma," on ", df.residual, " degrees of freedom\n")
    cat("Multiple R-squared: "); cat(r.squared); cat(" ");
    cat("Adjusted R-squared: "); cat(adj.r.squared); cat("\n") #	Adjusted R-squared:  0.2261
    cat("F-statistic: "); cat(fstat); cat(" on "); cat(npar-1); cat(" and "); cat(df.residual);
    cat(" DF, p-value "); cat(pvalue); cat("\n") #, 25.25 on 1 and 82 DF,  p-value: 2.906e-06

    cat("\nVariance Covariance matrix:\n")
    print(vcov)
    cat("\n")
  })
  invisible()
}

#' Variance covariance matricx for parameters
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
vcov.mylm <- function(object,...) {
  with(object, {
    return(vcov)
  })
}

#' Confidence intervals for parameters
#'
#' @param object object of class "mylm"
#'
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#'
#' @param level The confidence level required (default = 0.95).
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
confint.mylm <- function(object, parm=NULL, level=0.95,...) {
  with(object, {
    se <- sqrt(diag(vcov))
    tval <- qt(1-(1-level)/2, df=df.residual, lower.tail=TRUE)
    retval <- cbind(coef-tval*se, coef+tval*se)
    dimnames(retval)[[2]] <- sprintf("%f %%", c(0,100)+c(1,-1)*100*(1-level)/2)
    if(!is.null(parm)) retval <- retval[parm,]
    return(retval)
  })
}


#' Fitted values
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
fitted.mylm <- function(object,...) {
  with(object, {
    return(fitted.values)
  })
}

#' Residuals
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
residuals.mylm <- function(object,...) {
  with(object, {
    return(residuals)
  })
}

#' Predicted values
#'
#' @param object object of class "mylm"
#'
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
#'
#' @param se.fit A switch indicating if standard errors are required (default = FALSE).
#'
#' @param interval Type of interval calculation, can be "none", "confidence" or "prediction". Can be abbreviated.
#'
#' @param level The confidence level required (default = 0.95).
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
predict.mylm <- function(object, newdata=NULL, se.fit=FALSE,
                         interval=c("none", "confidence", "prediction"),
                         level=0.95,...) {
  with(object, {
    if(is.null(newdata)) newdata <- object$data
    xmat.new <- model.matrix(object$formula, data=newdata)
    fitval <- as.vector(xmat.new%*%object$coef)
    retval <- list(fit=fitval)
    if(se.fit || interval!="none") {
       se.val <- sqrt(diag(xmat.new%*%vcov%*%t(xmat.new)))
       if(se.fit) retval$se.fit <- se.val
    }
    zval <- qnorm(1-(1-level)/2)
    if(interval[1]=="confidence") {
      retval$fit <- cbind(fitval, fitval-zval*se.val, fitval+zval*se.val)
      dimnames(retval$fit)[[2]] <- c("fit","lwr","upr")
    } else if(interval[1]=="prediction") {
      pred.err <- sqrt(sigma^2 + se.val^2)
      retval$fit <- cbind(fitval, fitval-zval*pred.err, fitval+zval*pred.err)
      dimnames(retval$fit)[[2]] <- c("fit","lwr","upr")
    }
    retval$df <- df.residual
    retval$residual.scale <- sigma
    return(retval)
  })
}

#' Plotting
#'
#' @param x object of class "mylm"
#'
#' @param ... additional arguments to be passed to methods, such as graphical parameters such as \code{par}.
#'
#' @export
plot.mylm <- function(x, ...) {
  with(x, {
    plot(data[,yname], fitted.values,
         xlab="Observed", ylab="Fitted",
         ...)
    abline(a=0, b=1, ...)
  })
  invisible()
}

