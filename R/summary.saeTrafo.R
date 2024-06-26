#' Summarizes an saeTrafo object
#'
#' Additional information about the data and model in small area estimation
#' methods and components of an saeTrafo object are extracted. The returned
#' object is suitable for printing  with the \code{print} function.
#' @param object an object of type "NER", representing point and MSE estimates.
#' @param ... additional arguments that are not used in this method.
#' @return an object of type "summary.NER" with information about the sample and
#' population data, the usage of transformation, normality tests and information
#' of the model fit.
#' @seealso \code{\link{saeTrafoObject}}, \code{\link{NER_Trafo}},
#' \code{\link[moments]{skewness}}, \code{\link[moments]{kurtosis}},
#' \code{\link[stats]{shapiro.test}}
#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom stats shapiro.test residuals var
#' @importFrom nlme VarCorr
#' @name summaries.saeTrafo

summary.NER <- function(object, ...) {

  throw_class_error(object, "NER")

  call_emdi <- object$call

  N_dom_unobs <- object$framework$N_dom_unobs
  N_dom_smp <-   object$framework$N_dom_smp

  smp_size <- object$framework$N_smp
  pop_size <- object$framework$N_pop

  smp_size_dom <-
    summary(as.data.frame(table(object$framework$smp_domains_vec))[, "Freq"])

  if (is.null(object$framework$pop_area_size)) {
    pop_size_dom <-
      summary(as.data.frame(table(object$framework$pop_domains_vec))[, "Freq"])
  } else {
    pop_size_dom <- summary(object$framework$pop_area_size)
  }
  sizedom_smp_pop <- rbind(Sample_domains = smp_size_dom,
                           Population_domains = pop_size_dom)
  if (object$transformation == "log.shift") {
    transform_method <- data.frame(
      Transformation  = object$transformation,
      Method          = object$method,
      Optimal_lambda  = object$transform_param$optimal_lambda,
      row.names       = ""
    )
  } else if (object$transformation == "log") {
    transform_method <- data.frame(
      Transformation  = object$transformation,
      Shift_parameter = round(object$transform_param$shift_par, 3),
      row.names       = ""
    )
  } else if (object$transformation == "no") {
    transform_method <- NULL
  }

  skewness_res <- skewness(residuals(object$model, level = 0, type = "pearson"))
  kurtosis_res <- kurtosis(residuals(object$model, level = 0, type = "pearson"))

  skewness_ran <- skewness(ranef(object$model)$'(Intercept)')
  kurtosis_ran <- kurtosis(ranef(object$model)$'(Intercept)')

  if (length(residuals(object$model, level = 0, type = "pearson")) > 3 &
      length(residuals(object$model, level = 0, type = "pearson")) < 5000) {
    shapiro_p_res <-
      shapiro.test(residuals(object$model, level = 0, type = "pearson"))[[2]]
    shapiro_W_res <-
      shapiro.test(residuals(object$model, level = 0, type = "pearson"))[[1]]
  } else {
    warning(paste("Number of observations exceeds 5000 or is lower then 3 and",
                  "thus the Shapiro-Wilk test is not applicable for the",
                  "residuals."))
    shapiro_p_res <- NA
    shapiro_W_res <- NA
  }

  if (length(ranef(object$model)$'(Intercept)') > 3 &
      length(ranef(object$model)$'(Intercept)') < 5000) {
    shapiro_p_ran <- shapiro.test(ranef(object$model)$'(Intercept)')[[2]]
    shapiro_W_ran <- shapiro.test(ranef(object$model)$'(Intercept)')[[1]]
  } else {
    warning(paste("Number of domains exceeds 5000 or is lower then 3 and thus",
                  "the Shapiro-Wilk test is not applicable for the random",
                  "effects."))
    shapiro_p_ran <- NA
    shapiro_W_ran <- NA
  }

  norm <- data.frame(Skewness  = c(skewness_res, skewness_ran),
                     Kurtosis  = c(kurtosis_res, kurtosis_ran),
                     Shapiro_W = c(shapiro_W_res, shapiro_W_ran),
                     Shapiro_p = c(shapiro_p_res, shapiro_p_ran),
                     row.names = c("Error", "Random_effect")
  )

  tempMod <- object$model
  tempMod$call$fixed <- object$fixed
  r_squared <- lme_rsquared(tempMod)
  if (is.matrix(r_squared)) {
    r_marginal <- r_squared[1, 1]
    r_conditional <- r_squared[1, 2]
  } else {
    r_marginal <- r_squared[1]
    r_conditional <- r_squared[2]
  }
  icc_mixed <- icc(object$model)

  coeff_det <- data.frame(
    Marginal_R2    = r_marginal,
    Conditional_R2 = r_conditional,
    row.names      = ""
  )

  sum_NER <- list(out_of_smp   = N_dom_unobs,
                  in_smp       = N_dom_smp,
                  size_smp     = smp_size,
                  size_pop     = pop_size,
                  size_dom     = sizedom_smp_pop,
                  smp_size_tab = NULL,
                  transform    = transform_method,
                  normality    = norm,
                  icc          = icc_mixed,
                  coeff_determ = coeff_det,
                  call         = call_emdi
  )

  class(sum_NER) <- c("summary.NER", "saeTrafo")
  sum_NER
}


#' @export
print.summary.NER <- function(x, ...) {

  throw_class_error(x, "NER")
  cat("Nested Error Regression Model\n")
  cat("\n")
  cat("Call:\n ")
  print(x$call)
  cat("\n")
  cat("Out-of-sample domains: ", x$out_of_smp, "\n")
  cat("In-sample domains: ", x$in_smp, "\n")
  cat("\n")
  cat("Sample sizes:\n")
  cat("Units in sample: ", x$size_smp, "\n")
  cat("Units in population: ", x$size_pop, "\n")
  print(x$size_dom)
  cat("\n")
  if (is.null(x$call$weights)) {
    cat("Explanatory measures:\n")
  } else {
    cat("Explanatory measures for the mixed model:\n")
  }
  print(x$coeff_determ)
  cat("\n")
  if (is.null(x$call$weights)) {
    cat("Residual diagnostics:\n")
  } else {
    cat("Residual diagnostics for the mixed model:\n")
  }
  print(x$normality)
  cat("\n")
  cat("ICC: ", x$icc, "\n")
  cat("\n")
  if (is.null(x$transform)) {
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation:\n")
    print(x$transform)
  }
}

#  ICC
icc <- function(model) {
  u <- as.numeric(VarCorr(model)[1, 1])
  e <- model$sigma^2
  u / (u + e)
}

lme_rsquared <- function(x) {

  VarFx <- var(fitted(x, level = 0L))
  mmRE <- model.matrix(x$modelStruct$reStruct,
                       data = x$data[rownames(x$fitted), ,
                                     drop = FALSE])
  n <- nrow(mmRE)
  sigma2 <- x$sigma^2
  reStruct <- x$modelStruct$reStruct
  if ((m <- length(reStruct)) > 1L) {
    nams <- names(reStruct)
    for (i in seq.int(m)) attr(reStruct[[i]], "Dimnames")[[2L]] <- paste(nams[[i]],
                                                                         attr(reStruct[[i]], "Dimnames")[[2L]], sep = ".")
  }
  varRe <- sum(vapply(reStruct, function(z) {
    sig <- nlme::pdMatrix(z) * sigma2
    mm1 <- mmRE[, rownames(sig), drop = FALSE]
    sum(matmultdiag(mm1 %*% sig, ty = mm1)) / n
  }, FUN.VALUE = numeric(1)))
  varTot <- sum(VarFx, varRe)
  res <- c(VarFx, varTot)/(varTot + sigma2)
  names(res) <- c("R2m", "R2c")

  return(res)
}

matmultdiag <- function (x, y, ty = t(y))
{
  if (ncol(x) != ncol(ty))
    stop("non-conformable arguments")
  if (nrow(x) != nrow(ty))
    stop("result is not a square matrix")
  return(rowSums(x * ty))
}
