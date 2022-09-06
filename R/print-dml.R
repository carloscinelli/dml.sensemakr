
#' @export
print.summary_dml <- function(x, ...){
  cat("\n")
  cat("Debiased Machine Learning\n")
  cat("\n")
  cat("", "Model:", ifelse(x$info$model == "plm", "Partially Linear", "Nonparametric"), "\n")
  cat("", "Cross-Fitting:",x$info$cf.folds, "folds,", x$info$cf.reps, "reps", "\n")
  cat("", "ML Method:",
      "outcome", paste0("(",x$info$yreg$method, ", R2 = ", round(x$r2y,3), "),"),
      "treatment", paste0("(",x$info$dreg$method,", R2 = ", round(x$r2d,3), ")\n"))
  cat("", "Tuning:", ifelse(x$info$dirty.tuning, "dirty", "clean"), "\n")

  cat("\n")

  cat("Average Treatment Effect:", "\n\n")
  print(x$main)

  no.groups <- is.null(x$groups)
  if(!no.groups){
    cat("\n")
    cat("Group Average Treatment Effect:", "\n\n")
    print(x$groups)
    cat("\n")
  }
  cat("Note: DML estimates combined using the", x$combine.method, "method.")
}

#' @export
print.dml <- function(x, digits = max(3L, getOption("digits") - 3L), combine.method = "median", ...){
  cat("\n")
  cat("Debiased Machine Learning\n")
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

    if (length(coef(x))) {
      cat("Estimates:\n")
      print.default(format(coef(x), digits = digits), print.gap = 2L,
                    quote = FALSE)
    }
    else cat("No coefficients\n")
    cat("\n")
    invisible(x)
}



#' @export
print.cmat <- function(x, ...){
  colnames(x) <-  c("Estimate", "Std. Error", "t value", "P(>|t|)")
  # rownames(x) <-  toupper(rownames(x))
  # rownames(x) <- sapply(strsplit(rownames(x), split = "\\."), function(x) paste(x, collapse = " "))
  printCoefmat(x, has.Pvalue = T, P.values = T, signif.stars = T, ...)
}

# vcov.dml <- function(object, ...){
#   se(object, ...)^2
# }




