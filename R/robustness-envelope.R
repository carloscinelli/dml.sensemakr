# Internal implementation of the formal RV/XRV confidence-envelope definitions.
#
# The public confidence-bound methods use these helpers when max = TRUE and
# retain their fixed-sensitivity calculation when max = FALSE.  The helpers
# below work directly with the common factor
#
#   |rho_0| C_{0 Delta Y} C_{0D}
#
# so that large factors do not have to be round-tripped through a sensitivity
# R-squared arbitrarily close to one.

.rv_quadratic_roots <- function(constant, linear, quadratic) {
  coefficients <- c(constant, linear, quadratic)
  if (any(!is.finite(coefficients))) {
    return(numeric(0))
  }
  coefficient_scale <- max(abs(coefficients))
  if (coefficient_scale == 0) {
    return(numeric(0))
  }
  constant <- constant / coefficient_scale
  linear <- linear / coefficient_scale
  quadratic <- quadratic / coefficient_scale
  if (quadratic == 0) {
    if (linear == 0) {
      return(numeric(0))
    }
    return(-constant / linear)
  }

  discriminant <- linear^2 - 4 * quadratic * constant
  discriminant_tolerance <- 128 * .Machine$double.eps *
    max(linear^2, abs(4 * quadratic * constant))
  if (discriminant < -discriminant_tolerance) {
    return(numeric(0))
  }
  discriminant <- max(discriminant, 0)

  square_root <- sqrt(discriminant)
  signed_square_root <- if (linear >= 0) square_root else -square_root
  stable_numerator <- -0.5 * (linear + signed_square_root)

  if (stable_numerator == 0) {
    roots <- -linear / (2 * quadratic)
  } else {
    roots <- c(stable_numerator / quadratic,
               constant / stable_numerator)
  }
  unique(roots[is.finite(roots)])
}

.rv_unique_sorted <- function(values, tolerance = 1e-12) {
  values <- sort(values[is.finite(values)])
  if (length(values) < 2L) {
    return(values)
  }
  keep <- c(
    TRUE,
    diff(values) > tolerance *
      (1 + pmax(abs(values[-length(values)]), abs(values[-1L])))
  )
  values[keep]
}

.rv_median_indices <- function(values) {
  ordered <- order(values, method = "radix")
  count <- length(ordered)
  if (count %% 2L == 1L) {
    ordered[(count + 1L) / 2L]
  } else {
    ordered[c(count / 2L, count / 2L + 1L)]
  }
}

.rv_results_for_parameter <- function(model, parameter) {
  if (startsWith(parameter, .group_marker)) {
    group_name <- substring(parameter, nchar(.group_marker) + 1L)
    results <- model$results$groups[[group_name]]
  } else {
    slot <- unname(.target_to_slot[parameter])
    results <- if (length(slot) == 1L && !is.na(slot)) {
      model$results$main[[slot]]
    } else {
      NULL
    }
  }
  if (is.null(results)) {
    stop("Could not match parameter '", parameter,
         "' to the stored cross-fitting results.", call. = FALSE)
  }
  results
}

.rv_build_statistics <- function(theta, S2, theta_se, S2_se, covariance) {
  S <- sqrt(S2)
  list(
    theta = theta,
    S2 = S2,
    S = S,
    theta_se = theta_se,
    S2_se = S2_se,
    covariance = covariance,
    variance_constant = theta_se^2,
    variance_linear_magnitude = covariance / S,
    variance_quadratic = S2_se^2 / (4 * S2)
  )
}

.rv_numeric_statistics <- function(theta, S2, theta_se, S2_se,
                                   covariance) {
  values <- list(
    theta = theta,
    S2 = S2,
    theta_se = theta_se,
    S2_se = S2_se,
    covariance = covariance
  )
  lengths <- vapply(values, length, integer(1))
  if (any(lengths == 0L) || length(unique(lengths)) != 1L) {
    stop("The numeric confidence-bound statistics must have equal, positive ",
         "lengths.", call. = FALSE)
  }

  valid <- is.finite(theta) & is.finite(S2) & S2 > 0 &
    is.finite(theta_se) & theta_se >= 0 &
    is.finite(S2_se) & S2_se >= 0 & is.finite(covariance)
  if (!all(valid) && any(valid)) {
    warning("Some repetitions have invalid statistics or non-positive S2; ",
            "dropping those repetitions from the confidence-envelope ",
            "calculation.", call. = FALSE)
  }
  if (!any(valid)) {
    return(NULL)
  }

  .rv_build_statistics(
    theta = theta[valid],
    S2 = S2[valid],
    theta_se = theta_se[valid],
    S2_se = S2_se[valid],
    covariance = covariance[valid]
  )
}

.rv_extract_statistics <- function(results, parameter) {
  required <- c("theta.s", "S2", "se.theta.s", "se.S2",
                "cov.theta.S2")
  complete <- vapply(
    results,
    function(result) all(required %in% names(result$estimates)),
    logical(1)
  )
  if (!all(complete)) {
    warning("Some cross-fitting repetitions for '", parameter,
            "' lack statistics required for the robustness value; ",
            "dropping those repetitions.", call. = FALSE)
    results <- results[complete]
  }
  if (!length(results)) {
    return(NULL)
  }

  extract <- function(name) {
    vapply(results, function(result) result$estimates[[name]], numeric(1))
  }
  theta <- extract("theta.s")
  S2 <- extract("S2")
  theta_se <- extract("se.theta.s")
  S2_se <- extract("se.S2")
  covariance <- extract("cov.theta.S2")
  valid <- is.finite(theta) & is.finite(S2) & S2 > 0 &
    is.finite(theta_se) & theta_se >= 0 &
    is.finite(S2_se) & S2_se >= 0 & is.finite(covariance)

  if (!all(valid) && any(valid)) {
    warning("Some cross-fitting repetitions for '", parameter,
            "' have invalid or non-positive S2 statistics; dropping those ",
            "repetitions from the positive-sensitivity calculation.",
            call. = FALSE)
  }
  if (!any(valid)) {
    return(NULL)
  }

  .rv_build_statistics(
    theta = theta[valid],
    S2 = S2[valid],
    theta_se = theta_se[valid],
    S2_se = S2_se[valid],
    covariance = covariance[valid]
  )
}

.rv_nonnegative_variance <- function(values, calculation_scale,
                                     description) {
  tolerance <- 1e-10 * calculation_scale
  if (any(values < -tolerance)) {
    stop(description, " is negative beyond numerical tolerance.",
         call. = FALSE)
  }
  pmax(values, 0)
}

.rv_endpoint_variance <- function(factor, statistics, direction) {
  variance_constant <- statistics$variance_constant
  variance_linear <- direction * statistics$variance_linear_magnitude
  variance_quadratic <- statistics$variance_quadratic
  constant_se <- sqrt(variance_constant)

  positive_constant <- constant_se > 0
  if (any(!positive_constant & variance_linear != 0)) {
    stop("An endpoint covariance is incompatible with a zero variance.",
         call. = FALSE)
  }

  projected_slope <- numeric(length(constant_se))
  projected_slope[positive_constant] <-
    variance_linear[positive_constant] /
    (2 * constant_se[positive_constant])
  orthogonal_quadratic <- variance_quadratic - projected_slope^2
  orthogonal_quadratic <- .rv_nonnegative_variance(
    orthogonal_quadratic,
    variance_quadratic + projected_slope^2,
    "An endpoint variance quadratic"
  )

  (constant_se + factor * projected_slope)^2 +
    factor^2 * orthogonal_quadratic
}

.rv_factor_bounds <- function(factor, statistics, critical_value,
                              combine.method) {
  lower_point <- statistics$theta - factor * statistics$S
  upper_point <- statistics$theta + factor * statistics$S
  lower_variance <- .rv_endpoint_variance(
    factor, statistics = statistics, direction = -1
  )
  upper_variance <- .rv_endpoint_variance(
    factor, statistics = statistics, direction = 1
  )

  combine <- if (combine.method == "mean") combine.mean else combine.median
  lower <- combine(lower_point, sqrt(lower_variance))
  upper <- combine(upper_point, sqrt(upper_variance))
  c(
    lwr = unname(lower["estimate"] - critical_value * lower["se"]),
    upr = unname(upper["estimate"] + critical_value * upper["se"])
  )
}

.rv_side_components <- function(statistics, side) {
  direction <- switch(
    side,
    lwr = -1,
    upr = 1,
    stop("Unknown confidence-bound side.", call. = FALSE)
  )
  list(
    point_constant = statistics$theta,
    point_linear = direction * statistics$S,
    variance_constant = statistics$variance_constant,
    variance_linear = direction * statistics$variance_linear_magnitude,
    variance_quadratic = statistics$variance_quadratic,
    endpoint_direction = direction
  )
}

.rv_adjusted_variance_coefficients <- function(components,
                                               point_coefficients) {
  centered_constant <- components$point_constant - point_coefficients[1L]
  centered_linear <- components$point_linear - point_coefficients[2L]
  cbind(
    constant = components$variance_constant + centered_constant^2,
    linear = components$variance_linear +
      2 * centered_constant * centered_linear,
    quadratic = components$variance_quadratic + centered_linear^2
  )
}

.rv_mean_piece <- function(statistics, side, left, right) {
  components <- .rv_side_components(statistics, side)
  point <- c(
    mean(components$point_constant),
    mean(components$point_linear)
  )
  adjusted_variance <- .rv_adjusted_variance_coefficients(
    components, point
  )
  variance <- colMeans(adjusted_variance)
  list(
    left = left,
    right = right,
    point_constant = point[1L],
    point_linear = point[2L],
    variance_constant = variance[1L],
    variance_linear = variance[2L],
    variance_quadratic = variance[3L],
    endpoint_direction = components$endpoint_direction
  )
}

.rv_point_breakpoints <- function(components, maximum_factor) {
  candidates <- c(0, maximum_factor)
  count <- length(components$point_constant)
  if (count >= 2L) {
    pairs <- utils::combn(count, 2L)
    for (column in seq_len(ncol(pairs))) {
      first <- pairs[1L, column]
      second <- pairs[2L, column]
      root <- .rv_quadratic_roots(
        components$point_constant[first] -
          components$point_constant[second],
        components$point_linear[first] - components$point_linear[second],
        0
      )
      candidates <- c(candidates, root)
    }
  }
  interior <- candidates[candidates > 0 & candidates < maximum_factor]
  c(0, .rv_unique_sorted(interior), maximum_factor)
}

.rv_variance_breakpoints <- function(adjusted_variance, left, right) {
  candidates <- c(left, right)
  count <- nrow(adjusted_variance)
  if (count >= 2L) {
    pairs <- utils::combn(count, 2L)
    for (column in seq_len(ncol(pairs))) {
      difference <- adjusted_variance[pairs[1L, column], ] -
        adjusted_variance[pairs[2L, column], ]
      roots <- .rv_quadratic_roots(
        difference[1L], difference[2L], difference[3L]
      )
      candidates <- c(candidates, roots)
    }
  }
  interior <- candidates[candidates > left & candidates < right]
  c(left, .rv_unique_sorted(interior), right)
}

.rv_median_pieces <- function(statistics, side, maximum_factor) {
  components <- .rv_side_components(statistics, side)
  point_breakpoints <- .rv_point_breakpoints(components, maximum_factor)
  pieces <- list()

  for (point_index in seq_len(length(point_breakpoints) - 1L)) {
    point_left <- point_breakpoints[point_index]
    point_right <- point_breakpoints[point_index + 1L]
    if (point_right <= point_left) {
      next
    }
    point_midpoint <- point_left + (point_right - point_left) / 2
    point_at_midpoint <- components$point_constant +
      components$point_linear * point_midpoint
    point_indices <- .rv_median_indices(point_at_midpoint)
    point <- c(
      mean(components$point_constant[point_indices]),
      mean(components$point_linear[point_indices])
    )

    # Current combine.median() takes the median of
    #   se_j^2 + (theta_j - median(theta))^2.
    adjusted_variance <- .rv_adjusted_variance_coefficients(
      components, point
    )
    variance_breakpoints <- .rv_variance_breakpoints(
      adjusted_variance, point_left, point_right
    )

    for (variance_index in seq_len(length(variance_breakpoints) - 1L)) {
      left <- variance_breakpoints[variance_index]
      right <- variance_breakpoints[variance_index + 1L]
      if (right <= left) {
        next
      }
      midpoint <- left + (right - left) / 2
      powers <- c(1, midpoint, midpoint^2)
      variance_at_midpoint <- as.vector(adjusted_variance %*% powers)
      variance_indices <- .rv_median_indices(variance_at_midpoint)
      variance <- colMeans(
        adjusted_variance[variance_indices, , drop = FALSE]
      )
      pieces[[length(pieces) + 1L]] <- list(
        left = left,
        right = right,
        point_constant = point[1L],
        point_linear = point[2L],
        variance_constant = variance[1L],
        variance_linear = variance[2L],
        variance_quadratic = variance[3L],
        endpoint_direction = components$endpoint_direction
      )
    }
  }
  pieces
}

.rv_pieces <- function(statistics, side, maximum_factor, combine.method) {
  if (combine.method == "mean" || length(statistics$theta) == 1L) {
    return(list(.rv_mean_piece(statistics, side, 0, maximum_factor)))
  }
  .rv_median_pieces(statistics, side, maximum_factor)
}

.rv_piece_candidates <- function(piece, critical_value) {
  point_linear <- piece$point_linear
  variance_constant <- piece$variance_constant
  variance_linear <- piece$variance_linear
  variance_quadratic <- piece$variance_quadratic

  # Squaring the first-order condition may add roots but cannot remove any.
  stationary_roots <- .rv_quadratic_roots(
    4 * point_linear^2 * variance_constant -
      critical_value^2 * variance_linear^2,
    4 * point_linear^2 * variance_linear -
      4 * critical_value^2 * variance_linear * variance_quadratic,
    4 * point_linear^2 * variance_quadratic -
      4 * critical_value^2 * variance_quadratic^2
  )
  variance_roots <- .rv_quadratic_roots(
    variance_constant, variance_linear, variance_quadratic
  )
  interior <- c(stationary_roots, variance_roots)
  interior <- interior[interior > piece$left & interior < piece$right]
  c(piece$left, .rv_unique_sorted(interior), piece$right)
}

.rv_endpoint_extreme <- function(statistics, side, maximum_factor,
                                 critical_value, combine.method) {
  if (maximum_factor == 0) {
    value <- .rv_factor_bounds(
      0, statistics, critical_value, combine.method
    )[side]
    return(c(value = unname(value), factor = 0))
  }
  pieces <- .rv_pieces(
    statistics, side, maximum_factor, combine.method
  )
  candidates <- unlist(
    lapply(pieces, .rv_piece_candidates, critical_value = critical_value),
    use.names = FALSE
  )
  candidates <- .rv_unique_sorted(candidates)
  values <- vapply(
    candidates,
    function(factor) {
      .rv_factor_bounds(
        factor, statistics, critical_value, combine.method
      )[side]
    },
    numeric(1)
  )
  selected <- if (side == "lwr") which.min(values) else which.max(values)
  c(value = unname(values[selected]), factor = candidates[selected])
}

.rv_envelope <- function(statistics, maximum_factor, critical_value,
                         combine.method) {
  lower <- .rv_endpoint_extreme(
    statistics, "lwr", maximum_factor, critical_value, combine.method
  )
  upper <- .rv_endpoint_extreme(
    statistics, "upr", maximum_factor, critical_value, combine.method
  )
  c(
    lwr = unname(lower["value"]),
    upr = unname(upper["value"]),
    lwr.at = unname(lower["factor"]),
    upr.at = unname(upper["factor"])
  )
}

.rv_model_envelopes <- function(model, maximum_factor, critical_value,
                                combine.method) {
  main_slots <- names(model$results$main)
  main_parameters <- unname(.slot_to_target[main_slots])
  main_parameters <- main_parameters[!is.na(main_parameters)]
  group_parameters <- if (is.null(model$results$groups)) {
    character(0)
  } else {
    paste0(.group_marker, names(model$results$groups))
  }
  parameter_names <- c(main_parameters, group_parameters)
  if (!length(parameter_names)) {
    stop("Could not identify parameters for the confidence envelopes.",
         call. = FALSE)
  }

  envelopes <- lapply(parameter_names, function(parameter) {
    results <- .rv_results_for_parameter(model, parameter)
    statistics <- .rv_extract_statistics(results, parameter)
    if (is.null(statistics)) {
      warning("All cross-fitting repetitions for '", parameter,
              "' have invalid statistics or non-positive S2; returning NA ",
              "for its confidence envelope.", call. = FALSE)
      return(c(lwr = NA_real_, upr = NA_real_,
               lwr.at = NA_real_, upr.at = NA_real_))
    }
    tryCatch(
      .rv_envelope(
        statistics = statistics,
        maximum_factor = maximum_factor,
        critical_value = critical_value,
        combine.method = combine.method
      ),
      error = function(error) {
        warning("Unable to calculate the confidence envelope for '",
                parameter, "': ", conditionMessage(error), call. = FALSE)
        c(lwr = NA_real_, upr = NA_real_,
          lwr.at = NA_real_, upr.at = NA_real_)
      }
    )
  })
  names(envelopes) <- parameter_names
  do.call(rbind, envelopes)
}

.rv_contains <- function(interval, theta) {
  # The inclusive no-confounding case is handled before this search.  A
  # strict comparison brackets a positive-sensitivity crossing from the
  # containing side; its infimum is still the equality point.
  interval["lwr"] < theta && theta < interval["upr"]
}

.rv_required_factor_general <- function(statistics, theta, critical_value,
                                        combine.method,
                                        tolerance = 1e-10,
                                        maximum_factor = 1e6,
                                        maximum_iterations = 100L) {
  lower_factor <- 0
  upper_factor <- min(1, maximum_factor)
  upper_envelope <- .rv_envelope(
    statistics, upper_factor, critical_value, combine.method
  )
  while (!.rv_contains(upper_envelope, theta) &&
         upper_factor < maximum_factor) {
    lower_factor <- upper_factor
    upper_factor <- min(2 * upper_factor, maximum_factor)
    upper_envelope <- .rv_envelope(
      statistics, upper_factor, critical_value, combine.method
    )
  }
  if (!.rv_contains(upper_envelope, theta)) {
    stop("The confidence envelope did not reach theta before the maximum ",
         "factor.", call. = FALSE)
  }

  iterations <- 0L
  while (iterations < maximum_iterations &&
         upper_factor - lower_factor >
           tolerance * (1 + upper_factor)) {
    midpoint <- lower_factor + (upper_factor - lower_factor) / 2
    midpoint_envelope <- .rv_envelope(
      statistics, midpoint, critical_value, combine.method
    )
    if (.rv_contains(midpoint_envelope, theta)) {
      upper_factor <- midpoint
      upper_envelope <- midpoint_envelope
    } else {
      lower_factor <- midpoint
    }
    iterations <- iterations + 1L
  }
  upper_factor
}

.rv_required_factor_mean <- function(statistics, theta, critical_value,
                                     side, tolerance = 1e-10) {
  piece <- .rv_mean_piece(statistics, side, 0, Inf)
  point_difference <- piece$point_constant - theta

  if (critical_value == 0) {
    if (piece$point_linear == 0) {
      return(NA_real_)
    }
    root <- -point_difference / piece$point_linear
    return(if (is.finite(root) && root >= 0) root else NA_real_)
  }

  roots <- .rv_quadratic_roots(
    point_difference^2 -
      critical_value^2 * piece$variance_constant,
    2 * point_difference * piece$point_linear -
      critical_value^2 * piece$variance_linear,
    piece$point_linear^2 -
      critical_value^2 * piece$variance_quadratic
  )
  roots <- sort(roots[is.finite(roots) & roots >= 0])
  if (!length(roots)) {
    return(NA_real_)
  }

  valid <- vapply(
    roots,
    function(root) {
      value <- .rv_factor_bounds(
        root, statistics, critical_value, "mean"
      )[side]
      if (!is.finite(value)) {
        return(FALSE)
      }
      scale <- max(
        abs(theta), abs(value), abs(piece$point_constant),
        abs(piece$point_linear * root)
      )
      root_tolerance <- max(tolerance, 512 * .Machine$double.eps)
      abs(value - theta) <= root_tolerance * scale
    },
    logical(1)
  )
  roots <- roots[valid]
  if (!length(roots)) NA_real_ else roots[1L]
}

.rv_from_factor <- function(factor, rho2) {
  if (factor == 0) {
    return(0)
  }
  scaled <- factor / sqrt(rho2)
  if (scaled <= 1) {
    2 * scaled / (sqrt(scaled^2 + 4) + scaled)
  } else {
    2 / (sqrt(1 + 4 / scaled^2) + 1)
  }
}

.xrv_from_factor <- function(factor, rho2) {
  scaled <- factor / sqrt(rho2)
  if (scaled <= 1) {
    scaled^2 / (1 + scaled^2)
  } else {
    1 - 1 / (1 + scaled^2)
  }
}

.rv_method_arguments <- function(arguments, rho2 = NULL) {
  combine.method <- if (!is.null(arguments$combine.method)) {
    arguments$combine.method
  } else {
    "median"
  }
  arguments$combine.method <- NULL
  if (is.null(rho2)) {
    rho2 <- if (!is.null(arguments$rho2)) arguments$rho2 else 1
    arguments$rho2 <- NULL
  }
  list(
    combine.method = match.arg(combine.method, c("median", "mean")),
    rho2 = rho2,
    confint.arguments = arguments
  )
}

.rv_sensitivity_statistics <- function(model, theta, alpha, rho2,
                                       combine.method,
                                       confint.arguments = list()) {
  if (!inherits(model, "dml")) {
    stop("'model' must inherit from 'dml'.", call. = FALSE)
  }
  if (length(theta) != 1L || !is.numeric(theta) || !is.finite(theta)) {
    stop("'theta' must be a single finite number.", call. = FALSE)
  }
  if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) ||
      alpha <= 0 || alpha > 1) {
    stop("'alpha' must be a single number in (0, 1].", call. = FALSE)
  }
  if (length(rho2) != 1L || !is.numeric(rho2) || !is.finite(rho2) ||
      rho2 <= 0 || rho2 > 1) {
    stop("'rho2' must be a single number in (0, 1].", call. = FALSE)
  }
  combine.method <- match.arg(combine.method, c("median", "mean"))

  confint_call <- c(
    list(object = model, level = 1 - alpha,
         combine.method = combine.method),
    confint.arguments
  )
  no_confounding_intervals <- do.call(stats::confint, confint_call)
  if (is.null(dim(no_confounding_intervals)) ||
      ncol(no_confounding_intervals) != 2L ||
      is.null(rownames(no_confounding_intervals))) {
    stop("Could not identify the no-confounding confidence intervals.",
         call. = FALSE)
  }
  parameter_names <- rownames(no_confounding_intervals)
  factors <- setNames(rep(NA_real_, length(parameter_names)), parameter_names)
  critical_value <- stats::qnorm(max(1 - alpha, 0.5))

  for (index in seq_along(parameter_names)) {
    parameter <- parameter_names[index]
    interval <- no_confounding_intervals[index, ]
    if (all(is.finite(interval)) &&
        interval[1L] <= theta && theta <= interval[2L]) {
      factors[index] <- 0
      next
    }
    if (any(!is.finite(interval))) {
      warning("The no-confounding confidence interval for '", parameter,
              "' is not finite; returning NA for its robustness value.",
              call. = FALSE)
      next
    }

    results <- .rv_results_for_parameter(model, parameter)
    statistics <- .rv_extract_statistics(results, parameter)
    if (is.null(statistics)) {
      warning("All cross-fitting repetitions for '", parameter,
              "' have invalid statistics or non-positive S2; returning NA ",
              "for its robustness value.", call. = FALSE)
      next
    }
    side <- if (theta < interval[1L]) "lwr" else "upr"

    factor <- NA_real_
    if (combine.method == "mean" || length(statistics$theta) == 1L) {
      # With one repetition, mean and median aggregation coincide.
      factor <- tryCatch(
        .rv_required_factor_mean(
          statistics, theta, critical_value, side
        ),
        error = function(error) NA_real_
      )
    }
    if (!is.finite(factor) || factor < 0) {
      factor <- tryCatch(
        .rv_required_factor_general(
          statistics, theta, critical_value, combine.method
        ),
        error = function(error) {
          warning("Unable to calculate the robustness value for '",
                  parameter, "': ", conditionMessage(error), call. = FALSE)
          NA_real_
        }
      )
    }
    factors[index] <- factor
  }

  RV <- vapply(
    factors,
    function(factor) if (is.na(factor)) NA_real_ else .rv_from_factor(factor, rho2),
    numeric(1)
  )
  XRV <- vapply(
    factors,
    function(factor) if (is.na(factor)) NA_real_ else .xrv_from_factor(factor, rho2),
    numeric(1)
  )
  list(factor = factors, RV = RV, XRV = XRV)
}
