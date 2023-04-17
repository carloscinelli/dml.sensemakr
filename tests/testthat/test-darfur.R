library(sensemakr)
library(dml.sensemakr)

# loads dataset
data("darfur")

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female + village, data = darfur)
x
y <- as.numeric(darfur$peacefactor)
d <- darfur$directlyharmed
x <- model.matrix(~ age + farmer_dar + herder_dar +
                    pastvoted + hhsize_darfur + female + village, data = darfur)

test_that("Testing 401k PLM", {
  set.seed(12)
  dml.darfur <- dml(y,d,x, model = "npm", cf.folds = 2, cf.reps = 5)
  summary(dml.darfur)
  bench <- dml_benchmark(dml.darfur, benchmark_covariates = c("female"))

  robustness_value(dml.darfur)
})
