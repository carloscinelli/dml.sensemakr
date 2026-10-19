# Keep all plot output out of the source tree and close the device afterwards.
withr::local_pdf(NULL, .local_envir = testthat::teardown_env())
