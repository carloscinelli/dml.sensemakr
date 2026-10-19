# Development workflow

This package follows the workflow in [R Packages, second edition](https://r-pkgs.org/).
Work from the package root in a fresh R session.

Install the development dependencies, including suggested packages:

```r
devtools::install_deps(dependencies = TRUE)
```

During development, regenerate documentation from the roxygen comments and
load the current source:

```r
devtools::document()
devtools::load_all()
```

Run focused tests while making a change. Before completing the change, run
the entire suite and check the built package, including vignettes and the
PDF manual:

```r
devtools::test()
devtools::check(manual = TRUE, force_suggests = TRUE)
```

The full check needs Pandoc, a LaTeX installation, HTML Tidy, and the V8 R
package. Read the complete check results, including warnings, notes, and
skipped tests. After a fix, rerun the affected tests and finish with a full
test run and package check. A successful installation alone is not a check.

After checking, install the package and verify it in a fresh session:

```r
devtools::install(build_vignettes = TRUE)
```

Alternatively, use `R CMD INSTALL` on the source tarball produced by the
successful check to install that exact bundle without rebuilding its vignettes.

Keep tests self-contained. Define reusable fixtures as functions, cache
expensive fixture construction, and restore changes to random seeds,
graphics devices, options, or files. Do not attach packages or create
objects in the global environment from test files. Keep exploratory scripts
under `tools/`, outside the automated test suite.

Keep unreleased work under a development version and a development heading
in `NEWS.md`. Review the Git diff and commit completed changes. Publishing,
tagging, or submitting a release is a separate operation.

## Website publishing

The `pkgdown` workflow builds the website from `main` and publishes the generated
files to `gh-pages`. In GitHub Settings > Pages, use **Deploy from a branch**,
select **gh-pages**, and select **/(root)**. The checked-in `main/docs` directory
contains an older website and is not the publishing source.
