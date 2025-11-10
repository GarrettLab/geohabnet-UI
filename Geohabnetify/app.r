# inst/app/app.R

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
geohabnetify::run_app()
