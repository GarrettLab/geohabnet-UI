# inst/app/app.R
options(shiny.loadTimeout = 60000)
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB limit
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
geohabnetify::run_app()
