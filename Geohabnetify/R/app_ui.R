#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    fluidPage(
      shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Geohabnetify",titleWidth = "100%"),
        shinydashboard::dashboardSidebar(disable = TRUE),
        # remove sidebar
        shinydashboard::dashboardBody(

          shiny::tabsetPanel(
            id = "tabs",
            type = "tabs",

            shiny::tabPanel(
              title = tagList(icon("home"), "Home"),
              value = "sm_1_dashboard",
              uiOutput("uio_sm_1_dashboard")
            ),

            shiny::tabPanel(
              title = tagList(icon("th"), "Customize Input"),
              value = "sm_2_custinp",
              uiOutput("uio_sm_2_custinp")
            ),

            shiny::tabPanel(
              title = tagList(icon("chart-column"), "View Outputs"),
              value = "sm_3_genop",
              uiOutput("uio_sm_genop")
            )
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    golem::activate_js(),
    shinyjs::useShinyjs(),

    # =========================
    # UF GATOR THEME (COLORS ONLY)
    # =========================
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip({
          container: 'body',
          html: true
        });
      });
    ")),

    shinybusy::use_busy_spinner(
      spin = "semipolar",
      position = "full-page"
    ),

    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Geohabnetify"
    )
  )
}

# FA4616 - Orange
# 0021A5 - Blue
