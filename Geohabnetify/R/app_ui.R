#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title="Geohabnetify"),
        shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            id = "tabs",
            shinydashboard::menuItem("Home", tabName = "sm_1_dashboard", icon = icon("home")),
            shinydashboard::menuItem("Customize Input", icon = icon("th"), tabName = "sm_2_custinp",
                     badgeColor = "green"), #badgeLabel = "new",
            shinydashboard::menuItem("View Outputs", icon = icon("chart-column"), tabName = "sm_3_genop",
                     badgeColor = "blue") #badgeLabel = "new",
          )
        ),
        shinydashboard::dashboardBody(
          # titlePanel(title = tags$h2(
          #   tags$b("Title for the Basic Dashboard"),
          #   tags$style(HTML("h2 { text-align: center; }"))
          # )),
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "sm_1_dashboard",
                    uiOutput("uio_sm_1_dashboard")
            ),
            shinydashboard::tabItem(tabName = "sm_2_custinp",
                    uiOutput("uio_sm_2_custinp")
                    #h2("Widgets tab content 2")
            ),
            shinydashboard::tabItem(tabName = "sm_3_genop",
                    uiOutput("uio_sm_genop")
            )
          )
        ))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
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
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip({
          container: 'body',
          html: true
        });
      });
    ")),
    #shinyBS::useShinyBS(),
    #shinybusy::add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),
    shinybusy::use_busy_spinner(spin = "semipolar",position = "full-page"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Geohabnetify"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
