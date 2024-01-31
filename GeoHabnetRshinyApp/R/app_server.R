#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(yaml)
app_server <- function(input, output, session) {

  #UI###################
  # Your application server logic
  param_file_default <- geohabnet::get_parameters()
  param_file_default <- yaml::read_yaml(param_file_default)
  #write_yaml(param_file_default,"data/default_param.yml")


  #First Paramater #SEMI AUTOMATED
  param_1 <- list(
    name=names(param_file_default$default$`CCRI parameters`[1]),
    options=list(
      list(
        option=names(param_file_default$default$`CCRI parameters`$Hosts)[1],
        choices = geodata::monfredaCrops()$name
      ),
      list(
        option=names(param_file_default$default$`CCRI parameters`$Hosts)[2],
        choices = as.data.frame(geodata::spamCrops())$crop
      ),
      list(
        option=names(param_file_default$default$`CCRI parameters`$Hosts)[4],
        choices = ""
      )
      )
  )

  #Default density threshold values AUTOMATED
  param_dt_values <- param_file_default$default$`CCRI parameters`$HostDensityThreshold

  #Default distance metrics values AUTOMATED
  param_lt_values <- param_file_default$default$`CCRI parameters`$LinkThreshold

  #Gives all the supported network metrics AUTOMATED
  param_metrics <- geohabnet::supported_metrics()
  #Storing default values of network metrics
  param_metrics_default <- param_file_default$default$`CCRI parameters`$NetworkMetrics

  #Storing default priority map values AUTOMATED DOUBT
  param_prioritymaps_default <- param_file_default$default$`CCRI parameters`$PriorityMaps


  #Extracting CCRI parameters and their options


  #Input Tab##############
  output$uio_sm_2_custinp <- renderUI({
    fluidPage(
      h2("Select Inputs"),
      div(
             uiOutput("uio_inputs"),
             column(1,offset =11,actionButton("inp_submit_all","Submit"))
      )
    )
  })
  output$uio_inputs <- renderUI({
    fluidPage(
      #row 1#########
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Input Set 1",
      column(3,
             selectInput("inp_mofreda",
                         label =shinyBS::tipify(el = div("Mofreda",icon(name = "info-circle", lib = "font-awesome")), title = "Mofreda"),
                         choices = c(param_1$options[[1]]$choices),
                         multiple = T),
             radioButtons("inp_mapspam_options",label = "Mapspam",choices = c("2010","2017"),inline = T),
             selectInput("inp_mapspam",label = NULL,choices = c(param_1$options[[2]]$choices),multiple = T),#param_1$options[[2]]$option
             shinyBS::bsTooltip("inp_mapspam", "Info", trigger = "hover",
                       "right", options = list(container = "body")),
             fileInput("inp_host_file","File")#,

            ),
      column(3,style="height:23rem;overflow-y:auto;",
             div(
               h5("Density Threshold",style="font-weight: bold;margin-right: 1rem;"),
               actionButton("inp_add_dt",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
               actionButton("inp_remove_dt",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
               style="display:inline-flex;"
             ),
             uiOutput("uio_add_dt")
      ),
      column(3,style="height:23rem;overflow-y:auto;",
             div(
               h5("Link Threshold",style="font-weight: bold;margin-right: 1rem;"),
               actionButton("inp_add_lt",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
               actionButton("inp_remove_lt",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
               style="display:inline-flex;"
             ),
             uiOutput("uio_add_lt")
      ),
      column(2,
             checkboxGroupInput("inp_agg_strat","Aggregation Strategy",choices = c("sum","mean")),
             checkboxGroupInput("inp_distance_strat","Distance Strategy",choices = geohabnet::dist_methods()),
             numericInput("inp_resolution","Resolution",value=12),
      ),
      column(1,
             shinyBS::tipify(el = div(h5("Geo Extent",style="font-weight: bold;margin-top: -1px;"),icon(name = "info-circle", lib = "font-awesome"),style="display:inline-flex;"), title = paste0(geohabnet::geoscale_param(),collapse = ";")),
             checkboxInput("inp_globalextent",label = "Global",value = TRUE),
             uiOutput("uio_globalextent_user")
      )
      ),
      #row 2##################
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Input Set 2",
             h5("Network Metrics",style="font-weight: bold"),
             column(12,
               column(2,
                      h5("Inverse Power Law",style="font-weight: bold"),
                      selectInput(inputId = "inp_ipl_dd",NULL,choices = param_metrics,selected = tolower(param_metrics_default$InversePowerLaw$metrics),multiple = T)
                      ),
               column(9,
                      uiOutput("uio_create_ipl_input")
                      ),
               column(1,
                      h5("Beta",style="font-weight: bold"),
                      div(
                        actionButton("inp_add_beta",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                        actionButton("inp_remove_beta",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                        style="display:inline-flex;"
                      ),
                      uiOutput("uio_create_ipl_beta_input")
                      )
             ),
             column(12,
                    column(2,
                           h5("Negative Exponential",style="font-weight: bold"),
                           selectInput(inputId = "inp_ne_dd",NULL,choices = param_metrics,selected = tolower(param_metrics_default$NegativeExponential$metrics),multiple = T)
                    ),
                    column(9,
                           uiOutput("uio_create_ne_input")
                    ),
                    column(1,
                           h5("Gamma",style="font-weight: bold"),
                           div(
                             actionButton("inp_add_gamma",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                             actionButton("inp_remove_gamma",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                             style="display:inline-flex;"
                           ),
                           uiOutput("uio_create_ne_gamma_input")
                    )
              )

      ),
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Input Set 3",
                          h5("Priority Maps",style="font-weight: bold"),
                          column(3,
                                 shinyFiles::shinyDirButton("inp_prioritymaps_1", names(param_prioritymaps_default)[1] ,
                                                title = "Please select a folder:", multiple = FALSE,
                                                buttonType = "default", class = NULL),
                                 #checkboxInput("inp_prioritymaps_1",label = ,value = param_prioritymaps_default[[1]])
                                 ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_2",label = names(param_prioritymaps_default)[2],value = param_prioritymaps_default[[2]])
                          ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_3",label = names(param_prioritymaps_default)[3],value = param_prioritymaps_default[[3]])
                          ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_4",label = names(param_prioritymaps_default)[4],value = param_prioritymaps_default[[4]])
                          )
      )

    )

  })

  volumes = c(home = "C:/Users/")
  observe({
    shinyFiles::shinyDirChoose(input, "inp_prioritymaps_1",
                   roots = volumes)
  })

  #First##########
  fcn_add_numeric_input <- function(prefix,suffix,value,isLabel=F){
    input_list <- lapply(suffix,function(i){
      inp_id <- paste0(prefix,i)
      if(isLabel){
        value_new <- value
        if(prefix=="inp_ipl_matrix_"){
          vecCheck <- tolower(param_metrics_default$InversePowerLaw$metrics)
          if(i %in% vecCheck){
            value_new <- param_metrics_default$InversePowerLaw$weights[which(i == vecCheck)]
          }
        }
        else if(prefix=="inp_ne_matrix_"){
          vecCheck <- tolower(param_metrics_default$NegativeExponential$metrics)
          if(i %in% vecCheck){
            value_new <- param_metrics_default$NegativeExponential$weights[which(i == vecCheck)]
          }
        }
        column(3,numericInput(inp_id,label = i,value = value_new))
      }
      else{
        numericInput(inp_id,label = NULL,value = value[i])
      }

    })
    do.call(tagList,input_list)
  }
  global_btn_cnt <- reactiveValues(cnt_dt = -1,cnt_lt = -1)
  observeEvent(input$inp_add_dt,{
    req(input$inp_add_dt)
    isolate(global_btn_cnt$cnt_dt <- global_btn_cnt$cnt_dt + 1)
  })
  observeEvent(input$inp_remove_dt,{
    req(input$inp_remove_dt)
    isolate(global_btn_cnt$cnt_dt <- global_btn_cnt$cnt_dt - 1)
  })
  output$uio_add_dt <- renderUI({
    # -1 for the first time run only
    if(global_btn_cnt$cnt_dt== -1){
      global_btn_cnt$cnt_dt <- length(param_dt_values)
    }
    if(global_btn_cnt$cnt_dt==0){
      global_btn_cnt$cnt_dt <- 1
    }
    cnt <- global_btn_cnt$cnt_dt
    fcn_add_numeric_input("inp_density_threshold_",seq(1:cnt),param_dt_values)
  })
  observeEvent(input$inp_add_lt,{
    req(input$inp_add_lt)
    isolate(global_btn_cnt$cnt_lt <- global_btn_cnt$cnt_lt + 1)
  })
  observeEvent(input$inp_remove_lt,{
    req(input$inp_remove_lt)
    isolate(global_btn_cnt$cnt_lt <- global_btn_cnt$cnt_lt - 1)
  })
  output$uio_add_lt <- renderUI({
    if(global_btn_cnt$cnt_lt== -1){
      global_btn_cnt$cnt_lt <- length(param_lt_values)
    }
    if(global_btn_cnt$cnt_lt==0){
      global_btn_cnt$cnt_lt <- 1
    }
    cnt <- global_btn_cnt$cnt_lt
    fcn_add_numeric_input("inp_link_threshold_",seq(1:cnt),param_lt_values)
  })
  output$uio_globalextent_user <- renderUI({
    if(input$inp_globalextent==T){
      div()
    }else{
      default_global <- geohabnet::geoscale_param() #
      div(
        numericInput("inp_geoscale_1","X min",default_global[1]),
        numericInput("inp_geoscale_2","X Max",default_global[2]),
        numericInput("inp_geoscale_3","Y Min",default_global[3]),
        numericInput("inp_geoscale_4","Y Max",default_global[4])
      )
    }
  })

  #Second################
  output$uio_create_ipl_input <- renderUI({
    req(input$inp_ipl_dd)
    choices <- input$inp_ipl_dd
    fcn_add_numeric_input("inp_ipl_matrix_",choices,0,isLabel=T)
  })
  output$uio_create_ipl_beta_input <- renderUI({
    choices <- unlist(param_file_default$default$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta)
    fcn_add_numeric_input("inp_ipl_beta_",suffix = seq(1:length(choices)),choices,isLabel=F)
  })
  output$uio_create_ne_input <- renderUI({
    req(input$inp_ne_dd)
    choices <- input$inp_ne_dd
    fcn_add_numeric_input("inp_ne_matrix_",choices,0,isLabel=T)
  })
  output$uio_create_ne_gamma_input <- renderUI({
    choices <- unlist(param_file_default$default$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma)
    fcn_add_numeric_input("inp_ne_gamma_",suffix = seq(1:length(choices)),choices,isLabel=F)
  })


  observeEvent(input$inp_submit_all,{
    sendSweetAlert(session = session,title = "Inputs submitted. Updating Parameters",type = "success")
    geohabnet::reset_params()
    param_file <- geohabnet::get_parameters()
    def_yaml <- yaml::read_yaml(param_file)
    #HOSTS
    inp <- input$inp_mofreda
    def_yaml$default$`CCRI parameters`$Hosts$monfreda <- inp
    inp <- input$inp_mapspam
    #def_yaml$default$`CCRI parameters`$Hosts$mapspam <- inp

    #Density Threshold
    inp <- input$inp_density_threshold_1
    def_yaml$default$`CCRI parameters`$HostDensityThreshold <- c(inp,0.00025)

    #Link Threshold
    inp <- input$inp_link_threshold
    def_yaml$default$`CCRI parameters`$LinkThreshold <- inp

    #Aggregation Strategy
    inp <- input$inp_agg_strat
    def_yaml$default$`CCRI parameters`$AggregationStrategy <- inp

    #Distance Strategy
    inp <- input$inp_distance_strat
    def_yaml$default$`CCRI parameters`$DistanceStrategy <- inp

    #Resolution
    inp <- input$inp_resolution
    def_yaml$default$`CCRI parameters`$Resolution <- inp

    #Global Extent
    inp <- input$inp_globalextent
    def_yaml$default$`CCRI parameters`$GeoExtent$global <- inp

    #Network Metrics Inverse Power
    def_yaml$default$`CCRI parameters`$NetworkMetrics$InversePowerLaw$weights <- c(input$inp_netmet_ipl_bet,input$inp_netmet_ipl_ns,input$inp_netmet_ipl_nn,input$inp_netmet_ipl_evc)

    #Network Metrics Negative Exponential
    def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$weights <- c(input$inp_netmet_ne_bet,input$inp_netmet_ne_ns,input$inp_netmet_ne_nn,input$inp_netmet_ne_evc)

    yaml::write_yaml(def_yaml,param_file)
    set_parameters(param_file)
    Sys.sleep(5)
    sendSweetAlert(session = session,title = "Parameters Set. Generating Outputs",type = "success")

    sensitivity_analysis()

  })


}
