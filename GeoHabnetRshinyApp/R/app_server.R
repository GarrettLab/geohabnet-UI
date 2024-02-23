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
  param_hosts <- list(
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

  #Default Beta values AUTOMATED
  param_beta_values <- unlist(param_file_default$default$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta)

  #Default Beta values AUTOMATED
  param_gamma_values <- unlist(param_file_default$default$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma)

  #Gives all the supported network metrics AUTOMATED
  param_metrics <- geohabnet::supported_metrics()
  #Storing default values of network metrics
  param_metrics_default <- param_file_default$default$`CCRI parameters`$NetworkMetrics

  #Storing default priority map values AUTOMATED DOUBT
  param_prioritymaps_default <- param_file_default$default$`CCRI parameters`$PriorityMaps


  #Extracting CCRI parameters and their options


  #Input Tab##############
  # output$uio_sm_2_custinp <- renderUI({
  #
  #   fluidPage(
  #     #h2("Select Inputs"),
  #     div(
  #            uiOutput("uio_inputs"),
  #
  #
  #     )
  #   )
  # })
  fcnAddInfo <- function(label,info){
    return(shinyBS::tipify(el = div(label,icon(name = "info-circle", lib = "font-awesome")), title = info))
  }
  output$uio_sm_2_custinp <- renderUI({
    #shinyjs::disable("inp_submit_all")
    fluidPage(

      #Host Considerations#########
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Host Considerations",
                          column(3,
                                 selectInput("inp_mofreda",
                                             label =fcnAddInfo("Monfreda et al. (2008) dataset","This dataset provides global maps of harvested area fraction for over 150 crops. Need to select atleast one host"),#shinyBS::tipify(el = div("Mofreda",icon(name = "info-circle", lib = "font-awesome")), title = ),
                                             choices = c(param_hosts$options[[1]]$choices),
                                             multiple = T)
                                 ),
                          column(3,
                                 radioButtons("inp_mapspam_options",label = "MAPSPAM or IFPRI dataset",choices = c("Global 2010","Africa 2017"),inline = T),
                                 selectInput("inp_mapspam",label = NULL,choices = c(param_hosts$options[[2]]$choices),multiple = T)#param_hosts$options[[2]]$option
                                 # shinyBS::bsTooltip("inp_mapspam", "Info", trigger = "hover",
                                 #                    "right", options = list(container = "body"))
                                 ),
                          column(3,
                                 fileInput("inp_host_file","File")
                                 ),
                          column(3,style="height:14rem;overflow-y:auto;",
                                 div(
                                   h5(fcnAddInfo("Host Density Threshold","Selections have to unique and positive"),style="font-weight: bold;margin-right: 1rem;"),
                                   actionButton("inp_add_dt",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                                   actionButton("inp_remove_dt",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                                   style="display:inline-flex;"
                                 ),
                                 uiOutput("uio_add_dt")
                          )

      ),
      #Gegraphic##################
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Geographic Considerations",
                          column(4,
                                 checkboxGroupInput("inp_agg_strat",fcnAddInfo("Aggregation Strategy","Select atleast one option"),choices = c("sum","mean"))
                                 ),
                          column(4,
                                 shiny::radioButtons("inp_distance_strat",fcnAddInfo("Distance Strategy",""),choices = geohabnet::dist_methods())
                                 ),
                          column(2,
                                 numericInput("inp_resolution",fcnAddInfo("Spatial Resolution","Values should be between 1 and 48"),value=12),
                          ),
                          column(2,
                                 shinyBS::tipify(el = div(h5("Geographic Extent",style="font-weight: bold;margin-top: -1px;"),icon(name = "info-circle", lib = "font-awesome"),style="display:inline-flex;"), title = paste0(geohabnet::geoscale_param(),collapse = ";")),
                                 checkboxInput("inp_globalextent",label = "Global",value = TRUE),
                                 uiOutput("uio_globalextent_user")
                          )
      ),
      #Network#############
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Network Metrics and Dispersal Kernels",
             column(12,style="border-bottom: 1px solid lightgray;margin-bottom: 1rem;",
                    column(3,
                           h5(fcnAddInfo("Inverse Power Law Model","Select atleast one method. Sum of method(s) should be 100"),style="font-weight: bold"),
                           selectInput(inputId = "inp_ipl_dd",NULL,choices = param_metrics,selected = tolower(param_metrics_default$InversePowerLaw$metrics),multiple = T)
                    ),
                    column(9,
                           uiOutput("uio_create_ipl_input")
                    )
                    ),
             column(12,style="border-bottom: 1px solid lightgray;margin-bottom: 1rem;",
                    column(3,
                           h5(fcnAddInfo("Negative Exponential Model","Select atleast one method. Sum of method(s) should be 100"),style="font-weight: bold"),
                           selectInput(inputId = "inp_ne_dd",NULL,choices = param_metrics,selected = tolower(param_metrics_default$NegativeExponential$metrics),multiple = T)
                    ),
                    column(9,
                           uiOutput("uio_create_ne_input")
                    )
                    ),
             column(4,style="height:14rem;overflow-y:auto;",
                    div(
                      h5(fcnAddInfo("Link Weight Threshold","Inputs should be numeric, unique and positive."),style="font-weight: bold;margin-right: 1rem;"),
                      actionButton("inp_add_lt",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                      actionButton("inp_remove_lt",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                      style="display:inline-flex;"
                    ),
                    uiOutput("uio_add_lt")
             ),
             column(4,style="height:14rem;overflow-y:auto;",
                    h5(
                      fcnAddInfo(
                        "Dispersal Parameter Beta",
                        "Beta is a parameter used in the dispersal kernel based on the inverse power law model. The larger the beta, the more likely a pathogen or pest move from one location to another."),
                      style="font-weight: bold"),
                    div(
                      actionButton("inp_add_beta",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                      actionButton("inp_remove_beta",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                      style="display:inline-flex;"
                    ),
                    uiOutput("uio_create_ipl_beta_input")
             ),
             column(4,style="height:14rem;overflow-y:auto;",
                    h5(
                      fcnAddInfo(
                        "Dispersal Parameter Gamma",
                        "Gamma is a parameter used in the dispersal kernel based on the negative exponential model. The larger the gamma, the more likely a pathogen or pest move from one location to another."
                        )
                      ,style="font-weight: bold"),
                    div(
                      actionButton("inp_add_gamma",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                      actionButton("inp_remove_gamma",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                      style="display:inline-flex;"
                    ),
                    uiOutput("uio_create_ne_gamma_input")
             )


      ),
      #Output##################
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Output Selection",
                          #h5("Priority Maps",style="font-weight: bold"),
                          column(3,
                                fcnAddInfo(shinyFiles::shinyDirButton("inp_prioritymaps_1",names(param_prioritymaps_default)[1] ,
                                                title = "Please select a folder:", multiple = FALSE,
                                                buttonType = "default", class = NULL),
                                           "Choose the folder where user prefer to save the outputs.")
                                 #checkboxInput("inp_prioritymaps_1",label = ,value = param_prioritymaps_default[[1]])
                                 ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_2",label =fcnAddInfo("Mean Map",
                                                                                      "A map of the mean of habitat connectivity across all parameter combinations."
                                                                                      ),
                                                                                      value = param_prioritymaps_default[[2]])
                          ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_3",label = fcnAddInfo("Difference Map",
                                                                                       "A map of the difference in ranks between mean habitat connectivity and host density."
                                                                                       ),
                                               value = param_prioritymaps_default[[3]])
                          ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_4",label = fcnAddInfo("Variance Map",
                                                                                       "A map of the variance of habitat connectivity across all parameter combinations."
                                                                                       ),
                                               value = param_prioritymaps_default[[4]])
                          )
      ),
      shinydashboard::box(width = 12,collapsible = F,
                          column(1,offset =5,actionButton("inp_submit_all","Submit"))
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
  global_btn_cnt <- reactiveValues(cnt_dt = length(param_dt_values),cnt_lt = length(param_lt_values),cnt_beta = length(param_beta_values),cnt_gamma = length(param_gamma_values))
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
  # output$uio_create_ipl_beta_input <- renderUI({
  #   choices <- unlist(param_file_default$default$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta)
  #   fcn_add_numeric_input("inp_ipl_beta_",suffix = seq(1:length(choices)),choices,isLabel=F)
  # })
  output$uio_create_ne_input <- renderUI({
    req(input$inp_ne_dd)
    choices <- input$inp_ne_dd
    fcn_add_numeric_input("inp_ne_matrix_",choices,0,isLabel=T)
  })
  # output$uio_create_ne_gamma_input <- renderUI({
  #   choices <- unlist(param_file_default$default$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma)
  #   fcn_add_numeric_input("inp_ne_gamma_",suffix = seq(1:length(choices)),choices,isLabel=F)
  # })
  #Beta and gamma#######################
  observeEvent(input$inp_add_beta,{
    req(input$inp_add_beta)
    isolate(global_btn_cnt$cnt_beta <- global_btn_cnt$cnt_beta + 1)
  })
  observeEvent(input$inp_remove_beta,{
    req(input$inp_remove_beta)
    isolate(global_btn_cnt$cnt_beta <- global_btn_cnt$cnt_beta - 1)
  })
  output$uio_create_ipl_beta_input <- renderUI({
    # -1 for the first time run only
    if(global_btn_cnt$cnt_beta== -1){
      global_btn_cnt$cnt_beta <- length(param_dt_values)
    }
    if(global_btn_cnt$cnt_beta==0){
      global_btn_cnt$cnt_beta <- 1
    }
    cnt <- global_btn_cnt$cnt_beta
    fcn_add_numeric_input("inp_ipl_beta_",seq(1:cnt),param_beta_values)
  })
  observeEvent(input$inp_add_gamma,{
    req(input$inp_add_gamma)
    isolate(global_btn_cnt$cnt_gamma <- global_btn_cnt$cnt_gamma + 1)
  })
  observeEvent(input$inp_remove_gamma,{
    req(input$inp_remove_gamma)
    isolate(global_btn_cnt$cnt_gamma <- global_btn_cnt$cnt_gamma - 1)
  })
  output$uio_create_ne_gamma_input <- renderUI({
    # -1 for the first time run only
    if(global_btn_cnt$cnt_gamma== -1){
      global_btn_cnt$cnt_gamma <- length(param_dt_values)
    }
    if(global_btn_cnt$cnt_gamma==0){
      global_btn_cnt$cnt_gamma <- 1
    }
    cnt <- global_btn_cnt$cnt_gamma
    fcn_add_numeric_input("inp_ne_gamma_",seq(1:cnt),param_gamma_values)
  })
  fcnValidate <- function(obj){
    if(any(is.null(obj)))return(T)
    if(any(is.na(obj)))return(T)
    if(length(obj)==0)return(T)
    if(length(unique(obj)) != length(obj))return(T)
    if(any(obj %in% F))return(T)
    if(any(obj %in% ""))return(T)
    if(any(obj %in% 0))return(T)
    if(any(obj < 0))return(T)
    return(F)
  }
  #Submit button###################
  observeEvent(input$inp_submit_all,{
    #sendSweetAlert(session = session,title = "Inputs submitted. Updating Parameters",type = "success")
    if("degree" %in% input$inp_ipl_dd || "degree" %in% input$inp_ne_dd){
      shinybusy::notify_info("We are still updating'degree' functionality. Please remove it from selection",position = "center-bottom",timeout = 6000)#,config_notify(width='100rem')
      return()
    }
    geohabnet::reset_params()
    param_file <- geohabnet::get_parameters()
    def_yaml <- yaml::read_yaml(param_file)
    val_list <- list("Pass"=T,
                     "Issue" = list()
                     )
    #Host Considerations########
    #Host
    if(fcnValidate(input$inp_mofreda) && fcnValidate(input$inp_mapspam)){
      val_list$Issue <- c(val_list$Issue,list("Host"="Please select one host"))
    }else{
      if(!fcnValidate(input$inp_mofreda)){
        inp <- input$inp_mofreda
        def_yaml$default$`CCRI parameters`$Hosts$monfreda <- inp

      }
      if(!fcnValidate(input$inp_mapspam)){
        inp <- input$inp_mapspam
        if(input$inp_mapspam_options=="Global 2010"){
          def_yaml$default$`CCRI parameters`$Hosts$mapspam2010 <- inp
        }else{
          def_yaml$default$`CCRI parameters`$Hosts$mapspam2017Africa <- inp
        }
      }
    }

    #2 Density Threshold#
    inp <- NULL
    for(i in 1:global_btn_cnt$cnt_dt){
      inp <- c(inp,input[[paste0("inp_density_threshold_",i)]])
    }
    if(fcnValidate(inp)){
      val_list$Issue <- c(val_list$Issue,list("Density Threshold"="Incorrect Input"))
    }else{
      def_yaml$default$`CCRI parameters`$HostDensityThreshold <- inp
    }

    #3 Link Threshold#
    inp <- NULL
    for(i in 1:global_btn_cnt$cnt_lt){
      inp <- c(inp,input[[paste0("inp_link_threshold_",i)]])
    }
    if(fcnValidate(inp)){
      val_list$Issue <- c(val_list$Issue,list("Link Threshold"="Incorrect Input"))
    }else{
      def_yaml$default$`CCRI parameters`$LinkThreshold <- inp
    }


    #4 Aggregation Strategy
    if(fcnValidate(input$inp_agg_strat)){
      val_list$Issue <- c(val_list$Issue,list("Aggregation Strategy"="Incorrect Input"))
    }
    else{
      inp <- input$inp_agg_strat
      def_yaml$default$`CCRI parameters`$AggregationStrategy <- inp
    }


    #5 Distance Strategy
    if(fcnValidate(input$inp_distance_strat)){
      val_list$Issue <- c(val_list$Issue,list("Distance Strategy"="Incorrect Input"))
    }
    else if(length(input$inp_distance_strat) !=1){
      val_list$Issue <- c(val_list$Issue,list("Distance Strategy"="Incorrect Input"))
    }else{
      inp <- input$inp_distance_strat
      def_yaml$default$`CCRI parameters`$DistanceStrategy <- inp
    }


    #6 Resolution
    if(fcnValidate(input$inp_resolution)){
      val_list$Issue <- c(val_list$Issue,list("Resolution"="Incorrect Input"))
    } else if(input$inp_resolution<1 && input$inp_resolution>48){
      val_list$Issue <- c(val_list$Issue,list("Resolution"="Incorrect Input"))
    }else{
      inp <- input$inp_resolution
      def_yaml$default$`CCRI parameters`$Resolution <- inp
    }

    #7 Global Extent
    inp <- input$inp_globalextent
    if(!inp){
      def_yaml$default$`CCRI parameters`$GeoExtent$global <- inp
      def_yaml$default$`CCRI parameters`$GeoExtent$customExt <-c(input$inp_geoscale_1,input$inp_geoscale_2,input$inp_geoscale_3,input$inp_geoscale_4)
    }

    #Network Metrics Inverse Power
    inp <- NULL
    if(fcnValidate(input$inp_ipl_dd)){
      val_list$Issue <- c(val_list$Issue,list("Inverse Power Law"="Incorrect Input"))
    }
    for(i in input$inp_ipl_dd){
      inp <- c(inp,input[[paste0("inp_ipl_matrix_",i)]])
    }
    if(sum(inp)!=100){
      val_list$Issue <- c(val_list$Issue,list("Inverse Power Law"="Incorrect Sum"))
    }
    def_yaml$default$`CCRI parameters`$NetworkMetrics$InversePowerLaw$metrics <- input$inp_ipl_dd
    def_yaml$default$`CCRI parameters`$NetworkMetrics$InversePowerLaw$weights <- inp

    #Network Metrics Negative Exponential
    inp <- NULL
    if(fcnValidate(input$inp_ne_dd)){
      val_list$Issue <- c(val_list$Issue,list("Network Metrics"="Incorrect Input"))
    }
    for(i in input$inp_ne_dd){
      inp <- c(inp,input[[paste0("inp_ne_matrix_",i)]])
    }
    if(sum(inp)!=100){
      val_list$Issue <- c(val_list$Issue,list("Network Metrics"="Incorrect Sum"))
    }
    def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$metrics <- input$inp_ne_dd
    def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$weights <- inp

    #Beta
    inp <- NULL
    for(i in 1:global_btn_cnt$cnt_beta){
      inp <- c(inp,input[[paste0("inp_ipl_beta_",i)]])
    }
    if(fcnValidate(inp)){
      val_list$Issue <- c(val_list$Issue,list("Beta"="Incorrect Input"))
    }else{
      inpaslist <- c()
      for(i in 1:global_btn_cnt$cnt_beta){
        inpaslist <- c(inpaslist,list(inp[i]))
      }
      def_yaml$default$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta <- inpaslist
    }

    #Gamma
    inp <- NULL
    for(i in 1:global_btn_cnt$cnt_gamma){
      inp <- c(inp,input[[paste0("inp_ne_gamma_",i)]])
    }
    if(fcnValidate(inp)){
      val_list$Issue <- c(val_list$Issue,list("Gamma"="Incorrect Input"))
    }else{
      inpaslist <- c()
      for(i in 1:global_btn_cnt$cnt_gamma){
        inpaslist <- c(inpaslist,list(inp[i]))
      }
      def_yaml$default$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma <- inpaslist
    }

    #Output
    # if(fcnValidate(input$inp_prioritymaps_2) && fcnValidate(input$inp_prioritymaps_3) && fcnValidate(input$inp_prioritymaps_4)){
    #   val_list$Issue <- c(val_list$Issue,list("Output Selection"="Incorrect Selection"))
    # }
    if(fcnValidate(input$inp_prioritymaps_2)){
      val_list$Issue <- c(val_list$Issue,list("Output Selection"="Please select Mean Map"))
    }
    def_yaml$default$`CCRI parameters`$PriorityMaps$MeanCC <- input$inp_prioritymaps_2
    def_yaml$default$`CCRI parameters`$PriorityMaps$Difference <- input$inp_prioritymaps_3
    def_yaml$default$`CCRI parameters`$PriorityMaps$Variance <- input$inp_prioritymaps_4

    if(length(val_list$Issue)>0){
      optext <- ""
      for(i in 1:length(val_list$Issue)){
        optext <- paste0(optext,names(val_list$Issue[i]),": ",val_list$Issue[i],"<br>")
      }
      showModal(modalDialog(
        title = "Invalid Input Selection",
        HTML(optext)
      ))
      return()
    }

    shinybusy::notify_success("Inputs have been validated. Performing Sensitivity Analysis. This may take some time...",position = "center-bottom",timeout = 6000)#,config_notify(width='100rem')
    yaml::write_yaml(def_yaml,param_file)
    geohabnet::set_parameters(param_file)
    #Sys.sleep(5)
    #sendSweetAlert(session = session,title = "Parameters Set. Generating Outputs",type = "success")
    shinybusy::show_modal_spinner() # show the modal window
    #shinybusy::play_gif()
    rv$mainop <- geohabnet::sensitivity_analysis()
    rv$mean <- input$inp_prioritymaps_2
    rv$diff <-input$inp_prioritymaps_3
    rv$var <-input$inp_prioritymaps_4
    #Sys.sleep(10)
    shinybusy::remove_modal_spinner() # show the modal window
    shinybusy::notify_success("Outputs have been generated!",position = "center-bottom",timeout = 6000)#,config_notify(width='100rem')
    shinydashboard::updateTabItems(session = session,"tabs","sm_3_genop")
    #shinybusy::play_gif()
  })

  rv <- reactiveValues(mainop = NULL,mean=F,diff=F,var=F,plotmean=NULL,plotdiff=NULL,plotvar=NULL)

  output$uio_sm_genop <- renderUI({
    if(is.null(rv$mainop)){
      div("Please run sensitivity analysis first")
    }else{
      shiny::fluidPage(
        column(12,style= "overflow-y:auto;",
               uiOutput("uiomean"),
               uiOutput("uiodiff"),
               uiOutput("uiovar")
        )
      )
    }
  })
  output$uiomean <-renderUI({
    if(isolate(rv$mean)==T){
      shinydashboard::box(width=6,h3("Mean Map"),
                          shiny::downloadButton('dwnmean',"Download"),
                          shiny::plotOutput("plotoutmean"))
    }else{
      div()
    }
  })
  output$uiodiff <-renderUI({
    if(isolate(rv$diff)==T){
      shinydashboard::box(width=6,h3("Difference Map"),
                          shiny::downloadButton('dwndiff',"Download"),
                          shiny::plotOutput("plotoutdiff"))
    }else{
      div()
    }
  })
  output$uiovar <-renderUI({
    if(isolate(rv$var)==T){
      shinydashboard::box(width=6,h3("Variance Map"),
                          shiny::downloadButton('dwnvar',"Download"),
                          shiny::plotOutput("plotoutvar"))
    }else{
      div()
    }
  })

  output$plotoutmean <- renderPlot({
    #browser()
    rv$plotmean<-geohabnet:::.plotmap(rv$mainop@me_rast,geoscale = geohabnet::geoscale_param(),isglobal = T,col_pal = geohabnet:::.get_palette_for_diffmap(),zlim = c(0, 0))
    # rv$plotmean = ggplotly(rv$plotmean) %>%
    #   config(
    #     modeBarButtonsToRemove = list(
    #       "zoom2d",
    #       "pan2d",
    #       "zoomIn2d",
    #       "zoomOut2d",
    #       "autoScale2d",
    #       "resetScale2d",
    #       "hoverClosestCartesian",
    #       "hoverCompareCartesian",
    #       "sendDataToCloud",
    #       "toggleHover",
    #       "resetViews",
    #       "toggleSpikelines",
    #       "resetViewMapbox"
    #     ),
    #     displaylogo = FALSE
    #   )
    # rv$plotmean
    #ggsave("plot.pdf", rv$plotmean)
    #rv$plotmean
  })
  # output$dwnmean <- downloadHandler(
  #   filename = function() {
  #     "plot.pdf"
  #   },
  #   content = function(file) {
  #     file.copy("plot.pdf", file, overwrite=TRUE)
  #   }
  # )
  output$plotoutdiff <- shiny::renderPlot({
    rv$plotdiff<-geohabnet:::.plotmap(rv$mainop@diff_rast,geoscale = geohabnet::geoscale_param(),isglobal = T,col_pal = geohabnet:::.get_palette_for_diffmap(),zlim = c(0, 0))
  })
  output$plotoutvar <- shiny::renderPlot({
    rv$plotvar<-geohabnet:::.plotmap(rv$mainop@var_rast,geoscale = geohabnet::geoscale_param(),isglobal = T,col_pal = geohabnet:::.get_palette_for_diffmap(),zlim = c(0, 0))
  })
  output$dwnmean <- downloadHandler(
    filename = function() { paste("mean_map", '.tif', sep='') },
    content = function(file) {
      file.copy(file.path(paste0(tempdir(),"\\plots\\",list.files(paste0(tempdir(),"\\plots"),pattern = "mean"))),file)
    }
  )
  output$dwndiff <- downloadHandler(
    filename = function() { paste("mean_map", '.tif', sep='') },
    content = function(file) {
      file.copy(file.path(paste0(tempdir(),"\\plots\\",list.files(paste0(tempdir(),"\\plots"),pattern = "diff"))),file)
    }
  )
  output$dwnvar <- downloadHandler(
    filename = function() { paste("mean_map", '.tif', sep='') },
    content = function(file) {
      file.copy(file.path(paste0(tempdir(),"\\plots\\",list.files(paste0(tempdir(),"\\plots"),pattern = "var"))),file)
    }
  )
}
