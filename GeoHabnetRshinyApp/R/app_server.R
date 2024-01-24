#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  param_file_default <- geohabnet::get_parameters()
  param_file_default <- read_yaml(param_file_default)
  #First Paramater
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
  write_yaml(param_file_default,"data/default_param.yml")

  #Extracting CCRI parameters and their options


  #Input Tab##############
  output$uio_sm_2_custinp <- renderUI({
    fluidPage(
      h2("Select Inputs"),
      column(12,
             column(12,
                    uiOutput("uio_inputs")
             ),
             column(1,offset =11,actionButton("inp_submit_all","Submit"))
      )
    )
  })
  output$uio_inputs <- renderUI({
    fluidPage(
      #row 1
      column(3,
             selectInput("inp_mofreda",param_1$options[[1]]$option,choices = param_1$options[[1]]$choices),
             selectInput("inp_mapspam",param_1$options[[2]]$option,choices = param_1$options[[2]]$choices),
             fileInput("inp_host_file","File")
             #checkboxGroupInput("inp_mofreda","Monfreda",choices = c("avocado")),
             #checkboxGroupInput("inp_mapspam","Mapspam",choices = c("avocado","banana"))#,
             #selectInput("inp_hosts_selinp","Hosts",c(1,2))
      ),
      column(3,style="height:23rem;overflow-y:scroll;",
             div(
               h5("Density Threshold",style="font-weight: bold;margin-right: 1rem;"),
               actionButton("inp_add_dt","Add threshold"),
               style="display:inline-flex;"
             ),
             uiOutput("uio_add_dt")
      ),
      column(3,style="height:23rem;overflow-y:scroll;",
             div(
               h5("Link Threshold",style="font-weight: bold;margin-right: 1rem;"),
               actionButton("inp_add_lt","Add threshold"),
               style="display:inline-flex;"
             ),
             uiOutput("uio_add_lt")
      ),
      column(2,
             checkboxGroupInput("inp_agg_strat","Aggregation Strategy",choices = c("sum","mean")),
      # ),
      # #row 2
      # column(3,
             checkboxGroupInput("inp_distance_strat","Distance Strategy",choices = geohabnet::dist_methods())
      ),
      column(1,
             numericInput("inp_resolution","Resolution",value=12),
      # ),
      # column(2,
             h5("Geo Extnet",style="font-weight: bold"),
             checkboxInput("inp_globalextent",label = "Global",value = TRUE)
      ),
      column(12,
             h5("Network Metrics",style="font-weight: bold"),
             column(12,
                    h5("Inverse Power Law",style="font-weight: bold"),
                    column(3,numericInput("inp_netmet_ipl_bet","Betweeness",value = 50)),
                    column(3,numericInput("inp_netmet_ipl_ns","Node Strength",value = 15)),
                    column(3,numericInput("inp_netmet_ipl_nn","Sum of Nearest Neighbour",value = 15)),
                    column(3,numericInput("inp_netmet_ipl_evc","Eigen Vector Centrality",value = 20))
             ),
             column(12,
                    h5("Negative Exponential",style="font-weight: bold"),
                    column(3,numericInput("inp_netmet_ne_bet","Betweeness",value = 50)),
                    column(3,numericInput("inp_netmet_ne_ns","Node Strength",value = 15)),
                    column(3,numericInput("inp_netmet_ne_nn","Sum of Nearest Neighbour",value = 15)),
                    column(3,numericInput("inp_netmet_ne_evc","Eigen Vector Centrality",value = 20))
             )
      )

    )

  })
  fcn_add_numeric_input <- function(id,cnt,value){
    input_list <- lapply(1:cnt,function(i){
      inp_id <- paste0("id",i)
      numericInput(inp_id,label = NULL,value = value)
    })
  }
  global_btn_cnt <- reactiveValues(cnt_dt = 1,cnt_lt = 1)
  observeEvent(input$inp_add_dt,{
    req(input$inp_add_dt)
    isolate(global_btn_cnt$cnt_dt <- global_btn_cnt$cnt_dt + 1)
  })
  output$uio_add_dt <- renderUI({
    cnt <- global_btn_cnt$cnt_dt
    input_list <- fcn_add_numeric_input("inp_density_threshold_",cnt,0.00015)
    do.call(tagList,input_list)
  })
  observeEvent(input$inp_add_lt,{
    req(input$inp_add_lt)
    isolate(global_btn_cnt$cnt_lt <- global_btn_cnt$cnt_lt + 1)
  })
  output$uio_add_lt <- renderUI({
    cnt <- global_btn_cnt$cnt_lt
    input_list <- fcn_add_numeric_input("inp_link_threshold_",cnt,0.000001)
    do.call(tagList,input_list)
  })
  observeEvent(input$inp_submit_all,{
    sendSweetAlert(session = session,title = "Inputs submitted. Updating Parameters",type = "success")
    param_file <- geohabnet::get_parameters()
    def_yaml <- read_yaml("data/default_param.yml")
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

    write_yaml(def_yaml,param_file)
    set_parameters(param_file)
    Sys.sleep(5)
    sendSweetAlert(session = session,title = "Parameters Set. Generating Outputs",type = "success")

    sensitivity_analysis()

  })


}
