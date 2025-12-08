#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(yaml)
options(shiny.loadTimeout = 60000)
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB limit
app_server <- function(input, output, session) {


  #UI###################
  # Your application server logic
  param_file_default <- geohabnet::get_parameters()
  print(param_file_default)
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

  #Crop Choices
  global_crop_choices <- list(
    choices = c("Mapspam","Cropgrid","Earthstat","Others"),
    preprocess = c("Mapspam","Cropgrid") #Ordering Matters
  )


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
  #fcnAddInfo <- function(label,info){
  #  return(shinyBS::tipify(el = div(label,icon(name = "info-circle", lib = "font-awesome")), title = info))
  #}

  fcnAddInfo <- function(label, info) {
    tags$span(
      HTML(as.character(label)),
      tags$i(
        class = "fa fa-info-circle",
        `data-toggle` = "tooltip",
        title = as.character(info),
        style = "margin-left: 6px; cursor: help;"
      )
    )
  }


  output$uio_sm_1_dashboard <- renderUI({
    fluidPage(style="background-color:white",
          tags$head(
            tags$style(
              HTML(
                "
        body {
          background-image: url('www/farmland.jpg');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          height: 100vh;
          margin: 0;
          padding: 0;
          font-family: Arial, sans-serif;
        }
        .container {
                display: flex;
                align-items: center;
                justify-content: space-between;
                padding: 20px;
            }
            .text {
                flex: 1;
                padding-right: 20px;
            }
            .image {
                flex: 1;
                position: relative;
                overflow: hidden;
            }
            .image img {
                width: 100%;
                height: auto;
            }
            .overlay {
                position: absolute;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background: linear-gradient(to left, rgba(255, 255, 255, 0) 0%, rgba(255, 255, 255, 1) 100%);
            }
        "
              )
            )
          ),
         fluidRow(
                              div(class = "container",
                                  div(class = "text",
                                      h2("Welcome"),
                                      h4(
                                        "Welcome to the Geohabnetify app! This app expedites the functionality of geohabnet through an easy-to-use interactive dashboard. The geohabnet package helps users to conduct habitat connectivity analyses in a reproducible fashion, which is key to understanding the potential spread of plant pathogens, plant pests, pollinators, or endangered species. These tools are part of the",
                                        tags$a("R2M Plant Health Toolbox: Rapid risk assessment for mitigation of crop pathogens & pests.", href = "https://www.garrettlab.com/r2m/", target = "_blank")
                                        )
                                  ),
                                  div(class = "image",
                                      img(src = "www/farmland.jpg", alt = "Your Image"),
                                      div(class = "overlay")
                                  )
                              )
                              #h4("Welcome to geohabnet Dashboard. Our tool empowers you to analyze the network and connectivity of croplands, crucial for understanding the potential spread of plant pathogens. While geographical connection is significant, numerous other factors influence spread and connectivity, such as crop type and various environmental parameters.")
                              ),
         h3("Key Features",style="text-align: center;"),
         shinydashboard::box(
                             width = 4,
                             height = 300,
                             style = "text-align: center;",
                             img(src = "www/customize.png", width = 80),
                             h4(
                               tags$b("Customizable Parameters:"),
                               "Users can adjust up to 10 parameters including habitat landscapes, dispersal models, and geographic considerations"
                               )
           ),
          shinydashboard::box(width =4,
                              height = 300,
                              style="text-align: center;",
                              img(src = "www/international.png",width=80),
                              h4(tags$b("Global Perspective:"),
                                 "The geohabnet package (and its Geohabnetify) leverages publicly available global databases for the geographic distribution of habitat availability, such as global maps of host plants for plant pathogens and pests. Users can also provide their own maps of habitat distribution."
                                 )
                              ),
          shinydashboard::box(width =4,style="text-align: center;",
                              height = 300,
                              img(src = "www/snap.png",width=60),
                              h4(tags$b("User-Friendly Interface:"),
                                 "Inspired by configuration-based design in software development (Majors 2022), our RShiny interface offers intuitive control over parameter values, streamlining your analysis of habitat connectivity"
                                 )
                              ),
          shinydashboard::box(width = 12,style="text-align: center;",
                              h3("Example Output of the Host Landscape Connectivity for the Tomato Leafminer",style="text-align: center;"),
                              img(src = "www/map.jpg", width = 700)
          ),
          shinydashboard::box(width = 12,#style="text-align: center;",
                              h3("geohabnet: Geographical Risk Analysis Based on Habitat Connectivity",style="text-align: center;"),
                              h4(
                                p(
                                  "The geohabnet package is designed to perform a geographically or spatially explicit risk analysis of habitat connectivity.",
                                  tags$a("Xing et al (2021)",href="https://academic.oup.com/bioscience/article/70/9/744/5875255",target="_blank"),
                                  "proposed the concept of cropland connectivity as a risk factor for plant pathogen or pest invasions. As the functions in geohabnet were initially developed thinking on cropland connectivity, users are recommended to first be familiar with the concept by looking at the Xing et al paper. In a nutshell, a habitat connectivity analysis combines information from maps of habitat availability (e.g., host density), estimates the relative likelihood of pest movement between habitat locations in the area of interest, and applies network analysis to calculate the connectivity of habitat locations."
                                )
                              ),
                              h4(
                                p(
                                  "The functions of geohabnet are built to conduct a habitat connectivity analysis relying on geographic parameters (spatial resolution and spatial extent), dispersal parameters (in two commonly used dispersal kernels: inverse power law and negative exponential models), and network parameters (link weight thresholds and network metrics)."
                                )
                              ),
                              h4(
                                p(
                                  "More information about the stable version of geohabnet can be found at ",
                                  tags$a("CRAN: Package geohabnet", href = "https://cran.r-project.org/web/packages/geohabnet/index.html", target = "_blank"),
                                  ". More information about the development version of geohabnet can be found at ",
                                  tags$a("GitHub - GarrettLab/HabitatConnectivity: geohabnet R package", href = "https://github.com/GarrettLab/HabitatConnectivity", target = "_blank"),
                                  ". You are welcome to contribute to speed up and broaden the functionality of this package."
                                )
                              )
          ),
         column(12,
         tags$footer(
                 style = "
          color: #333;
          padding: 15px 0;
          font-size: 14px;
          width: 100%;
          text-align: center;
          border-top: 1px solid #ddd;
          position: relative;
          bottom: 0;
        ",

                 fluidRow(
                   column(
                     12,
                     tags$div(
                       style = "margin-bottom: 5px;",
                       tags$b("Stavan Shah"), " - Developer ",
                       tags$a("stavannikhi.shah@ufl.edu",
                              href = "mailto:stavannikhi.shah@ufl.edu",
                              style = "color:#0073e6; text-decoration:none;"),
                       tags$br(),
                       tags$b("Aaron Plex"), " - Maintainer ",
                       tags$a("plexaaron@ufl.edu",
                              href = "mailto:plexaaron@ufl.edu",
                              style = "color:#0073e6; text-decoration:none;"),
                       tags$br(),
                       tags$b("Karen Garrett"), " - Lead Instructor ",
                       tags$a("karengarrett@ufl.edu",
                              href = "mailto:karengarrett@ufl.edu",
                              style = "color:#0073e6; text-decoration:none;")
                     ),
                     tags$div(
                       style = "margin-top: 8px; color: #555;",
                       HTML("&copy; University of Florida - Copyright holder, funder")
                     )
                   )
                 )
         ) #footer ends
         )


        )
  })

  output$uio_sm_2_custinp <- renderUI({
    fluidPage(

      #Host Considerations#########
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Habitat Considerations",
                          column(12,
                                 h5("geohabnet can provide analyses of habitat connectivity based on data you provide. It can also evaluate cropland connectivity analyses for crop-specific pathogens and pests based on Monfreda et al. (2008) and MapSPAM data sets. ")
                                 ),
                          shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = div(icon(name = "info-circle", lib = "font-awesome")," Links & Instructions for downloading data*"),
                             column(12,
                                    h5("*Note that a valid input data for geohabnet is a raster layer of habitat availability (such as host availability), in which each grid cell has any values between zero and one. Users can use the publicly available data sources listed below to conduct the habitat connectivity analysis, but these raster layers may need to be transformed before uploading them in the 'Upload File' button.")
                                    ),
                             column(12,
                               tags$div(
                                 style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 15px;",

                                 tags$div(
                                   tags$i(class = "fa fa-database", style = "margin-right: 6px; color: #4a4a4a;"),
                                   tags$a("Monfreda",
                                          href = "http://www.earthstat.org/harvested-area-yield-175-crops/",
                                          target = "_blank",
                                          style = "text-decoration: none; color: #0073e6; font-weight: 500;")
                                 ),

                                 tags$div(
                                   tags$i(class = "fa fa-database", style = "margin-right: 6px; color: #4a4a4a;"),
                                   tags$a("Mapspam",
                                          href = "https://dataverse.harvard.edu/file.xhtml?fileId=10120889&version=3.0",
                                          target = "_blank",
                                          style = "text-decoration: none; color: #0073e6; font-weight: 500;")
                                 ),

                                 tags$div(
                                   tags$i(class = "fa fa-database", style = "margin-right: 6px; color: #4a4a4a;"),
                                   tags$a("Cropgrid",
                                          href = "https://figshare.com/articles/dataset/CROPGRIDS/22491997/9",
                                          target = "_blank",
                                          style = "text-decoration: none; color: #0073e6; font-weight: 500;")
                                 )
                               )
                             ),
                            column(12,
                                   h5("1. If you are using data about the area fraction of a crop from the EARTHSAT dataset, you can directly upload that raster layer in this Shinny App.")
                            ),
                            column(12,
                                   h5("2. If you are using the data layers of harvested area (in hectares) from CROPGRIDS, you will need to run the following code in R studio before uploading it in this Shinny App")
                            ),
                            column(12,
                                   textAreaInput(
                                     inputId = "cropgrid_calculation_output_1",
                                     label = NULL,#"Copy and run the script below before uploading Cropgrid data",
                                     value =
                                       "
                                        library(terra)
                                        avocado_sp <- rast(\"CROPGRIDSv1.08_avocado.nc\")
                                        cell.area <- (0.05 * 111111) * (0.05 * 111111) / 10000  # area in hectares
                                        avocado_sp <- avocado_sp$harvarea / cell.area  # area in hectares
                                        values(avocado_sp) <- ifelse(values(avocado_sp) > 0,values(avocado_sp), NaN)
                                        writeRaster(avocado_sp, \"avocado_density.tif\", overwrite = TRUE)",
                                     rows = 3,
                                     width = "100%"
                                   )
                          ),
                          column(12,
                                 h5("3. If you are using the data layers of cropland area (in hectares) from MapSPAM, you will need to run the following code in R studio")
                          ),
                          column(12,
                                 textAreaInput(
                                   inputId = "cropgrid_calculation_output_2",
                                   label = NULL,#"Copy and run the script below before uploading Cropgrid data",
                                   value =
                                     "
                                      library(terra)
                                      terra_obj <- rast(\"spam2020_v1r0_global_H_BANA_A.tif\")
                                      cell.area <- (res(terra_obj)[1] * 111111) * (res(terra_obj)[2] * 111111) / 10000  # area in hectares
                                      terra_sp <- terra_sp / cell.area  # area in hectares
                                      writeRaster(terra_sp, \"terra_density.tif\", overwrite = TRUE)",
                                   rows = 3,
                                   width = "100%"
                                 )
                          ),
                          column(12,
                                 h5("4. If you are using your own dataset, please also make sure that you raster layer is in the standard coordinate reference system (i.e., EPSG:4326).")
                          )
                          ),
                          shinydashboard::box(width = 9,collapsible = F,collapsed = F, title = div(icon(name = "info-circle", lib = "font-awesome"),"Upload your own file with a map of habitat quality"),
                                      column(4,style="border-right: 1px solid lightgray;",
                                             h5(fcnAddInfo("Type","Add Info here"),style="font-weight: bold"),
                                             selectInput(inputId = "inp_select_file_type",NULL,choices = global_crop_choices$choices,selected = NULL,multiple = F),
                                             div(
                                               #style = "margin-top:-2rem",
                                               checkboxInput("inp_check_preprocess_host_file",fcnAddInfo("Pre-process the Data","Works only for CROPGRIDS and MAPSPAM"),value=TRUE)
                                             )
                                      ),
                                      column(8,
                                             fileInput(
                                               inputId = "inp_host_file",
                                               label = "Upload File",
                                               accept = c(
                                                 ".tif", ".tiff",   # GeoTIFF
                                                 ".img",            # ERDAS Imagine files
                                                 ".nc",             # NetCDF
                                                 ".grd", ".gri"     # Raster formats used by terra/raster
                                               )
                                             )#,

                                      )
                          ),
                          column(3,style="height:14rem;overflow-y:auto;",
                                 div(
                                   h5(fcnAddInfo("Habitat Density Threshold","Selections have to be unique and positive"),style="font-weight: bold;margin-right: 1rem;"),
                                   actionButton("inp_add_dt",NULL,icon = icon("plus", class = NULL, lib = "font-awesome")),
                                   actionButton("inp_remove_dt",NULL,icon = icon("minus", class = NULL, lib = "font-awesome")),
                                   style="display:inline-flex;"
                                 ),
                                 uiOutput("uio_add_dt")
                          )

      ),
      #Gegraphic##################
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Geographic Considerations",
                          column(12,
                                 h5("Select the geographic extent for the analysis, the spatial aggregation factor, the aggregation method (if aggregating data), and the method for evaluating the distance between locations")
                                 ),
                          column(2,
                                 fcnAddInfo(h5("Geographic Extent",style="font-weight: bold;margin-top: -1px;"),paste0("Deselect Global to input custom scales. Default Scale: ",paste0(geohabnet::geoscale_param(),collapse = ";"))),
                                 #shinyBS::tipify(el = div(h5("Geographic Extent",style="font-weight: bold;margin-top: -1px;"),icon(name = "info-circle", lib = "font-awesome"),style="display:inline-flex;"), title = paste0()),
                                 checkboxInput("inp_globalextent",label = "Global",value = TRUE),
                                 uiOutput("uio_globalextent_user")
                          ),
                          column(2,style="margin-left: 5rem;margin-right:8rem;",
                                 numericInput("inp_resolution",fcnAddInfo("Spatial Aggregation Factor","Values should be between 1 and 48"),value=12),
                          ),
                          column(4,
                                 checkboxGroupInput("inp_agg_strat",fcnAddInfo("Aggregation Strategy","Select at least one option"),choices = c("sum","mean"))
                                 ),
                          column(4,style ="margin-left: -13rem;",
                                 shiny::radioButtons("inp_distance_strat",fcnAddInfo("Distance Strategy",""),choices = geohabnet::dist_methods())
                                 ),

      ),
      #Dispersal#############
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Dispersal Kernels",
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
                                     "Beta is a parameter used in the dispersal kernel based on the inverse power law model. The smaller the beta, the more likely a pathogen or pest move from one location to another."),
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
                                     "Gamma is a parameter used in the dispersal kernel based on the negative exponential model. The smaller the gamma, the more likely a pathogen or pest move from one location to another."
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
      ####Network
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Network Metrics",
                          column(12,
                                 column(3,
                                        h5(fcnAddInfo("Metrics","Select at least one metric Sum of metric(s) should be 100"),style="font-weight: bold"),
                                        selectInput(inputId = "inp_ipl_dd",NULL,choices = param_metrics,selected = tolower(param_metrics_default$InversePowerLaw$metrics),multiple = T)
                                 ),
                                 column(9,
                                        uiOutput("uio_create_ipl_input")
                                 )
                                 )
      ),

      #Output##################
      shinydashboard::box(width = 12,collapsible = T,collapsed = T, title = "Output Selection",
                          #h5("Priority Maps",style="font-weight: bold"),
                          column(3,
                                fcnAddInfo(shinyFiles::shinyDirButton("inp_prioritymaps_1",names(param_prioritymaps_default)[1] ,
                                                title = "Please select a folder:", multiple = FALSE,
                                                buttonType = "default", class = NULL),
                                           "Choose the folder where user prefers to save the outputs.")
                                 #checkboxInput("inp_prioritymaps_1",label = ,value = param_prioritymaps_default[[1]])
                                 ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_2",label =fcnAddInfo("Map of Mean Habitat Connect",
                                                                                      "A map of the mean of habitat connectivity across all parameter combinations."
                                                                                      ),
                                                                                      value = param_prioritymaps_default[[2]])
                          ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_3",label = fcnAddInfo("Map of Difference Habitat Connect",
                                                                                       "A map of the difference in ranks between mean habitat connectivity and habitat density."
                                                                                       ),
                                               value = param_prioritymaps_default[[3]])
                          ),
                          column(3,
                                 checkboxInput("inp_prioritymaps_4",label = fcnAddInfo("Map of Variance Habitat Connect",
                                                                                       "A map of the variance of habitat connectivity across all parameter combinations."
                                                                                       ),
                                               value = param_prioritymaps_default[[4]])
                          ),
                          uiOutput("uiodisablemean")
      ),
      shinydashboard::box(width = 12,collapsible = F,
                          column(1,offset =5,actionButton("inp_submit_all","Submit"))
      )

    )

  })
  observe({
    if(!fcnValidate(input$inp_select_file_type)){
      if (input$inp_select_file_type %in% global_crop_choices$preprocess) {
        shinyjs::show("inp_check_preprocess_host_file")  # Show it first
        shinyjs::disable("inp_check_preprocess_host_file")
      } else {
        shinyjs::hide("inp_check_preprocess_host_file")

      }
    }
  })
  output$uiodisablemean <-renderUI({
    shinyjs::disable("inp_prioritymaps_2")
    div()
  })
  #volumes = c(Home = fs::path_home(), "C:" = "C:/", "D:" = "D:/")
  volumes <- c(
    Home = fs::path_home(),
    Temp = tempdir()
  )

  #shinyFiles::shinyDirChoose(input, "inp_prioritymaps_1", roots = volumes, session = session)
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
    shinyjs::disable("inp_mapspam")
    shinyjs::disable("inp_mofreda")
    shinyjs::disable("inp_monfreda_link")
    shinyjs::disable("inp_mapspam_link")
    shinyjs::disable("inp_cropgrid_link")
    shinyjs::disable("cropgrid_calculation_output_1")
    shinyjs::disable("cropgrid_calculation_output_2")




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
        numericInput("inp_geoscale_1","X Min",default_global[1]),
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
      shinybusy::notify_info("We are still updating 'degree' functionality. Please remove it from selection",position = "center-bottom",timeout = 6000)#,config_notify(width='100rem')
      return()
    }
    geohabnet::reset_params()
    param_file <- geohabnet::get_parameters()
    def_yaml <- yaml::read_yaml(param_file)
    val_list <- list("Pass"=T,
                     "Issue" = list()
                     )
    message("LOG -> location of param",param_file)
    #message("LOG -> paramfile", def_yaml)
    #Host Considerations########
    #Host
    if(fcnValidate(input$inp_mofreda) && fcnValidate(input$inp_mapspam)){
      #sS Temp Commenting for time being
      #val_list$Issue <- c(val_list$Issue,list("Habitat Considerations"="Please select one habitat"))
    }else{
      if(!fcnValidate(input$inp_mofreda)){
        inp <- input$inp_mofreda
        def_yaml$default$`CCRI parameters`$Host$monfreda <- inp

      }
      if(!fcnValidate(input$inp_mapspam)){
        inp <- input$inp_mapspam
        if(input$inp_mapspam_options=="Global 2010"){
          def_yaml$default$`CCRI parameters`$Host$mapspam2010 <- inp
        }else{
          def_yaml$default$`CCRI parameters`$Host$mapspam2017Africa <- inp
        }
      }
    }
    inp <-NULL
    #SS - May 20, File Input
    #SS - July 18th Updating This to compulsory preprocess
    message("----- [LOG] validate -----")
    inp_temp_var <<- NULL
    message("LOG value of ",input$inp_host_file)
    message("LOG value of ",input$inp_host_file$datapath)
    if(FALSE){#temp fcnValidate(input$inp_host_file$datapath)
      val_list$Issue <- c(val_list$Issue,list("Habitat Considerations"="Please upload a valid file."))
    }else{
      temp_loc <- input$inp_host_file$datapath
      if (input$inp_select_file_type == global_crop_choices$preprocess[1] && global_crop_choices$preprocess[1] == "Mapspam") {
        tryCatch({

          inp <- terra::rast(temp_loc)
          cell.area <- (terra::res(inp)[1] * 111111) * (terra::res(inp)[2] * 111111) / 10000  # area in hectares
          inp <- inp / cell.area
          terra::values(inp) <- ifelse(terra::values(inp) > 0, terra::values(inp), NaN)
          #inp <- ifelse(values(inp) > 0, values(inp), NaN)
          temp_loc <- file.path(tempdir(), "temp_write.tif")
          terra::writeRaster(inp, temp_loc, overwrite = TRUE)
          inp_temp_var <<- inp

        }, error = function(e) {
          print(e)
          val_list$Issue <<- c(val_list$Issue, list(
            "Habitat Considerations" = "Error in the automated pre-processing of uploaded file. Please refer to the example script and upload a processed file."
          ))
        })
      }
      else if (input$inp_select_file_type == global_crop_choices$preprocess[2] && global_crop_choices$preprocess[2] == "Cropgrid"){
        tryCatch({

          inp <- terra::rast(temp_loc)
          cell.area <- (0.05 * 111111) * (0.05 * 111111) / 10000  # area in hectares
          inp <- inp$harvarea / cell.area
          terra::values(inp) <- ifelse(terra::values(inp) > 0,
                                       terra::values(inp), NaN)
          temp_loc <- file.path(tempdir(), "temp_write.tif")
          inp_temp_var <<- inp
          terra::writeRaster(inp, temp_loc, overwrite = TRUE)

        }, error = function(e) {
          print(e)
          val_list$Issue <<- c(val_list$Issue, list(
            "Habitat Considerations" = "Error in the automated pre-processing of uploaded file. Please refer to the example script and upload a processed file."
          ))
        })
      }
      #Common for both if and else
      def_yaml$default$`CCRI parameters`$Host <-  temp_loc
      temp_loc <- NULL
    }

    message("----- [LOG] density threshold -----")
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
    message("----- [LOG] slink threshold -----")
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

    message("----- [LOG] aggregation strat -----")
    #4 Aggregation Strategy
    if(fcnValidate(input$inp_agg_strat)){
      val_list$Issue <- c(val_list$Issue,list("Aggregation Strategy"="Incorrect Input"))
    }
    else{
      inp <- input$inp_agg_strat
      def_yaml$default$`CCRI parameters`$AggregationStrategy <- inp
    }

    message("----- [LOG] distance strat -----")
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

    message("----- [LOG] resolution -----")
    #6 Resolution
    if(fcnValidate(input$inp_resolution)){
      val_list$Issue <- c(val_list$Issue,list("Resolution"="Incorrect Input"))
    } else if(input$inp_resolution<1 && input$inp_resolution>48){
      val_list$Issue <- c(val_list$Issue,list("Resolution"="Incorrect Input"))
    }else{
      inp <- input$inp_resolution
      def_yaml$default$`CCRI parameters`$Resolution <- inp
    }
    message("----- [LOG] global extent -----")
    #7 Global Extent
    inp <- input$inp_globalextent
    if(!inp){
      def_yaml$default$`CCRI parameters`$GeoExtent$global <- inp
      def_yaml$default$`CCRI parameters`$GeoExtent$customExt <-c(input$inp_geoscale_1,input$inp_geoscale_2,input$inp_geoscale_3,input$inp_geoscale_4)
    }
    message("----- [LOG]  network metrics-----")
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
    #Updated that same to same for negative exponential. (15th march)
    def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$metrics <- input$inp_ipl_dd
    def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$weights <- inp
    #Network Metrics Negative Exponential
    # inp <- NULL
    # if(fcnValidate(input$inp_ne_dd)){
    #   val_list$Issue <- c(val_list$Issue,list("Network Metrics"="Incorrect Input"))
    # }
    # for(i in input$inp_ne_dd){
    #   inp <- c(inp,input[[paste0("inp_ne_matrix_",i)]])
    # }
    # if(sum(inp)!=100){
    #   val_list$Issue <- c(val_list$Issue,list("Network Metrics"="Incorrect Sum"))
    # }
    #def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$metrics <- input$inp_ne_dd
    #def_yaml$default$`CCRI parameters`$NetworkMetrics$NegativeExponential$weights <- inp

    #Beta
    message("----- [LOG]  beta-----")
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
    message("----- [LOG] gamma -----")
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
    message("----- [LOG]  output-----")
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
    message("----- [LOG] validation complete -----")
    shinybusy::notify_success("Inputs have been validated. Performing Sensitivity Analysis. This may take some time...",position = "center-bottom",timeout = 6000)#,config_notify(width='100rem')


    #changing this
    #old
    ##yaml::write_yaml(def_yaml,param_file)
    ##geohabnet::set_parameters(param_file)
    #new

    print(inp_temp_var)
    # Create a temporary file to store the updated YAML
    tmp_yaml <- file.path(tempdir(), "user_param.yml")
    # Write the updated YAML to the temp file
    yaml::write_yaml(def_yaml, tmp_yaml)
    # Set parameters using the temp YAML file
    geohabnet::set_parameters(tmp_yaml)


    #Sys.sleep(5)
    #sendSweetAlert(session = session,title = "Parameters Set. Generating Outputs",type = "success")
    shinybusy::show_modal_spinner() # show the modal window
    #shinybusy::play_gif()

    message("LOG -> updated parameters", def_yaml)
    message("----- [LOG] sensitivity_analysis() -----")
    #temp fix
    #browser()
    #rv$mainop <-geohabnet::msean(rast=inp_temp_var,agg_methods="sum",res=24,inv_pl = geohabnet::inv_powerlaw(NULL, betas = c(0.5, 1, 1.5), mets = c("NODE_STRENGTH"), we = c(100), linkcutoff = -1),neg_exp = geohabnet::neg_expo(NULL, gammas = c(0.05, 1, 0.2, 0.3), mets = c("NODE_STRENGTH"), we = c(100), linkcutoff = -1))
    rv$mainop <- geohabnet::sensitivity_analysis()
    message("----- [LOG] analysis complete. generating outputs -----")

    message("LOG -> MAP OUTPUT(s)", input$inp_prioritymaps_2)
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
      shinydashboard::box(width=6,h3("Map of Mean Habitat Connect"),
                          shiny::downloadButton('dwnmean',"Download"),
                          shiny::plotOutput("plotoutmean"))
    }else{
      div()
    }
  })
  output$uiodiff <-renderUI({
    if(isolate(rv$diff)==T){
      shinydashboard::box(width=6,h3("Map of Difference Habitat Connect"),
                          shiny::downloadButton('dwndiff',"Download"),
                          shiny::plotOutput("plotoutdiff"))
    }else{
      div()
    }
  })
  output$uiovar <-renderUI({
    if(isolate(rv$var)==T){
      shinydashboard::box(width=6,h3("Map of Variance Habitat Connect"),
                          shiny::downloadButton('dwnvar',"Download"),
                          shiny::plotOutput("plotoutvar"))
    }else{
      div()
    }
  })
  #palette1 <- viridisLite::viridis(n=100, option = "inferno", direction = -1, begin = 0.05, end = 0.95)
  dark.palette<-viridis::viridis_pal(option = "inferno", begin = 0.05, end = 0.95)

  get_zlim <- function(r) {
    z <- terra::minmax(r)
    c(z[1], z[2])
  }

  output$plotoutmean <- renderPlot({
    req(rv$mainop@me_rast)
    message("PLOTTING mean raster")
    print(rv$mainop@me_rast)
    plot<-geohabnet:::.plotmap(
      rv$mainop@me_rast,
      geoscale = geohabnet::geoscale_param(),
      isglobal = TRUE,
      col_pal = dark.palette(100),
      zlim = c(0, 0)
    )
    plot
  })

  output$plotoutdiff <- renderPlot({
    req(rv$mainop@diff_rast)
    geohabnet:::.plotmap(
      rv$mainop@diff_rast,
      geoscale = geohabnet::geoscale_param(),
      isglobal = TRUE,
      col_pal = geohabnet:::.get_palette_for_diffmap(),
      zlim = get_zlim(rv$mainop@diff_rast)
    )
  })

  output$plotoutvar <- renderPlot({
    req(rv$mainop)

    message("DEBUG: entering plotoutvar, class = ", class(rv$mainop@var_rast))

    tryCatch({

      geohabnet:::.plotmap(
        rv$mainop@var_rast,
        geoscale = geohabnet::geoscale_param(),
        isglobal = TRUE,
        col_pal = dark.palette(100),
        zlim = get_zlim(rv$mainop@var_rast)
      )

      message("DEBUG: .plotmap finished successfully for var_rast")

    }, error = function(e) {
      message("ERROR in plotoutvar(): ", conditionMessage(e))
    })

  }, height = 600, res = 96)




  #######Download

  # Robust helper function to create download handlers
  create_download <- function(output, name, pattern) {
    output[[name]] <- downloadHandler(
      filename = function() { paste0(name, ".tif") },
      content = function(file) {
        plots_dir <- file.path(tempdir(), "plots")

        # List files matching the pattern
        files <- list.files(plots_dir, pattern = pattern, full.names = TRUE)

        if (length(files) == 0) {
          showNotification(paste("No files found for pattern:", pattern), type = "error")
          return(NULL)
        }

        # Pick the most recently modified file
        latest_file <- files[which.max(file.info(files)$mtime)]

        # Copy to download
        file.copy(latest_file, file)
      }
    )
  }

  # Create download handlers for your app
  create_download(output, "dwnmean", "mean")
  create_download(output, "dwndiff", "diff")
  create_download(output, "dwnvar", "var")

}
