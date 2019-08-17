library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinyjs)
library(bubbles)

js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"



#UI---------------------------------------------------------------
  ui <- shinyUI(
    
    fluidPage(tags$style(HTML("
      .btn-default{color: #fff !important;
                   background-color: #428bca!important;
                   border-color: #428bca!important;}

      .col-sm-8 {min-height: 1px!important;
                 padding-right: 0px!important;
                 padding-left: 15px!important;
                 }

      .row {margin-right: 0px; 
            margin-left: 0px; 
      }
      #container * {  
                    display: inline;
                    }
      .form-control[disabled], .form-control[readonly], fieldset[disabled] .form-control {
                        background-color: #fff;
                        opacity: 1;}
      .title {margin-left:2%;}
      .title2 {margin-left:1%;}
       h2,h4,h5 {
           font-weight: bold;
             }"
                              )),
              useShinyjs(),
              extendShinyjs(text = js_code, functions = 'browseURL'),
              
      theme = "custom.css",
      navbarPage("",id="navibar",
                 tabPanel("Home",
                          tags$div(class="grid-container",
                            tags$div(class="item-1",
                                     tags$h1(tags$b("Ambiya Sang Aji"),style="margin-top:50%;margin-left:30%;font-family: 'Trebuchet MS'"),
                                     br(),
                                     tags$h1("Credit Card Default Prediction",style="margin-left:30%;font-family: 'Trebuchet MS'"),
                                     tags$h3("Predict Credit Card Default Based on Customer Historical Payment using 3 Algorithm, XGBtree, Decision Tree and Neural Network",style="margin-left:30%;font-family: 'Trebuchet MS';"),
                                     style="text-align: center; width:100%"
                            ),
                            tags$div(class="item-2",
                                     tags$img(src='CC4.png', height="514px", width = "80%"),
                                     style="text-align: right; width:100%"
                            ),
                            tags$div(class="item-3",
                                     tags$img(src='CC6.jpg', height="514px", width = "80%"),
                                     style="text-align: right; width:100%"
                            )
                          )
                 ),
                 tabPanel("Overview",
                          tags$div(class="grid-container2",
                                   tags$div(class="item-1",
                                            tags$img(src='SS1.png', height="300px", width = "27%", style="margin-top:3%;margin-left:3%"),
                                            tags$img(src='SS2.png', height="320px", width = "19%", style="margin-top:3%;margin-left:3%"),
                                            br(),
                                            tags$img(src='SS3.png', height="250px", width = "20%", style="margin-top:3%;margin-left:3%"),
                                            tags$img(src='SS4.png', height="440px", width = "23%", style="margin-top:6%;margin-left:3%"),
                                            tags$img(src='Arrow.png', height="200px", width = "35%", style="margin-top:6%;margin-left:3%")
                                            ),
                                   tags$div(class="item-2",
                                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                            tags$b("Default or Not?",style="margin-left:15%;color:#ffffff;font-size:3em"),
                                            tags$img(src='CCrem.png', height="400px", width = "400px", style="position:abolute;margin-left:-40%;"),
                                            style="background-color:#00b0f0;height:1028px"
                                            )
                                   )
                          ),
                 tabPanel("EDA",
                          fluidRow(
                            column(6, tags$h3(tags$b("Total Default based on Categorical Variable"), style="margin-top:25%"),style="text-align: center;"),
                            column(6,
                                   br(),
                                   plotlyOutput("plot10", width = "100%", height = "360px"),
                                   selectInput("slct10", label = h4("Select Variable"), 
                                               choices = list("SEX" = 1, "EDUCATION" = 2, "MARRIAGE" = 3, "PAYMENT M-6" = 4,
                                                              "PAYMENT M-5" = 5,"PAYMENT M-4" = 6,"PAYMENT M-3" = 7,"PAYMENT M-2" = 8,"PAYMENT M-1" = 9), 
                                               selected = 1)
                            )
                          ),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   plotlyOutput("plot20", width = "100%", height = "360px"),
                                   selectInput("slct20", label = h4("Select Variable"), 
                                               choices = list("AGE" = 1, "LIMIT BALANCE" = 2, "BILL&PAYMENT M-6" = 3, "BILL&PAYMENT M-5" = 4,
                                                              "BILL&PAYMENT M-4" = 5,"BILL&PAYMENT M-3" = 6,"BILL&PAYMENT M-2" = 7,"BILL&PAYMENT M-1" = 8
                                                              ), 
                                               selected = 1)
                            ),
                            column(6, tags$h3(tags$b("Range of Data from Various Variable based on Default Status"), style="margin-top:25%"),style="text-align: center;")),
                          tags$hr(),
                          fluidRow(
                            column(5, tags$h3(tags$b("Range Bill Amount and Payment over The Month"), style="margin-top:30%"),style="text-align: center;"),
                            column(7,
                                   plotlyOutput("plot30", width = "100%", height = "600px")
                            )
                          )
      
                          ),
                 tabPanel("Result",
                          tags$div(class="title",titlePanel("Test Your Data on Model")),
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              # Input: Select a file ----
                              fileInput("file1", "Upload Your Test File",
                                        multiple = FALSE,
                                        accept = c("csv",
                                                   "comma-separated-values,text",
                                                   ".csv")),
                              selectInput("slct100", label = h5("Select Algorithm"), 
                                          choices = list("XGBtree" = 1, "Decision Tree" = 2, "Neural Network" = 3), 
                                          selected = 1),
                              style="margin-left:3%"
                            ),
                            mainPanel(
                              tags$div(
                                tags$h3(tags$b(textOutput("names",inline = TRUE))),
                                plotlyOutput("plot3", width = "99%", height = "450px"),
                                br(),
                                column(width=4,wellPanel(id="well",span(id="container",h4("Sensitivity :"),h4(textOutput("sensitivityxgb",inline = TRUE))))), 
                                column(width=4,wellPanel(id="well",span(id="container",h4("Specitivity :"),h4(textOutput("specitivityxgb",inline = TRUE))))), 
                                column(width=4,wellPanel(id="well",span(id="container",h4("Balanced Accuracy :"),h4(textOutput("balaccxgb",inline = TRUE))))),
                                br(),
                                sliderInput("sldr3", "Select Cut-off",
                                            min = 0, max = 1,
                                            value = 0.5, step = 0.01, width = "99%"),
                                style="text-align:center"
                              )
                            )
                            )
                          ),
                 tabPanel("Predict",
                          tags$div(class="title2",titlePanel("Predict New Data")),
                          sidebarLayout(
                            sidebarPanel(
                                    fileInput("file2", "Upload a CSV File to Predict Default",
                                              multiple = FALSE,
                                              accept = c("csv",
                                              "comma-separated-values,text",
                                              ".csv")),
                                     selectInput("slct6", label = tags$b(h5("Select Algorithm")), 
                                                  choices = list("Decision Tree" = 1, "XgbTree" = 2, "Neural Net" = 3), 
                                                  selected = 2),
                                     numericInput("num6", label=tags$b(h5("Insert Cut-Off Point")), value="0.50", min = 0.1, max = 1, step = 0.01)
                                     ),
                            mainPanel(
                          dataTableOutput('contents',width="98%")
                            )
                          )
                        ),  
                 tabPanel("Variable Importance",
                          fluidRow(
                                   column(6,
                                      tags$h3(tags$b("XGBtree"),style ="margin-left:40%"),
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Bubble", bubblesOutput("bubs1", width = "100%", height = "750px")),
                                                  tabPanel("Bar", plotlyOutput("plotx", width = "100%", height = "750px"))
                                      )
                                   ),
                                   column(6,
                                      tags$h3(tags$b("Decision Tree"),style ="margin-left:40%"),
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Bubble", bubblesOutput("bubs2", width = "100%", height = "750px")),
                                                  tabPanel("Bar", plotlyOutput("ploty", width = "100%", height = "750px"))
                                      )
                                   )
                            )
                          ),
                 tabPanel("Code", value = "Code",
                          mainPanel(
                            tags$div(br(),br(),
                              tags$h1(tags$b("Redirect to Rpubs...")),
                              style="text-align: center; margin-left:40%"
                            )
                          )
                          )
      )
    )
  )
    