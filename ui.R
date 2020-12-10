ui = fluidPage(
  useShinydashboard(),
    navbarPage(
          theme = "spacelab",  # <--- To use a theme, uncomment this
        "Covid-19 analysis", 
        tabPanel("Summary", icon = icon('map'),
                 fluidRow(
                     column(9,
                            h1(strong("Covid Incidence Rate in the US")),
                            h6("Representing hospitalization and death per million 
                               for each state as of 11/25/2020"))
                 ),
                 
                 sidebarPanel(
                     selectizeInput('selected',
                                    "Select Item to Display",
                                     choices = c('Hospitalization Rate (per million)' = 'hospitalizedPerMil', 
                                                 'Death Rate (per million)'='deathPerMil')
                                                ),
                     
                     dateInput("date", label = ("Select a Date: "), value = "2020-11-25"),
  
                     br(),
                     
                     h5("Top 3 states with highest rate:"),
                     h4(strong(htmlOutput("no1"))),
                     h4(strong(htmlOutput("no2"))),
                     h4(strong(htmlOutput("no3")))
                     
                    ),
              
                 mainPanel(
                     tabsetPanel(
                         tabPanel(htmlOutput("map")
                         )
                     )
                 ),

                 sidebarPanel(
                     selectizeInput('states',
                                    "Select a state or states to compare",
                                    choices_state,
                                    selected = "Alabama",
                                    multiple = TRUE),
                     selectizeInput('incidence',
                                    "Select Item to Display",
                                    choices = c('Hospitalization 
                                                 (per million)' = 'hospitalizedPerMil', 
                                                'Death (per million)' = 'deathPerMil')
                     ),
                     "Hover over the line for details", align ="center"),
             
                 mainPanel(plotlyOutput("state"))
        ),
        
        tabPanel("Pre-existing Conditions", icon = icon("plus", lib = "glyphicon"), 
                 fluidRow(
                     column(2,
                            br(),
                            fluidRow(column(3, verbatimTextOutput("value"))),
                            sliderInput(inputId = "h", "Date:", min =
                                          as.Date("2020-03-21"), max = as.Date("2020-11-25"),
                                        value = as.Date("2020-07-15"), width = "800px"),
                            br(),
                            br(),
           
                            radioButtons(inputId = "condition",
                                         label = "Select a condition:",
                                         choices = c("COPD",
                                                        "Hypertension",
                                                        "Obesity",
                                                        "Diabetes",
                                                        "Flu",
                                                        "Cancer" 
                                                        ),
                                         selected = "COPD"),
                            h6("Note: Double click on the state of interest to isolate its point on the graph.
                            Click another state(s) once for subsequent comparison."),
                            # h6("Flu denotes flu vaccination rate and Cancer denotes cancer mortality per 100K.")
                            ),
                     
                     column(5,
                            plotlyOutput("plot", height = 500)),
                
                     column(5, 
                            plotlyOutput("plot2", height = 500))
        ),
        
    br(),
    br(),
    
    fluidRow(

             valueBoxOutput("min"),

             valueBoxOutput("max"),

             valueBoxOutput("avg"))
            
      ),
    
    tabPanel("Correlation", icon = icon("stats", lib = "glyphicon"),
             tabsetPanel(
               tabPanel("Correlation map",
                        fluidRow(
                          column(10,
                                 plotOutput("cor", height = 450)),
                        ),
            
                        fluidRow(
                          br(),
                          br(),
                          br(),
                          br(),
                     
                          valueBox(0.34, "Obesity vs hosp", icon = icon("bar-chart"), width = 2),
                          valueBox(0.24, "Cancer mortality vs hosp", icon = icon("bar-chart"), width = 2),
                          valueBox(0.2, "COPD vs hosp", icon = icon("bar-chart"), width = 2),
                          valueBox(0.1, "Hypertension vs hosp", icon = icon("bar-chart"), width = 2),
                          valueBox(0.04, "Diabetes vs hosp", icon = icon("bar-chart"), width = 2),
                          valueBox(-0.05, "Flu vs hosp", icon = icon("bar-chart"), width = 2, color = "red"),
                          
                          valueBox(0.13, "Diabetes vs death", icon = icon("bar-chart"), width = 2),
                          valueBox(0.07, "Flu vs death", icon = icon("bar-chart"), width = 2),
                          valueBox(0.11, "Hypertension vs death", icon = icon("bar-chart"), width = 2),
                          valueBox(0.02, "COPD vs death", icon = icon("bar-chart"), width = 2),
                          valueBox(0.02, "Obesity vs death", icon = icon("bar-chart"), width = 2),
                          valueBox(0.017, "Cancer mortality vs death", icon = icon("bar-chart"), width = 2)
                          
                          )
                        ),
               
               tabPanel("Demographics",
                        fluidRow(
                          br(),
                          column(5, 
                                 plotlyOutput("pie")),
                          
                          column(7,
                                 plotlyOutput("ethnicity"))
                        ),
                        
                        fluidRow(
                          column(8,
                                 sidebarPanel(
                                   selectizeInput('state',
                                                  "Choose a state",
                                                  choices_death,
                                                  selected = "AL"))
                        ),
                        
                        column(8,
                               plotlyOutput("ethnicity2"))
                        )
             )
             
             # tabPanel("Demographic correlation",
             #          fluidRow(
             #            column(10,
             #                   plotOutput("demcor", height = 500)),
             #          ))

)),

      tabPanel("Data", icon = icon("database"),
               tabsetPanel(
                 tabPanel("Conditions",
                          DT::dataTableOutput("table")
                 ),
                 
                 tabPanel("Demographics",
                          DT::dataTableOutput("demo"))
               )
               
               )

)
)

