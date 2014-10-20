shinyUI(pageWithSidebar(
    headerPanel("Bacterial Growth Curve Analysis"),
    
    sidebarPanel(
        helpText("This app is designed to analyse bacterial growth curves, including interactive plotting of growth curves, estimating of curve parameters and comparing curve parameters of different groups."),
        tags$hr(),
        helpText("Choose tab-delimited file to upload, please refer to the", tags$a(href="https://github.com/mdphan/GrowthCurvesAnalysis_shinyApp/blob/master/sample_data.txt", "sample file"),  "for arrangment of raw data."),
        helpText("Note: The data provided should be blank corrected."),
        fileInput('file1', 'File: ',
                  accept=c('text/comma-separated-values,text/plain')),
        tags$hr(),
        h4(textOutput("caption1")),
        htmlOutput("overview"),
        tags$hr(),
        tags$p("Source code: ", tags$a(href="https://github.com/mdphan/GrowthCurvesAnalysis_shinyApp", "@mdphan/GrowthCurvesAnalysis_shinyApp/"))
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Graphs",
                     h4(textOutput("caption2")),
                     uiOutput("checklist"),
                     submitButton(text="Submit"),
                     plotOutput("gc_plot"),
                     tags$hr(),
                     downloadButton("dl_gc_plot","Download plot (.pdf)"),
                     value = 1),
            tabPanel("Parameter estimation",
                     h4(textOutput("caption3")),
                     tableOutput("param"),
                     tags$hr(),
                     tags$body(textOutput("note1")),
                     value = 2),
            tabPanel("Statistical Comparisons",
                     h4(textOutput("caption4")),                      
                     uiOutput("selectref"),
                     submitButton(text="Submit"),
                     plotOutput("ci_plot"),
                     tags$hr(),
                     tags$body(textOutput("note2")),
                     tags$hr(),
                     downloadButton("dl_ci_plot", "Download plot (.pdf)"),
                     value = 3),
            id="tabs1")
    )
))