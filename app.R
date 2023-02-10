# topic modeling for MSOM
# JPL
# questions? email john.lalor@nd.edu 


# topic modeling code modified from https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html


#################### SETUP #########################
# Load required libraries

# utilities
library(datasets)
library(DT)
library(jsonlite)
library(plotly)
library(utf8)
library(stringi)
library(data.table)
library(shinybusy)

# tidyverse libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(reshape2)
library(stringr)
library(tidyr)

library(ggsci)

# shiny libraries
library(shiny)
library(gitlink)

# NLP libraries
library(hunspell)
library(lda)
library(quanteda)
library(quanteda.textstats)
library(tidytext)
library(tm)
library(topicmodels)
library(LaplacesDemon)

# network libraries
library(igraph)
library(networkD3)

# library for reading pubmed files
library(revtools)

# markdown
library(markdown)

# load the helper functions
source("utils.R")

set.seed(42)

# through iterative testing we found that
# removing these words from the token
# list improved the topic model outputs
stop_words_poms <- c(
  "health",
  "care",
  "healthcare",
  "data",
  "author",
  "abstract",
  "abstracts",
  "i.e.",
  "i.e",
  "copyright",
  "mis",
  "quarterly"
)

data_options <- c(
  "utd24.ris",
#  "informatics.nbib",
#  "medjournals.nbib",
  "subareaPubMedPull.nbib",
  "custom"
)

names(data_options) <- c(
  "UTD24 Journals",
#  "Health Informatics Journals",
#  "General Medical Journals",
  "Medical Journals by Subcategory",
  "Custom..."
)


# placeholders, this will be updated reactively
confYears <- 2007:2020
#journalList <- get_journals()
journalList <- c()
##########################################################


# This is all commented out for now, and will be removed
# these values will be set dynamically based on file input

# set some initial parameters (year range and custom stop words)


# pull the data once, then do the filtering reactively
#D <- get_merged_data()

#global_vocab <- D %>%
#  select(abstract) %>%
#  unnest_tokens(word, abstract) %>%
#  select(word) %>%
#  unique()

#global_vocab <- append(
#  as.list(global_vocab$word),
#  c("incentive",   "health-care", "user-system")
#)


######################## UI #######################
# UI code
ui <- fluidPage (
  theme = "app.css",
  add_busy_spinner(spin = "fading-circle", position="full-page"),
  tags$head(includeHTML(("google_analytics.html"))),
  tags$head(
           tags$style(".shiny-notification {position: fixed; top: 20%;left: 50%}
                            #selectedJournals2 .shiny-options-group{
                            -webkit-column-count: 3; /* Chrome, Safari, Opera */
                            -moz-column-count: 3;    /* Firefox */
                            column-count: 3;
                            -moz-column-fill: balanced;
                            -column-fill: balanced;
                            }
                            .checkbox{
                            margin-top: 0px !important;
                            -webkit-margin-after: 0px !important; 
                            }
                            ")),
  tags$style("input[type=checkbox] {
                    transform: scale(.8);
           }"),
  tags$style("#selectedJournals .shiny-options-group{
                    font-size:12px;
                    padding-top:10px;
           }"),
  #ribbon_css("https://github.com/nd-ball/msom-bah"),
  # title
  headerPanel(
    windowTitle="Exploring Business Analytics in Healthcare",
    HTML('<p>Exploring Business Analytics in Healthcare<a href="https://mendoza.nd.edu/" target="_blank"><img style="float:right;width:450px;" src="mendoza_gold_highres.png"/></a></p>')
  ),
  # sidebar will dynamically load relevant parameters
  sidebarPanel(
    selectInput(
      "fileselection",
      "Select a Dataset:",
      data_options
    ),
    conditionalPanel(
        condition = "input.fileselection == 'utd24.ris'",
        checkboxInput("useReviews",
                      "Include reviews and commentaries?",
                      value=TRUE
                      )
    ),
    conditionalPanel(
        condition = "input.fileselection == 'subareaPubMedPull.nbib'",
        actionButton("rerunModal", "Select different subcategories"),
    ),
    selectInput("start",
                "Start Year:",
                c(confYears),
                selected = 2007),
    selectInput("end",
                "End Year:",
                c(confYears),
                selected = 2020),
    sliderInput(
      "numTopics",
      "Number of Topics",
      3,
      min = 2,
      max = 10
    ),
    #conditionalPanel(
    #    condition = "input.fileselection != 'subareaPubMedPull.nbib'",
    checkboxGroupInput(
      "selectedJournals",
      "Journals to include:",
      choices = journalList,
      selected = journalList
    #)
  ),
    strong(textOutput("docsLoaded")),
    actionButton("do", "Run Analyses"),
    width = 2
  ),
  mainPanel(
    # Output: Formatted text for caption ----
    width=10,
    h3(textOutput("caption")),
    tabsetPanel(
      id = "mainTabs",
      type = "tabs",
      tabPanel(
        value="about",
        "About",
        uiOutput("aboutPanel"),
        actionButton("showKeywords", "Details on Search Keywords")
        #img(src='grow-the-good.jpg', align = "right")
      ),
      tabPanel(
        value="instructions",
        "Instructions",
               uiOutput("instructionsPanel"),
               uiOutput("uploadIntro"),
               actionButton("show", "Show Upload Instructions"),
               tags$head(tags$style(".modal-dialog{ width:1000px}")),
              ),
      tabPanel(
        "Examples",
        uiOutput("example1Panel"),
        actionButton("eg1is1", "Run Example 1.1 (IS, 2007-2014)"),
        actionButton("eg1is2", "Run Example 1.2 (IS, 2015-2020)"),
        actionButton("eg1om1", "Run Example 1.3 (OM, 2007-2014)"),
        actionButton("eg1om2", "Run Example 1.4 (OM, 2015-2020)"),
        actionButton("eg1is3", "Run Example 1.5 (IS, 2007-2020)"),
        actionButton("eg1om3", "Run Example 1.6 (OM, 2007-2020)"),
        uiOutput("example2Panel"),
        actionButton("eg2", "Run Example 2.1 (IS)"),
        actionButton("eg2hitech", "Run Example 2.2 (OM)"),
        uiOutput("example3Panel"),
        actionButton("eg3", "Run Example 3"),
        uiOutput("example4Panel"),
        actionButton("eg4", "Run Example 4"),
        
      ),
      # tab 1: main topic model output
      tabPanel(
        value = "results",
        "Topic Model",
        plotlyOutput("topicPlot",
                     width = "100%",
                     height = "600px"),
          h3("Methods Analysis"),        
        conditionalPanel(
          condition = "input.fileselection == 'utd24.ris'",
          h5(
              "The plot below shows the average topic proportions for all articles associated with a specific method (listed on the y-axis)."
          ),
              plotlyOutput("methodsProp",
                           height="600px")
        ),
        conditionalPanel(
            condition = "input.fileselection != 'utd24.ris'",
            h5(
                "A methods analysis chart cannot be generated for this selection of journals because most journals do not require that the analysis methods be reported as keywords. At the moment we provide a methods analysis chart for the UTD24 dataset. We hope future enhancements of the IWA can provide this feature."
            )
        ),
        h3("Top articles for each topic"),
        h5("See the instructions tab for more details."),
      DT::dataTableOutput("topDocs")
                   #width="100%",
                   #height="100%")
      ),
      # tab 2: trend analysis
      tabPanel(
        "Trend Analysis",
        # plot of topic distributions over time
        uiOutput("trendPanel"),
        uiOutput("graphControls"),
        uiOutput("trendPanel2"),
        checkboxInput("trendLine", "Display topic proportions as line graphs", FALSE),
        plotlyOutput("trendPlot"),
        uiOutput("trendPanel3"),
        conditionalPanel(
            condition = "input.fileselection != 'custom'",
            uiOutput("trendPanel4")
        ),
        plotlyOutput("nByYearPlot")
      ),
      # tab 3: abstract topic scores
      tabPanel(
        "Abstract Scores",
        h3(textOutput("abstractTitle")),
        textOutput("abstractInstructions"),
        uiOutput("abstractControls"),
        # for a selected topic, show top abstracts
        DT::dataTableOutput("topicAbstracts")
      ),
      # tab 4: network analysis
      # HIDE FOR NOW
      
       tabPanel(
         "Network Analysis",
         # co-occurance network for tags
         h3(textOutput("networkTitle")),
         uiOutput("networkInstructions"),
         sliderInput(
           "obs",
           "Number of keywords to include:",
           min = 5,
           max = 100,
           value = 20
         ),
         actionButton("graphReset", "Reset Network"),
         forceNetworkOutput("networkPlot",
                            height = "600px"),
         h3(textOutput("networkTableCaption")),
         DT::dataTableOutput("graphPapers")
       ),
      tabPanel(
        "Advanced Options",
        h3(textOutput("advancedTitle")),
        uiOutput("advancedInstructions"),
        # TODO: different options in here
        # 
        sliderInput(
          "nTokens",
          "Number of keywords to show per topic:",
          min = 5,
          max = 15,
          value = 10
        ),
        textInput('txt','Add exclusion words here:','')
        ,actionButton('add','add')
        ,actionButton('reset','reset')
        ,verbatimTextOutput('list'),
      textInput('txt2','Add merge words here:','')
      ,actionButton('add2','add')
      ,actionButton('reset2','reset')
      ,verbatimTextOutput('list2'),
      sliderInput(
        "alpha",
        "Alpha parameter for topic model:",
        min = 0.1,
        max = 10,
        value = 0.2
      ),
      h4('Check the box below to use keywords instead of titles for network analysis'),
      checkboxInput("networkKeywords", "Use keywords for network analysis instead of titles", FALSE),
    ),
    tabPanel(
      "Resources",
      h3("Resources"),
      uiOutput("resourceInstructions"),
      DT::dataTableOutput("dataSources")
    ),
    )
    
  )
)


server <- function(input, output, session) {
    options(shiny.maxRequestSize=40*1024^2)
  # set up a reactive value to store start and end date
  v <- reactiveValues(
    start = NULL,
    end = NULL,
    numTopics = NULL,
    source = NA,
    target = NA,
    D = NA,
    alpha = NULL,
    useReviews = TRUE,
    selectedJournals2 = NA
    #global_vocab = NA
  )

  # create a default merge word list that includes key phrases  
  # and merge words from our figure

    examplesMergeWordList <- c(
        "analytical model",
        "empirical model",
        "theoretical formulation",
        "simulation analysis",
        "machine learning",
        "grounded theory",
        "case study",
        "case studies",
        "queueing model",
        "game theory",
        "game theoretic",
        "bayesian modeling",
        "natural language processing",
        "sequence analysis",
        "regression model",
#        "regression modeling",
        "causal analysis",
        "field experiment",
        "field experiments",
        "instrumental variable",
        "bayesian regression modeling",
        "linear programming",
        "random forest",
        "dynamic programming"
    )

    mergeListEx1 <- c(
         "operations management", 
        "operations research", 
        "big data", 
        "information technology", 
        "information systems", 
        "health informatics"
    )
    
  
  myValues <- reactiveValues()
  observe({
    if(input$add > 0){
      myValues$dList <- c(isolate(myValues$dList), isolate(input$txt))
    }
  })
  observe({
    if(input$add2 > 0){
      myValues$mList <- c(isolate(myValues$mList), isolate(input$txt2))
    }
  })
  
  observe({
    if(input$reset > 0){
      myValues$dList <- c()
    }
  })
  observe({
    if(input$reset2 > 0){
      myValues$mList <- c()
    }
  })
  
  output$list<-renderPrint({
    myValues$dList
  })
  output$list2<-renderPrint({
    myValues$mList
  })
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      fileInput(
        "dataset",
        "Choose NBIB, BIB, or RIS File",
        multiple = FALSE,
        accept = c(".ris", ".nbib", ".bib")
      ),
      if (failed)
        div(tags$b("Invalid file", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }

    specialtyModal <- function(failed=FALSE){
        specialtyCategories <- read_csv(
            "specialtyCategories.csv"
        ) %>%
            arrange(specialty) %>%
            pull(specialty) %>%
            unique()
        
        modalDialog(
        if (failed)
          div(tags$b("Select between 1 and 5 subcategories", style = "color: red;")),

      checkboxGroupInput(
          "selectedJournals2",
          "Select subcategories (max 5)",
        choices = specialtyCategories,
        selected = v$selectedJournals2,
        inline=FALSE,
        width='90%'
      ),
      footer = tagList(
          modalButton("Cancel"),
          actionButton("ok2", "OK")
          )
        )
    }
    my_max <- 5
    my_min <- 1
    
#    observe({
#      if(length(input$selectedJournals2) > my_max){
#        updateCheckboxGroupInput(session, "selectedJournals2", selected= tail(input$selectedJournals2,my_max))
#      }
#      if(length(input$selectedJournals2) < my_min){
#        updateCheckboxGroupInput(session, "selectedJournals2", selected= "Addiction")
#      }
#    })

    getFname <- function(specialty){
        return(str_glue("specialties/{specialty}.csv"))
        }
        
    observeEvent(input$ok2, {
        if(length(input$selectedJournals2) <= my_max && length(input$selectedJournals2) >= my_min){
            removeModal()
            v$selectedJournals2 <- input$selectedJournals2
        specialtyCategories <- read_csv(
            "specialtyCategories.csv"
        ) %>%
            filter(specialty %in% v$selectedJournals2) %>%
            pull(journal)

        fileList <- lapply(v$selectedJournals2, getFname)
        if(length(fileList) == 1){
          v$D <- read_specialties(fileList[[1]])
        } else{
          v$D <- read_specialties(fileList)
        }
        v$selectedJournals <- specialtyCategories

        updateCheckboxGroupInput(
      session,
      inputId = "selectedJournals",
      choices = v$selectedJournals,
      selected = v$selectedJournals
      )
        }
        else{
      showModal(specialtyModal(failed = TRUE))            
        }   
    }
    )
    
    
      
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
                                        # message.

    observeEvent(input$rerunModal, {
        showModal(specialtyModal())
    }
    )
    
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    #print(input$dataset)
    if (!is.null(input$dataset) && nzchar(input$dataset)) {
      v$D <- read_upload(input$dataset$datapath)
      removeModal()
      updateSelectInput(
        session,
        inputId = "start",
        choices = sort(unique(v$D$yr)),
        selected = min(v$D$yr)
      )
      updateSelectInput(
        session,
        inputId = "end",
        choices = sort(unique(v$D$yr)),
        selected = max(v$D$yr)
      )
      updateCheckboxGroupInput(
        session,
        inputId = "selectedJournals",
        choices = sort(unique(v$D$journal)),
        selected = sort(unique(v$D$journal))
      )
      
      
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  observeEvent(input$fileselection,{
    # if the user selected a custom file, show a popup to load it
    updateTabsetPanel(session, "mainTabs",
                      selected = "about")
    if (input$fileselection == "custom"){
      #print("TRUE")
      showModal(dataModal())
    }
    else if (input$fileselection == "subareaPubMedPull.nbib"){
        showModal(specialtyModal())
    } 

    else{
    # read in file
    v$D <- read_upload(input$fileselection)
    }
    # update the options based on the input
    updateSelectInput(
      session,
      inputId = "start",
      choices = sort(unique(v$D$yr)),
      selected = min(v$D$yr)
    )
    updateSelectInput(
      session,
      inputId = "end",
      choices = sort(unique(v$D$yr)),
      selected = max(v$D$yr)
    )
    
    updateCheckboxGroupInput(
      session,
      inputId = "selectedJournals",
      choices = sort(unique(v$D$journal)),
      selected = sort(unique(v$D$journal))
    )
   
    
    
  })
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Custom Upload Instructions",
      includeMarkdown("data/upload_help.md")
    ))
  })

  observeEvent(input$showKeywords, {
    showModal(modalDialog(
      title = "Show Search Keywords",
      includeMarkdown("data/keyword_list.md")
    ))
  })

  # after the button is pushed, update values
  observeEvent(input$do, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    
    v$start <- input$start
    v$end <- input$end
    v$numTopics <- input$numTopics
    v$selectedJournals <- input$selectedJournals
    v$alpha <- input$alpha
    v$useReviews <- input$useReviews
    
  })
  
  # buttons for running the examples
  observeEvent(input$eg4, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    v$start <- 2007
    v$end <- 2020
    v$numTopics <- 4
    v$selectedJournals <- sort(unique(v$D$journal))
    v$alpha = 0.2
    myValues$mList <- examplesMergeWordList
    myValues$dList <- c()
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
  })
  
  observeEvent(input$eg1is1, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    j <- c(
      "Information Systems Research",
      "Journal of Management Information Systems",
      "MIS Quarterly",
      "Management Science"
    )
    v$start <- 2007
    v$end <- 2014
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
        mergeListEx1
    )    
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg1is2, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    j <- c(
      "Information Systems Research",
      "Journal of Management Information Systems",
      "MIS Quarterly",
      "Management Science"
    )
    v$start <- 2015
    v$end <- 2020
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
        mergeListEx1
    )
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg1is3, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    j <- c(
      "Information Systems Research",
      "Journal of Management Information Systems",
      "MIS Quarterly",
      "Management Science"
    )
    v$start <- 2007
    v$end <- 2020
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
        mergeListEx1
    )
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg1om1, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    j <- c(
      "Journal of Operations Management",
      "Manufacturing & Service Operations Management",
      "Operations Research",
      "Production & Operations Management"
    )
    v$start <- 2007
    v$end <- 2014
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
        mergeListEx1
    )
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg1om2, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    j <- c(
      "Journal of Operations Management",
      "Manufacturing & Service Operations Management",
      "Operations Research",
      "Production & Operations Management"
    )
    v$start <- 2015
    v$end <- 2020
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
        mergeListEx1
    )
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg1om3, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    v$D <- read_upload("utd24.ris")
    
    j <- c(
      "Journal of Operations Management",
      "Manufacturing & Service Operations Management",
      "Operations Research",
      "Production & Operations Management"
    )
    v$start <- 2007
    v$end <- 2020
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
        mergeListEx1
    )
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg2, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    j <- c(
      "Information Systems Research",
      "Journal of Management Information Systems",
      "MIS Quarterly"
    )
    v$D <- read_upload("utd24.ris")
    
    v$start <- 2007
    v$end <- 2020
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- examplesMergeWordList
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg2hitech, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    j <- c(
      "Journal of Operations Management",
      "Manufacturing & Service Operations Management",
      "Operations Research",
      "Production & Operations Management",
      "Management Science"
    )
    
    v$D <- read_upload("utd24.ris")
    
    v$start <- 2007
    v$end <- 2020
    v$numTopics <- 6
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- examplesMergeWordList
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  observeEvent(input$eg3, {
    updateTabsetPanel(session, "mainTabs",
                      selected = "results")
    j <- c(
      "Academy of Management Journal",
      "Information Systems Research",
      "Journal of Consumer Research",
      "Journal of Management Information Systems",
      "Journal of Marketing",
      "Journal of Marketing Research",
      "Journal of Operations Management",
      "Management Science",
      "Manufacturing & Service Operations Management",
      "Marketing Science",
      "MIS Quarterly",
      "Operations Research",
      "Organization Science",
      "Production & Operations Management"
    )
    v$D <- read_upload("utd24.ris")
    
    v$start <- 2007
    v$end <- 2020
    v$numTopics <- 10
    v$selectedJournals <- j
    v$alpha = 0.2
    myValues$mList <- c(
        examplesMergeWordList,
      "big data", 
      "data analytics", 
      "information systems", 
      "information technology", 
      "electronic health record", 
      "electronic medical record", 
      "operations research", 
      "operations management", 
      "medical informatics", 
      "health informatics"
    )
    myValues$dList <- c()
    
    updateSelectInput(session, "start", selected = v$start)
    updateSelectInput(session, "end", selected = v$end)
    updateSliderInput(session, "numTopics", value = v$numTopics)
    updateCheckboxGroupInput(session, "selectedJournals", selected = v$selectedJournals)
    v$useReviews = FALSE
    updateCheckboxInput(session, "useReviews", value = v$useReviews)
    
  })
  
  # after the button is pushed, update values
  observeEvent(input$graphReset, {
    updateNumericInput(session, "obs", value = 20)
    
    v$source <- NA
    v$target <- NA
  })
  
  
  # render some UI elements
  output$abstractControls <- renderUI({
    selectInput("selectedTopic",
                "Select a topic/topics:",
                1:v$numTopics,
                selected = 1:v$numTopics,
                multiple=TRUE)
  })
  
  output$graphControls <- renderUI({
    selectInput("selectedJournalsGraph",
                "Select a journal/journals:",
                v$selectedJournals,
                selected = v$selectedJournals,
                multiple=TRUE,
                width="80%")
  })
  
  output$abstractTitle <- renderText({
    if (is.null(v$start))
      return()
    
    "Top Abstracts for Selected Topic"
  })
  
  
  output$abstractInstructions <- renderText({
    if (is.null(v$start))
      return()
    
    "If you want to restrict the topics that are displayed, you can add/delete topics in the text box below. Click within the box and then hit your delete key to delete a topic. Once deleted, you can add a topic back in by selecting it from the dropdown."
  })
  
  output$instructionsPanel <- renderUI({
    # load instructions
    includeMarkdown("data/app_instructions.md")
  })
  
  
  output$resourceInstructions <- renderUI({
    # load instructions
    includeMarkdown("data/app_resources.md")
  })
  
  
  output$advancedInstructions <- renderUI({
    # load instructions
    includeMarkdown("data/advanced_instructions.md")
  })
  
  output$trendPanel <- renderUI({
    # load instructions
    includeMarkdown("data/app_trend.md")
  })
  
  output$trendPanel2 <- renderUI({
    # load instructions
    includeMarkdown("data/app_trend2.md")
  })

  output$trendPanel3 <- renderUI({
    # load instructions
    includeMarkdown("data/app_trend3.md")
  })

  output$trendPanel4 <- renderUI({
    # load instructions
    includeMarkdown("data/app_trend4.md")
  })

  output$aboutPanel <- renderUI({
    # load instructions
    includeMarkdown("data/app_about.md")
  })
  
  output$example1Panel <- renderUI({
    # load instructions
    #"TODO"
    includeMarkdown("data/app_example1.md")
  })

  output$example2Panel <- renderUI({
    # load instructions
    #"TODO"
    includeMarkdown("data/app_example2.md")
  })

  output$example3Panel <- renderUI({
    # load instructions
    #"TODO"
    includeMarkdown("data/app_example3.md")
  })
  
  output$example4Panel <- renderUI({
    # load instructions
    #"TODO"
    includeMarkdown("data/app_example4.md")
  })
  
  
  output$uploadPanel <- renderUI({
    # load upload instructions
    includeMarkdown("data/upload_help.md")
  })
  
  output$uploadIntro <- renderUI({
    # load upload instructions
    includeMarkdown("data/upload_intro.md")
  })
  
  
  output$networkTitle <- renderText({
    if (is.null(v$start))
      return()
    
    "Network Analysis"
  })
  
  output$networkInstructions <- renderText({
    if (is.null(v$start))
      return()
    
    includeMarkdown("data/app_network.md")
    
  })
  
  
  
  formulaText <- reactive({
    DR <-
      confYears[confYears >= as.numeric(v$start) &
                  confYears <= as.numeric(v$end)]
    paste("Topic Model for ", v$start, " - ", v$end)
  })
  
  
  
  networkText <- reactive({
    if (input$graphLinks$source == input$graphLinks$target) {
      paste("Papers for keyword:", input$graphLinks$source)
    }
    else{
      paste(
        "Papers for keyword link:",
        input$graphLinks$source,
        " - ",
        input$graphLinks$target
      )
    }
  })
  
  output$caption <- renderText({
    if (is.null(v$start))
      return()
    
    formulaText()
  })
  
  docsLoadedText <- reactive({
    num_abstracts <- v$D %>%
        filter((str_detect(replace_na(tags, ""), "shiny", negate=TRUE)) | (input$useReviews)) %>%
        filter(yr >= as.numeric(input$start)) %>%
        filter(yr <= as.numeric(input$end)) %>%
        filter(journal %in% input$selectedJournals) 
    
    str_glue("Number of abstracts loaded: {length(num_abstracts$journal)}")
  })
  output$docsLoaded <- renderText({
    docsLoadedText()
  })
  
  output$networkTableCaption <- renderText({
    if (is.null(input$graphLinks$source))
      return()
    
    v$source <- input$graphLinks$source
    v$target <- input$graphLinks$target
    networkText()
    
  })
  
  topicData <- reactive({
      topicData <- v$D %>%
      filter((str_detect(replace_na(tags, ""), "shiny", negate=TRUE)) | (input$useReviews)) %>%
      filter(yr >= as.numeric(v$start)) %>%
      filter(yr <= as.numeric(v$end)) %>%
      filter(journal %in% v$selectedJournals)
    
    if(!is.null(myValues$mList)){
      mergedList <- str_replace(myValues$mList, " ", "-") 
      names(mergedList) <- myValues$mList
    } else{
      mergedList <- c(" " = " ")
    }

    if (!is.null(input$file1)) {
      uploadedData <- read_upload(input$file1$datapath)
      topicData <- topicData %>%
        bind_rows(uploadedData)
    }

      print(mergedList)
    
    shiny::validate(
      need(
        length(topicData$title) > 10,
        "Not enough papers in the selected subset. Please select additional journals or expand the year range."
      )
    )
    
    # update our keyword phrases
    topicData <- topicData %>%
      mutate(abstract=str_to_lower(abstract))%>%
      mutate(
        abstract = 
      str_replace_all(
        abstract,
        c(
          "business analytics" = "business-analytics",
          "machine learning" = "machine-learning",
          "deep learning" = "deep-learning",
          "clinical decision support system" = "clinical-decision-support-system",
          "hospital decision support" = "hospital-decision-support",
          "healthcare decision support" = "healthcare-decision-support",
          "covid-19"="covid19"
        )
      )
      ) %>%
      mutate(
        abstract = 
          str_replace_all(
            abstract,
            mergedList
        )
      ) %>%
      mutate(title=str_to_lower(title))%>%
      mutate(
        title = 
          str_replace_all(
            title,
            c(
              "business analytics" = "business-analytics",
              "machine learning" = "machine-learning",
              "deep learning" = "deep-learning",
              "clinical decision support system" = "clinical-decision-support-system",
              "hospital decision support" = "hospital-decision-support",
              "healthcare decision support" = "healthcare-decision-support",
              "covid-19"="covid19"
            )
          )
      ) %>%
      mutate(
        title = 
          str_replace_all(
            title,
            mergedList
          )
      ) %>%
      mutate(title=str_to_title(title))
    
    if(input$networkKeywords){
      topicData <- topicData %>%
        mutate(
          networkKeywords = tags
        )
    } else{
      topicData <- topicData %>%
        mutate(
          networkKeywords = title
        )
    }
    
    global_vocab <- topicData %>%
      select(abstract) %>%
      unnest_tokens(word, abstract) %>%
      select(word) %>%
      unique()
    
    global_vocab <- append(
      as.list(global_vocab$word),
      c(
        "incentive",   "health-care", "user-system", 
        "decision-making", "probable", "mammography", 
        "web-based", "uncertainty", "primary", "operations-management",
        "no-show", "trade-off",
        "big-data", myValues$mList)
    )

    corpus <- corpus(topicData$abstract,
                     docnames = topicData$id)
    corpus_tokens <- corpus %>%
      tokens(
        remove_punct = T,
        remove_numbers = T,
        remove_symbols = T
      ) %>%
      tokens_tolower()%>%
      tokens_remove(stop_words$word, padding = T) %>%
      tokens_remove(stop_words_poms) %>%
      tokens_wordstem() %>%
      tokens_remove(myValues$dList) 
    
    abstracts_dtm <- corpus_tokens %>%
      tokens_remove("") %>%
      dfm() %>%
      dfm_wordstem() %>%
      dfm_remove(SnowballC::wordStem(myValues$dList)) %>%
      dfm_trim(
        min_docfreq = 0.05,
        max_docfreq = 0.15,
        docfreq_type = "prop"
      )
    
    sel_idx <- rowSums(abstracts_dtm) > 0
    topicData[sel_idx, ]
    
  })
  
  topicModel <- reactive({
    topicData <- topicData()
    
    global_vocab <- topicData %>%
      select(abstract) %>%
      unnest_tokens(word, abstract) %>%
      select(word) %>%
      unique()
    
    global_vocab <- append(
      as.list(global_vocab$word),
      c(
        "incentive",   "health-care", "user-system", 
        "decision-making", "probable", "mammography", 
        "web-based", "uncertainty", "primary", "operations-management",
        "no-show", "trade-off",
        "big-data", myValues$mList)
    )
    
    corpus <- corpus(topicData$abstract,
                     docnames = topicData$id)
    corpus_tokens <- corpus %>%
      tokens(
        remove_punct = T,
        remove_numbers = T,
        remove_symbols = T
      ) %>%
      tokens_tolower() %>%
      tokens_remove(stop_words$word, padding = T) %>%
      tokens_remove(stop_words_poms) %>%
      tokens_wordstem() %>%
      tokens_remove(myValues$dList) 

    abstracts_dtm <- corpus_tokens %>%
      tokens_remove("") %>%
      dfm() %>%
      dfm_wordstem() %>%
      dfm_remove(SnowballC::wordStem(myValues$dList)) %>%
      dfm_trim(
        min_docfreq = 0.05,
        max_docfreq = 0.15,
        docfreq_type = "prop"
      )
    
    sel_idx <- rowSums(abstracts_dtm) > 0
    abstracts_dtm <- abstracts_dtm[sel_idx, ]
    topicData <- topicData[sel_idx, ]
    
    LDA(
      abstracts_dtm,
      k = v$numTopics,
      method="Gibbs",
      control = list(
        seed = 42,
        #alpha = 0.2
        alpha = as.numeric(v$alpha)
        #alpha = 50 / v$numtopics
        #delta = 0.2
      ))
  })
  
  output$topicPlot <- renderPlotly({
    if (is.null(v$start))
      return()
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.25, detail = "Loading data")
      
      abstracts_lda <- topicModel()
      incProgress(0.75, detail = "Generating plots")
      
      abstracts_lda_tidy <- tidy(abstracts_lda)
      
      topicData <- topicData()
      global_vocab <- topicData %>%
        select(abstract) %>%
        unnest_tokens(word, abstract) %>%
        select(word) %>%
        unique()
      
      global_vocab <- append(
        as.list(global_vocab$word),
        c(
          "incentive",   "health-care", "user-system", 
          "decision-making", "probable", "mammography", 
          "web-based", "uncertainty", "primary", "operations-management",
          "no-show", "trade-off",
          "big-data", myValues$mList)
      )


      
      
      abstracts_lda_tidy %>%
          group_by(topic) %>%
          arrange(topic, -beta) %>%
          mutate(rank=row_number(topic)) %>%
          write_csv("find_term.csv")
      
      top_terms <- abstracts_lda_tidy %>%
          group_by(topic) %>%
        top_n(input$nTokens, beta) %>%
        ungroup() %>%
        arrange(topic, -beta) 
      print(top_terms, n=100)
      top_terms <- top_terms %>%
        mutate(term = str_replace(term, "aci$", "acy")) %>%
        mutate(term = str_replace(term, "phi$", "phy")) %>%
        mutate(term = str_replace(term, "communiti$", "community")) %>%
        mutate(term = str_replace(term, "web-ba$", "web-based")) %>%
        mutate(term = str_replace(term, "uncertainti$", "uncertainty")) %>%
        mutate(term = str_replace(term, "primari$", "primary")) %>%
        mutate(term = str_replace(term, "operations-manag$", "operations-management")) %>%
        mutate(term = str_replace(term, "suppli$", "supply")) %>%
        mutate(term = stemCompletion(term, global_vocab))
      
      tmResult <- posterior(abstracts_lda)
#                            control = list(seed = 42, ))
      
      theta <- tmResult$topics
      topic_proportion <- colMeans(theta) * 100
      
      topic.labs <- as.data.frame(topic_proportion) %>%
        mutate(n = row_number()) %>%
        mutate(topic_proportion = round(topic_proportion, digits = 1)) %>%
        mutate(label = str_glue("Topic {n}: {topic_proportion}%")) %>%
        select(label)
      
      topic.labs <- unlist(as.list(topic.labs$label))
      
      names(topic.labs) <- c(1:v$numTopics)
      
      
      if (v$numTopics == 4) {
        ncols = 2
      }
      else{
        ncols = 3
      }
      
      p <- top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        mutate(topic = factor(topic)) %>%
        ggplot(aes(term, beta, fill = topic)) +
        geom_col(show.legend = FALSE, position = position_dodge(0.7)) +
        coord_flip() +
        facet_wrap(
          . ~ topic,
          ncol = ncols,
          scales = "free_y",
          shrink = FALSE,
          switch = "y",
          labeller = labeller(topic = topic.labs)
          #labeller = label_both
        ) +
        scale_x_reordered() +
        theme_bw() +
        scale_fill_jco() +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      incProgress(1, detail = "Wrapping up")
      
    })
    ggplotly(p)
  })
  
  
  output$trendPlot <- renderPlotly({
    if (is.null(v$start))
      return()
    abstracts_lda <- topicModel()
    topicData <- topicData()
    global_vocab <- topicData %>%
      select(abstract) %>%
      unnest_tokens(word, abstract) %>%
      select(word) %>%
      unique()
    
    global_vocab <- append(
      as.list(global_vocab$word),
      c(
        "incentive",   "health-care", "user-system", 
        "decision-making", "probable", "mammography", 
        "web-based", "uncertainty", "primary", "operations-management",
        "no-show", "trade-off",
        "big-data", myValues$mList)
    )
    
    tmResult <- posterior(abstracts_lda)
    #                          control = list(seed = 42, ))
    
    theta <- tmResult$topics
    beta <- tmResult$terms
    trendTokens <- terms(abstracts_lda, 5)
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "aci$", "acy")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "phi$", "phy")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "communiti$", "community")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "web-ba$", "web-based")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "uncertainti$", "uncertainty")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "primari$", "primary")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "operations-manag$", "operations-management")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "suppli$", "supply")
    trendTokens <-
      apply(trendTokens, c(1, 2), stemCompletion, global_vocab)
    
    topicNames <- apply(trendTokens, 2, paste, collapse = " ")
    
    for (i in 1:length(topicNames)) {
      topicNames[i] <- str_glue("Topic {i}: {topicNames[i]}")
    }
    
    
    topicData <- topicData()
    
    theta <- theta[topicData$journal %in% input$selectedJournalsGraph,]
    years <- topicData[topicData$journal %in% input$selectedJournalsGraph,]$yr
    topic_proportion_per_year <-
      aggregate(theta, by = list(year = years), mean)
    
    colnames(topic_proportion_per_year)[2:(v$numTopics + 1)] <-
      topicNames
    
    uniform.dist <- rep(1 / v$numTopics, v$numTopics)
    
    # reshape data frame
    vizDataFrame <- topic_proportion_per_year %>%
      rowwise() %>%
      mutate(k.l.d = KLD(c_across(starts_with("Topic")),
                         uniform.dist)$sum.KLD.px.py) %>%
      mutate(k.l.d = round(k.l.d, 3)) %>%
      ungroup() %>%
      pivot_longer(starts_with("Topic"),
                   names_to = "variable",
                   values_to = "value")
    
    #melt(topic_proportion_per_year, id.vars = "year")
    
    # plot topic proportions per decade as bar plot
    
    if(!input$trendLine){
    
    p <- vizDataFrame %>%
      ggplot(aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      ylab("Proportion") +
      scale_fill_jco() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme_bw() +
      labs(fill = "Topic") +
      scale_x_continuous(breaks = c(v$start:v$end)) +
      geom_text(aes(x = year, y = 1, label = k.l.d), color="black")
    }
    else{
      p <- vizDataFrame %>%
        ggplot(aes(x = year, y = value, color = variable)) +
        geom_line() +
        ylab("Proportion") +
        scale_color_jco() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme_bw() +
        labs(color = "Topic") +
        scale_x_continuous(breaks = c(v$start:v$end)) +
        geom_text(aes(x = year, y = 1, label = k.l.d), color="black")
    }
    
    ggplotly(p)%>%
  layout(legend = list(
             orientation = "h",
             y=-0.3
         )
         )
  })
  
  output$nByYearPlot <- renderPlotly({
    inData <- topicData()
    
    p <- inData %>%
      rename(year = yr) %>%
      filter(journal %in% input$selectedJournalsGraph) %>%
      group_by(journal, year) %>%
        summarise(count = n(), .groups="drop") %>%
        complete(journal, year=v$start:v$end, fill=list(count=0)) %>%
      ggplot(aes(x = year,
                 y = count,
                 color = journal)) +
      geom_line() +
      ylab("Count") +
      theme_bw() +
      labs(color="Journal") +
      scale_x_continuous(breaks = c(v$start:v$end))
    
    ggplotly(p) %>%
  layout(legend = list(
             orientation = "h",
             y=-0.3
    )
  )
  })


    output$methodsProp <- renderPlotly({
        if (is.null(v$start))
      return()
    abstracts_lda <- topicModel()
    topicData <- topicData()
    global_vocab <- topicData %>%
      select(abstract) %>%
      unnest_tokens(word, abstract) %>%
      select(word) %>%
      unique()
    
    global_vocab <- append(
      as.list(global_vocab$word),
      c(
        "incentive",   "health-care", "user-system", 
        "decision-making", "probable", "mammography", 
        "web-based", "uncertainty", "primary", "operations-management",
        "no-show", "trade-off",
        "big-data", myValues$mList)
    )
    
    tmResult <- posterior(abstracts_lda)
    #                          control = list(seed = 42, ))
    
    theta <- tmResult$topics
    beta <- tmResult$terms
    trendTokens <- terms(abstracts_lda, 5)
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "aci$", "acy")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "phi$", "phy")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "communiti$", "community")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "web-ba$", "web-based")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "uncertainti$", "uncertainty")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "primari$", "primary")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "operations-manag$", "operations-management")
    trendTokens <-
      apply(trendTokens, c(1, 2), str_replace, "suppli$", "supply")
    trendTokens <-
      apply(trendTokens, c(1, 2), stemCompletion, global_vocab)
    
    topicNames <- apply(trendTokens, 2, paste, collapse = " ")
    
    for (i in 1:length(topicNames)) {
      topicNames[i] <- str_glue("Topic {i}: {topicNames[i]}")
    }
    
    
        topicData <- topicData()

        topicData <- topicData %>%
        mutate(Method = str_extract(tags, "msom_.+?(and|$)")) %>%
        mutate(Method = str_sub(Method, 6)) %>%
            mutate(Method = str_replace(Method, "and", "")) #%>%
          #mutate(Method = str_replace(Method, "ML", "Machine-Learning"))
#keeping the previous line in the back pocket if we want to change it
        
        methods <- topicData$Method
        
    topic_proportion_per_year <-
      aggregate(theta, by = list(Method = methods), mean)
    
    colnames(topic_proportion_per_year)[2:(v$numTopics + 1)] <-
      topicNames
    
    # reshape data frame
    vizDataFrame <- topic_proportion_per_year %>%
      pivot_longer(starts_with("Topic"),
                   names_to = "variable",
                   values_to = "value") %>%
        mutate(Topic = as.numeric(str_extract(variable, "\\d+"))) %>%
        mutate(variable = as.factor(variable))

        # don't love it but a test
        if(length(unique(vizDataFrame$Topic)) == 10){
            oldLevels <- levels(vizDataFrame$variable)
            t10 <- oldLevels[2]
            vizDataFrame$variable <- factor(
                vizDataFrame$variable,
                levels = c(oldLevels[1], oldLevels[3:10], t10)
            )
        }
        
    
    # plot topic proportions per decade as bar plot
        fillLabels <- unique(vizDataFrame$variable)
        fillVals <- unique(vizDataFrame$Topic)
        p <- vizDataFrame %>%
            filter(Method != "NA") %>%
            filter(Method != "NA ") %>%
            ggplot(
                aes(
                    x = Method,
                    y = value,
                    fill = variable
                )
            ) +
            geom_bar(
                stat = "identity",
                position=position_fill(
                    reverse=TRUE
                )
            ) +
            ylab("Proportion") +
            xlab("Methods") +
            scale_fill_jco(
                name = "Topic"
            ) +
            theme(
                axis.text.x = element_text(
                    angle = 90,
                    hjust = 1
                )
            ) +
            theme_bw() +
            #labs(fill = "Topic") +
            scale_x_discrete(limits=rev) + 
            coord_flip()
        
    
    ggplotly(p)%>%
  layout(legend = list(
             orientation = "h",
             y=-0.3
         )
         )
    })
    


    output$topDocs <- DT::renderDataTable({
    if (is.null(v$start))
      return()
    topicData <- topicData()
    corpus <- corpus(topicData$abstract,
                     docnames = topicData$docid)
    
    selectedTopic <- 1:v$numTopics
    topicThreshold <- 0.1
    abstracts_lda <- topicModel()    
    tmResult <- posterior(abstracts_lda)
    theta <- tmResult$topics
    
    topicWeights <- as_tibble(theta[, selectedTopic])
    names(topicWeights) <- str_glue("Topic {selectedTopic} Top Scoring Articles")
    
    topicCorpus <- topicData %>%
        bind_cols(round(topicWeights, 4))
    print(names(topicCorpus))
          
    topicCorpus <- topicCorpus %>%
        pivot_longer(
            cols = starts_with("Topic"),
            names_to="TopicNum",
            values_to="weight"
        ) %>%
        arrange(TopicNum, -weight) %>%
        group_by(TopicNum) %>%
        top_n(5, weight) %>%
        mutate(rank=row_number()) %>%
        ungroup() 

      p <- topicCorpus %>%
        mutate(title = reorder_within(title, weight, TopicNum)) %>%
          mutate(TopicNum = factor(TopicNum)) %>%
          mutate(label=str_extract(title, "[^_]+"), 100) %>%
          mutate(Method = str_extract(tags, "msom_.+?(and|$)")) %>%
        mutate(Method = str_sub(Method, 6)) %>%
        mutate(Method = str_replace(Method, "and", "")) %>%
        mutate(Url = paste0(
          "<a href='",
          Url,
          "' target='_blank'>click for online access</a>"
        )) %>%
          select(
              Author,
              yr,
              label,
              TopicNum,
              Method,
              Url
          ) %>%
          rename(
              Year = yr,
              Title = label
          )

        if(input$fileselection != 'utd24.ris'){
            p <- p %>%
        select(-Method)
        }

        p
   },
  escape = FALSE,
  rownames=FALSE,
  plugins="ellipsis",
  extensions="RowGroup",
  options = list(
    ordering=F,
    search = list(regex = TRUE),
    rowGroup=list(dataSrc=c(3)),
    columnDefs = list(
        list(
            visible=FALSE,
            targets=c(3)
        ),
        list(
      targets = c(0),
      render = JS("$.fn.dataTable.render.ellipsis( 80, false )")
    ))
  )
  )
        

    output$topicAbstracts <- DT::renderDataTable({
    if (is.null(v$start))
      return()
    
    topicData <- topicData()
    corpus <- corpus(topicData$abstract,
                     docnames = topicData$docid)
    
    selectedTopic <- input$selectedTopic
    topicThreshold <- 0.1
    
    abstracts_lda <- topicModel()
    
    tmResult <- posterior(abstracts_lda)
    theta <- tmResult$topics
    
    topicWeights <- as_tibble(theta[, selectedTopic])
    names(topicWeights) <- str_glue("Topic {selectedTopic} Score")
    #print(topicData$tags)
    
    topicCorpus <- topicData %>%
      bind_cols(round(topicWeights, 4)) %>%
      #arrange(desc(topicWeight)) %>%
      mutate(Url = paste0(
        "<a href='",
        Url,
        "' target='_blank'>click for online access</a>"
        )) %>%
        mutate(Method = str_extract(tags, "msom_.+?(and|$)")) %>%
        mutate(Method = str_sub(Method, 6)) %>%
        mutate(Method = str_replace(Method, "and", "")) %>%
        select(-c(id, tags, networkKeywords)) %>%
        mutate(ID=row_number()) %>%
      relocate(ID, Author, title, yr, abstract, Method, journal, starts_with("Topic"), Url) %>%
      mutate(
        abstract = str_to_sentence(abstract)
      ) %>%
      rename(
        Title = title,
        Journal = journal,
        Abstract = abstract,
        `Paper Link` = Url,
        Year = yr
        #Score = topicWeight
      )

    if(input$fileselection != 'utd24.ris'){
    topicCorpus <- topicCorpus %>%
        select(-Method)
    }
    topicCorpus
  },
  escape = FALSE,
  rownames=FALSE,
  plugins="ellipsis",
  options = list(
      search = list(regex = TRUE),
      columnDefs = list(list(
          targets=c(1),
          render=JS("$.fn.dataTable.render.ellipsis(100,false)")
      ),
      list(
      targets = c(2,4),
      render = JS("$.fn.dataTable.render.ellipsis( 200, false )")
    ))
    )
  )
  
  output$dataSources <- DT::renderDataTable({
    D.sources <- readr::read_delim("data/data_sources.txt", delim="|")
    D.sources
  },
  escape = FALSE,
  plugins="ellipsis",
  options = list(
    search = list(regex = TRUE),
    # limit cells in columns 1 and 2 to 17 characters
    columnDefs = list(list(
      targets = c(2,3),
      render = JS("$.fn.dataTable.render.ellipsis( 200, false )")
    ))
  )
  )
  
  output$networkPlot <- renderForceNetwork({
    if (is.null(v$start))
      return()
    
    topicData <- topicData()
    create_cooccurance_matrix(
      topicData,
      v$start,
      v$end,
      v$selectedJournals,
      as.numeric(input$obs),
      v$source,
      v$target
    )
  })
  
  output$graphPapers <- DT::renderDataTable({
    if (is.null(input$graphLinks$source))
      return()
    D <- topicData()
    D %>%
      filter(yr >= as.numeric(v$start)) %>%
      filter(yr <= as.numeric(v$end)) %>%
      filter(journal %in% v$selectedJournals) %>%
      #mutate(tags = tolower(tags)) %>%
      filter(grepl(input$graphLinks$source, networkKeywords, ignore.case = TRUE)) %>%
      filter(grepl(input$graphLinks$target, networkKeywords, ignore.case = TRUE)) %>%
      mutate(Url = paste0("<a href='", Url, "' target='_blank'>Click here</a>")) %>%
      select(-c(tags, abstract, id, networkKeywords)) %>%
      relocate(Author, title, journal, yr, Url) %>%
      rename(
        Title = title,
        Journal = journal,
        Year = yr,
        `Paper Link` = Url
      )
    
  },
  escape = FALSE,
  options = list(dom = '<"top"flip>rt<"bottom"><"clear">'))
}


shinyApp(ui, server)
