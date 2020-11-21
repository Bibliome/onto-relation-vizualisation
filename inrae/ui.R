############################# LIBRARIES ################################

suppressMessages(library(sankeyD3))
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))

################################# UI ###################################

shinyUI(bootstrapPage(
    theme = "styles.css",
    useShinyjs(),
    
    #------------------------ SEARCH PANEL ----------------------------#
    div(
        id = "searchPanel", class = "panel",
        h1("Florilege Relations"),
        
        div(
            class = "conceptArea",
            selectInput(
                inputId = "cpt_A",
                choices = c("taxon", ontobiotope),
                label = "Concept A"
            ),
            selectizeInput(
                inputId = "root_A",
                label = "p",
                choices = NULL,
                multiple = FALSE,
                width = "100%",
                options = list(
                    placeholder = 'concept ID',
                    maxOptions = 5,
                    create = FALSE
                )
            ),
            selectizeInput(
                inputId = "list_A",
                label = NULL,
                choices = NULL,
                multiple = TRUE,
                width = "100%",
                options = list(
                    placeholder = 'list of concept',
                    maxOptions = 5,
                    create = FALSE
                )
            )
        ),
        
        div(
            class = "conceptArea",
            selectInput(
                inputId = "cpt_B",
                choices = rev(c("taxon", ontobiotope)),
                label = "Concept B"
            ),
            selectizeInput(
                inputId = "root_B",
                label = "p",
                choices = NULL,
                multiple = FALSE,
                width = "100%",
                options = list(
                    placeholder = 'concept ID',
                    maxOptions = 5,
                    create = FALSE
                )
            ),
            selectizeInput(
                inputId = "list_B",
                label = NULL,
                choices = NULL,
                multiple = TRUE,
                width = "100%",
                options = list(
                    placeholder = 'list of concept',
                    maxOptions = 5,
                    create = FALSE
                )
            )
        ),
        
        prettyToggle(
            inputId = "showfilters",
            label_on = "Hide filters",
            label_off = "Show filters",
            outline = TRUE,
            plain = TRUE,
            icon_on = icon("chevron-circle-down"),
            icon_off = icon("chevron-circle-up"),
            status_on = 'info',
            status_off = 'primary'
        ),
        conditionalPanel(
            condition = 'input.showfilters == true',
            class = "filterArea",
            div(
                class = "selectArea",
                selectInput(
                    inputId = "indicator",
                    label = "Indicator",
                    choices = c("doc", "relation"),
                    selected = "doc"
                ),
                
                selectInput(
                    inputId = "source",
                    label = "Source",
                    choices = list(
                        All = '', Pubmed = "PubMed",
                        CIRM = 'CIRM', DSMZ = 'DSMZ',
                        Genbank = 'GenBank'
                    ),
                    selected = ''
                ),
                
                selectInput(
                    inputId = "cpt_join",
                    label = "Join concept",
                    choices = c(ontobiotope, "taxon"),
                    selected = "habitat"
                )
            ),
            
            div(
                class = "switchArea",
                materialSwitch(
                    inputId = "qps",
                    label = "Qualified Presumption of Safety",
                    value = FALSE,
                    status = "primary"
                ),
                
                materialSwitch(
                    inputId = "second_rel",
                    label = "Show direct secondary relations",
                    value = FALSE,
                    status = "primary"
                )
            )
        ),

        actionButton("process", "Process")
        
    ),
    #--------------------------- DIAGRAM ------------------------------#
    div(
        id = "visuPanel", class = "panel",
        uiOutput("UI_path_A", class = "nodepath"),
        sankeyNetworkOutput("relationDiagram", width = "40%"),
        uiOutput("UI_path_B", class = "nodepath")
    ),
    
    checkboxInput("th", NULL, value = FALSE),
    conditionalPanel(
        condition = "input.th == true",
        id = "threshold_control",
        p("Threshold: "),
        sliderInput(
            "threshold_slide",
            label = NULL,
            min = 1, max = 100,
            value = c(40, 60)
        ),
        actionButton(
            "reload",
            "Reload"
        )
    ),
    
    conditionalPanel(
        condition = "input.second_rel == true",
        id = "secondPanel", class = "panel",
        sankeyNetworkOutput("relationA", width = "50%"),
        sankeyNetworkOutput("relationB", width = "50%")
    ),
    
    DT::dataTableOutput("response")
))