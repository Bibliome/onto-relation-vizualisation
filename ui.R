############################# LIBRARIES ################################

suppressMessages(library(sankeyD3))
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))

################################# UI ###################################

source("help-ui.R", local = TRUE)

shinyUI(bootstrapPage(
    theme = "styles.css",
    useShinyjs(),
    
    #------------------------ SEARCH PANEL ----------------------------#
    div(
        id = "searchPanel", class = "panel",
        h1(config$LABELS$TITLE),
        p(
          id ="caption",
          config$LABELS$INTRO,
          span(class="caption_taxon", "taxon"),
          span(","),
          span(class="caption_habitat", "habitat"),
          span(","),
          span(class="caption_phenotype", "phenotype"),
          span(","),
          span(class="caption_use", "use"),
        ),
        
        div(
            class = "conceptArea",
            h2(config$LABELS$CONCEPT_A),
            selectInput(
                inputId = "cpt_A",
                choices = choices_type,
                label = NULL,
                selected = "taxon"
            ),
            selectizeInput(
                inputId = "root_A",
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                width = "100%",
                options = list(
                    placeholder = config$LABELS$CONCEPT_NAME_PLACEHOLDER_A,
                    maxOptions = 5,
                    create = FALSE
                )
            )
        ),
        
        div(
            class = "conceptArea",
            h2(config$LABELS$CONCEPT_B),
            selectInput(
                inputId = "cpt_B",
                choices = rev(choices_type),
                label = NULL,
                selected = "habitat"
            ),
            selectizeInput(
                inputId = "root_B",
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                width = "100%",
                options = list(
                    placeholder = config$LABELS$CONCEPT_NAME_PLACEHOLDER_B,
                    maxOptions = 5,
                    create = FALSE
                )
            )
        ),
        
        prettyToggle(
            inputId = "showfilters",
            label_on = config$LABELS$FILTER_ON,
            label_off = config$LABELS$FILTER_OFF,
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
                    label = config$LABELS$INDICATOR,
                    choices = choices_indicator,
                    selected = "doc"
                ),
                
                selectizeInput(
                  inputId = "list_A",
                  label = config$LABELS$CONCEPT_LIST_A,
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    placeholder = config$LABELS$CONCEPT_LIST_PLACEHOLDER,
                    maxOptions = 5,
                    create = FALSE
                  )
                ),
                
                selectInput(
                    inputId = "source",
                    label = config$LABELS$SOURCE,
                    choices = c(All = '', choices_source),
                    selected = ''
                ),
                
                selectizeInput(
                  inputId = "list_B",
                  label = config$LABELS$CONCEPT_LIST_B,
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    placeholder = config$LABELS$CONCEPT_LIST_PLACEHOLDER,
                    maxOptions = 5,
                    create = FALSE
                  )
                ),
                
                selectInput(
                    inputId = "cpt_join",
                    label = config$LABELS$JOIN_CONCEPT,
                    choices = rev(choices_type),
                    selected = "habitat"
                )
            ),
            
            div(
                class = "switchArea",
                materialSwitch(
                    inputId = "qps",
                    label = config$LABELS$QPS,
                    value = FALSE,
                    status = "primary"
                ),
                
                materialSwitch(
                    inputId = "second_rel",
                    label = config$LABELS$SECOND_REL,
                    value = FALSE,
                    status = "primary"
                )
            )
        ),

        actionButton("process", config$LABELS$PROCESS)
        
    ),
    #--------------------------- DIAGRAM ------------------------------#
    div(
        id = "visuPanel", class = "panel",
        uiOutput("visuPanel_intro"),
        uiOutput("UI_path_A", class = "nodepath"),
        sankeyNetworkOutput("relationDiagram", width = "40%"),
        uiOutput("UI_path_B", class = "nodepath")
    ),
    
    checkboxInput("threshold_checkbox", NULL, value = FALSE),
    conditionalPanel(
        condition = "input.threshold_checkbox == true",
        id = "threshold_control",
        p(config$LABELS$THRESHOLD),
        sliderInput(
            "threshold_slide",
            label = NULL,
            min = 1, max = 100,
            value = c(as.numeric(config$PARAMETERS$THRESHOLD)-10, as.numeric(config$PARAMETERS$THRESHOLD)+10)
        ),
        actionButton(
            "reload",
            config$LABELS$RELOAD
        )
    ),
    
    conditionalPanel(
        condition = "input.second_rel == true",
        id = "secondPanel", class = "panel",
        sankeyNetworkOutput("relationA", width = "50%"),
        sankeyNetworkOutput("relationB", width = "50%")
    ),
    
    uiOutput("source_url"),
    DT::dataTableOutput("response")
))
