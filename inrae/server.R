############################# LIBRARIES ################################

suppressMessages(library(DT))
suppressMessages(library(sankeyD3))
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))

############################## SERVER ##################################

shinyServer(function(input, output, session){
    
    
    ###-- AUTOCOMPLETION --###
    observe({
        updateSelectizeInput(
            session, 'root_A', server = TRUE, selected = '',
            choices = concept_choices[[input$cpt_A]]
        )
        
        updateSelectizeInput(
            session, 'list_A', server = TRUE, selected = '',
            choices = concept_choices[[input$cpt_A]]
        )
    })
    
    observe({
        updateSelectizeInput(
            session, 'root_B', server = TRUE, selected = '',
            choices = concept_choices[[input$cpt_B]]
        )
        
        updateSelectizeInput(
            session, 'list_B', server = TRUE, selected = '',
            choices = concept_choices[[input$cpt_B]]
        )
    })

    
    ###--- PROCESS CALL ---###
    response_data <- reactiveVal()
    rootCpt_A <- reactiveValues(pos = '1')
    rootCpt_B <- reactiveValues(pos = '2')
    rootJoin <- reactiveValues()
    use_doc <- reactiveVal()
    activeCpt_A <- reactiveVal()
    activeCpt_B <- reactiveVal()
    
    observeEvent(input$process,{
        A <- input$root_A != "" | length(input$list_A) > 0
        B <- input$root_B != "" | length(input$list_B) > 0
        req(A, B)
        
        disable("process")
        disable("path_A")
        disable("path_B")
        
        response_data(NULL)
        
        use_doc(isolate({input$indicator == "doc"}))
        
        if(input$root_A != ""){
            activeCpt_A(isolate(input$root_A))
            rootCpt_A$id <- isolate(input$root_A)
            rootCpt_A$list <- NULL
        } else {
            activeCpt_A(NULL)
            rootCpt_A$id <- NULL
            rootCpt_A$list <- isolate(input$list_A)
        }
        
        if(input$root_B != ""){
            activeCpt_B(isolate(input$root_B))
            rootCpt_B$id <- isolate(input$root_B)
            rootCpt_B$list <- NULL
        } else {
            activeCpt_B(NULL)
            rootCpt_B$list <- NULL
            rootCpt_B$list <- isolate(input$list_B)
        }
        
        
        rootCpt_A$cpt <- isolate(input$cpt_A)
        rootCpt_B$cpt <- isolate(input$cpt_B)
        rootJoin$cpt <- isolate(input$cpt_join)
        
        filterQuery(list(
            reactiveValuesToList(rootCpt_A),
            reactiveValuesToList(rootCpt_B),
            reactiveValuesToList(rootJoin)
            ),
            doc = use_doc(),
            source = isolate({input$source}),
            qps = isolate({input$qps})
        ) %>% response_data()
        
        enable("process")
        enable("path_A")
        enable("path_B")
    })
    
    
    ###--- TABLE ---###
    output$response <- renderDataTable({
        req(response_data())
        req(nrow(response_data()) > 0)
        
        indirect <- "id_join"%in%names(response_data())
        
        data <- response_data()
        
        print(paste(nrow(data), "relations."))
        
        if(indirect){
            data <- select(data, -value)
            names(data) <- c('Type A', 'id A', 'Value A', 'Type join', 'id join', 'Type B', 'id B', 'Value B')
        }else{
            names(data) <- c('Type A', 'id A', 'Type B', 'id B', 'Value')
        }
        
        out <- datatable(
            data, 
            options = list(
                pageLength = 10, lengthChange = FALSE,
                bFilter = TRUE
            ),
            width = "100%"
        )
        
        return(out)
    })
    
    
    ###--- THRESHOLD ---###
    threshold <- reactive({
        req(response_data())
        data <- response_data()
        maxnode <- sapply(
            list(data$id_A, data$id_B),
            function(x) unique(x) %>% length
        ) %>% max
        
        return(maxnode)
    })
    
    
    plot_data <- reactiveVal()
    observe({
        req(threshold())
        data <- response_data()
        if (threshold() < 50){
            updateCheckboxInput(session, "th", value = FALSE)
            plot_data(data)
        } else {
            updateCheckboxInput(session, "th", value = TRUE)
            plot_data(NULL)
            
            limits <- c(min(data$value), max(data$value))
            
            updateSliderInput(
                session, "threshold_slide",
                min = limits[1], max = limits[2],
                value = c(median(limits), limits[2])
            )
            
            response_data() %>% 
                filter(between(value, median(limits), limits[2])) %>% 
                plot_data()
        }
    })
    
    
    observeEvent(input$reload,{
        req(response_data())
        print("update input")
        limits <- input$threshold_slide
        response_data() %>% 
            filter(between(value, limits[1]+1, limits[2])) %>% 
            plot_data()
    })
    
    
    ###--- JOINT ---###
    indirect <- reactive({
        req(input$cpt_A, input$cpt_B)
        inputs <- c(input$cpt_A, input$cpt_B)
        return(all(inputs%in%ontobiotope) | all(inputs=="taxon"))
    })
    
    
    ###--- PLOTS ---###
    output$relationDiagram <- renderSankeyNetwork({
        req(plot_data())
        req(nrow(plot_data()) > 0)
        plot_diagram(plot_data(), use_doc())
    })
    
    output$relationA <- renderSankeyNetwork({
        req(indirect(), response_data(), req(input$second_rel))
        req(nrow(response_data()) > 0)
        part_diagram(response_data(), "A")
    })
    
    output$relationB <- renderSankeyNetwork({
        req(indirect(), response_data(), req(input$second_rel))
        req(nrow(response_data()) > 0)
        part_diagram(response_data(), "B")
    })
    
    
    ###--- PATH A ---###
    output$UI_path_A <- renderUI({
        req(activeCpt_A())
        path <- get_entity(activeCpt_A())$path %>% 
            sapply(function(x) get_entity(x)$name) %>% 
            invert# %>% tail(3) # 3 ancètres au plus
        
        radioGroupButtons(
            inputId = "path_A",
            label = "Path node A",
            choices = path,
            selected = activeCpt_A(),
            direction = "vertical"
        )
    })
    
    ###--- PATH B ---###
    output$UI_path_B <- renderUI({
        req(activeCpt_B())
        path <- get_entity(activeCpt_B())$path %>% 
            sapply(function(x) get_entity(x)$name) %>%
            invert# %>% tail(3) # 3 ancètres au plus
        
        radioGroupButtons(
            inputId = "path_B",
            label = "Path node B",
            choices = path,
            selected = activeCpt_B(),
            direction = "vertical"
        )
    })
    
    
    ###--- NO RELATION ---###
    observe({
        req(response_data())
        req(nrow(response_data()) == 0)
        
        showModal(modalDialog(
            title = NULL,
            "No more relation beyond this level",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    ###--- CLICK NODE ---###
    observeEvent(input$nodeID,{
        disable("process")
        disable("path_A")
        disable("path_B")
        hide("relationDiagram")
        
        use_doc(isolate({input$indicator == "doc"}))
        node <- isolate(input$nodeID)
        #node: id, cpt, posX
        
        temp_A <- list(
            active = activeCpt_A(),
            cpt = rootCpt_A$cpt,
            list = rootCpt_A$list
        )
        temp_B <- list(
            active = activeCpt_B(),
            cpt = rootCpt_B$cpt,
            list = rootCpt_B$list
        )
        
        if(node[3] == rootCpt_A$pos){ # pos "1"
            temp_A$active <- node[1]
            temp_A$list <- NULL
        }else{
            temp_B$active <- node[1]
            temp_B$list <- NULL
        }
        
        formated <- filterQuery(list(
            list(cpt = temp_A$cpt, list = temp_A$list, id = temp_A$active, pos = '1'),
            list(cpt = temp_B$cpt, list = temp_B$list, id = temp_B$active, pos = '2'),
            reactiveValuesToList(rootJoin)
            ),
            doc = use_doc(),
            source = isolate({input$source}),
            qps = isolate({input$qps})
        )
        
        
        if(nrow(formated)){
            
            formated %>% response_data()
            
            if(node[3] == rootCpt_A$pos){
                activeCpt_A(node[1])
                rootCpt_A$id <- node[1]
                rootCpt_A$list <- NULL
            }else{
                activeCpt_B(node[1])
                rootCpt_B$id <- node[1]
                rootCpt_B$list <- NULL
            }
        } else {
            showModal(modalDialog(
                title = NULL,
                "No more relation beyond this level",
                easyClose = TRUE,
                footer = NULL
            ))
        }
        
        response_data()
        enable("process")
        enable("path_A")
        enable("path_B")
        show("relationDiagram")
    })
    
    
    ###--- CLICK PATH ---###
    observeEvent({
        input$path_A
        input$path_B
    }, {
        req(is.valid(input$path_A) && input$path_A != activeCpt_A() | is.valid(input$path_B) && input$path_B != activeCpt_B())
        
        disable("process")
        disable("path_A")
        disable("path_B")
        hide("relationDiagram")

        
        use_doc(isolate({input$indicator == "doc"}))

        clicked <- c()
        for (inputs in list(c(input$path_A, activeCpt_A()), c(input$path_B, activeCpt_B()))){
            clicked <- c(clicked, inputs[1] != inputs[2])
        } # which one is clicked
        
        node <- list(input$path_A, input$path_B)[[which(clicked)]]
        
        temp_A <- list(
            active = activeCpt_A(),
            cpt = rootCpt_A$cpt,
            list = rootCpt_A$list
        )
        temp_B <- list(
            active = activeCpt_B(),
            cpt = rootCpt_B$cpt,
            list = rootCpt_B$list
        )
        
        if(which(clicked) == rootCpt_A$pos){ # pos "1"
            temp_A$active <- node
            temp_A$list <- NULL
        }else{
            temp_B$active <- node
            temp_B$list <- NULL
        }
        
        formated <- filterQuery(list(
            list(cpt = temp_A$cpt, list = temp_A$list, id = temp_A$active, pos = '1'),
            list(cpt = temp_B$cpt, list = temp_B$list, id = temp_B$active, pos = '2'),
            reactiveValuesToList(rootJoin)
            ),
            doc = use_doc(),
            source = isolate({input$source}),
            qps = isolate({input$qps})
        )
        
        
        if(nrow(formated)){
            
            formated %>% response_data()
            
            if(which(clicked) == rootCpt_A$pos){ # pos "1"
                activeCpt_A(node)
                rootCpt_A$id <- node
                rootCpt_A$list <- NULL
            }else{
                activeCpt_B(node)
                rootCpt_A$id <- node
                rootCpt_A$list <- NULL
            }
        } else {
            showModal(modalDialog(
                title = NULL,
                "No more relation beyond this level",
                easyClose = TRUE,
                footer = NULL
            ))
        }
        
        enable("process")
        enable("path_A")
        enable("path_B")
        show("relationDiagram")
    })
    
    
    
    ###--- URL CALL---###
    observe({
        req(is.null(activeCpt_A()), is.null(activeCpt_B()))
        
        query <- parseQueryString(session$clientData$url_search)
        cpt_A <- query$cptA
        root_A <- query$rootA
        list_A <- query$listA
        cpt_B <- query$cptB
        root_B <- query$rootB
        list_B <- query$listB
        cpt_join <- query$join
        source <- query$source
        qps <- query$qps
        indicator <- query$indicator
        
        
        A <- length(c(root_A, list_A)) > 0
        B <- length(c(root_B, list_B)) > 0
        
        req(cpt_A, cpt_B, A, B)
        updateSelectInput(session, inputId = "cpt_A", selected = cpt_A)
        updateSelectInput(session, inputId = "cpt_B", selected = cpt_B)
        updateSelectizeInput(
            session, 'root_A', server = TRUE, selected = root_A,
            choices = concept[[cpt_A]]$id %>% unname
        )
        updateSelectizeInput(
            session, 'root_B', server = TRUE, selected = root_B,
            choices = concept[[cpt_B]]$id %>% unname
        )
        
        if(is.valid(list_A)){
            updateSelectizeInput(
                session, 'list_A', server = TRUE,
                selected = list_A %>% strsplit(',') %>% .[[1]],
                choices = concept[[input$cpt_A]]$name %>% invert
            )
        }
        
        if(is.valid(list_B)){
            updateSelectizeInput(
                session, 'list_B', server = TRUE,
                selected = list_B %>% strsplit(',') %>% .[[1]],
                choices = concept[[input$cpt_B]]$name %>% invert
            )
        }
        
        if(is.valid(cpt_join)){
            updateSelectInput(
                session, 'cpt_join', selected = cpt_join
            )
        }
        
        if(is.valid(source)){
            updateSelectInput(
                session, 'session', selected = source
            )
        }
        
        if(is.valid(qps)){
            updateMaterialSwitch(
                session, 'qps', as.logical(qps)
            )
        }
        
        
        if(is.valid(indicator)){
            updateSelectInput(
                session, 'cpt_join', selected = indicator
            )
        }
        
        req(input$root_A, input$root_B, input$indicator)
        
        disable("process")
        disable("path_A")
        disable("path_B")
        response_data(NULL)
        
        use_doc(isolate({input$indicator == "doc"}))
        
        if(input$root_A != ""){
            activeCpt_A(isolate(input$root_A))
            rootCpt_A$id <- isolate(input$root_A)
            rootCpt_A$list <- NULL
        } else {
            activeCpt_A(NULL)
            rootCpt_A$id <- NULL
            rootCpt_A$list <- isolate(input$list_A)
        }
        
        if(input$root_B != ""){
            activeCpt_B(isolate(input$root_B))
            rootCpt_B$id <- isolate(input$root_B)
            rootCpt_B$list <- NULL
        } else {
            activeCpt_B(NULL)
            rootCpt_B$list <- NULL
            rootCpt_B$list <- isolate(input$list_B)
        }
        
        rootCpt_A$cpt <- isolate(input$cpt_A)
        rootCpt_B$cpt <- isolate(input$cpt_B)
        rootJoin$cpt <- isolate(input$cpt_join)
        
        filterQuery(list(
            reactiveValuesToList(rootCpt_A),
            reactiveValuesToList(rootCpt_B),
            reactiveValuesToList(rootJoin)
            ),
            doc = use_doc(),
            source = isolate({input$source}),
            qps = isolate({input$qps})
        ) %>% response_data()
        
        enable("process")
        enable("path_A")
        enable("path_B")
    })
})