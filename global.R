############################# LIBRARIES ################################

suppressMessages(library(htmlwidgets))
suppressMessages(library(httr))
suppressMessages(library(jsonlite))
suppressMessages(library(tidyverse))
suppressMessages(library(ontologyIndex))
suppressMessages(library(sankeyD3))
suppressMessages(library(ini))
suppressMessages(library(promises))
suppressMessages(library(future))

plan(multisession)
############################## APPENDIX ################################

source("scripts/appendix.R")

############################# VARIABLES ################################

concept <<- future(list(
  habitat = get_ontology("data/BioNLP-OST+EnovFood-Habitat.obo"),
  phenotype = get_ontology("data/BioNLP-OST+EnovFood-Phenotype.obo"),
  use = get_ontology("data/Use_V2.obo"),
  taxon = get_ontology("data/microorganisms.obo")
))



concept_choices <- concept %...>%
  future(lapply(function(x) x$name %>% invert %>% .[order(nchar(names(.)))]))

                
config <- read.ini("conf.ini")
ontobiotope <<- c("habitat", "phenotype", "use")

choices_type <- c("taxon", "habitat", "phenotype", "use")
names(choices_type) <- c(config$LABELS$TYPE_TAXON,
                         config$LABELS$TYPE_HABITAT,
                         config$LABELS$TYPE_PHENOTYPE,
                         config$LABELS$TYPE_USE)

choices_indicator <- list("doc","relation")
names(choices_indicator) <- c(config$LABELS$INDICATOR_DOC,
                              config$LABELS$INDICATOR_RELATION)

choices_source <- list("Pubmed",
                       "CIRM",
                       "DSMZ",
                       "Genbank")
names(choices_source) <- c(config$LABELS$SOURCE_PUBMED,
                            config$LABELS$SOURCE_CIRM,
                            config$LABELS$SOURCE_DSMZ,
                            config$LABELS$SOURCE_GENBANK)

DEBUG <<- FALSE
request_api_url <<- NULL

############################# FUNCTIONS ################################


get_property <- function(id, type, property){
  #' Get direct children of object
  #' id: (str)
  #' type: (str)
  #' property: (str)
  #' :return: (vector)

  response <- tryCatch(
    {
      get_term_property(
        concept[[type]],
        property = property,
        term = id
      )
    }, error = function(cond){
      message("invalid term property search")
      if (property == "name"){
        return(id)
      } else {
        return(NA)
      }
    }
  )
  
  return(response)
}



get_entity <- function(id){
  #' Properties of a taxon or OntoBiotope concept given an identifier.
  #' id: (str) ncbi: taxid or OBT: obtid
  #' :return: (list) id, name, path, type, children
  
  taxon <- FALSE
  base <- paste0(config$PARAMETERS$API_HOST, "/get/")
  data <- list(
    id = id,
    name = id,
    path = id,
    type = character(),
    children = character()
  )
  
  if(grepl("ncbi:", id, fixed = T)){
    main <- paste0(base, "taxon/", id)
    taxon <- TRUE
  }else if(grepl("OBT|EC", id)){
    main <- paste0(base, "obt/", id)
  }else{
    stop(paste("\nError: unrecognized id pattern", id))
  }
  
  request <- GET(url = main)
  response <- FALSE
  
  if(request$status_code == 200){
    response <- request %>% 
      content(as = "text", encoding = "utf-8")
    response <- response != "[]"
  } else {
    if(DEBUG) cat("\nServer error")
  }
  
  if(response){
    data <- request %>% 
      content(as = "text", encoding = "utf-8") %>% 
      fromJSON
    
    data$path <- data$path[1] %>% 
      str_split("/") %>% unlist

    if(taxon){
      data$type <- "taxon"
      data$path <- data$path[-c(1:3)] # From Bacteria
    }
    data$path <- data$path[nchar(data$path) > 0] # Empty str
    data$children <- get_property(id, data$type, "children")
    
    
  } else {
    if(DEBUG) cat(paste(
      "\nget_entity: Not enough data pulled for",
      id, "\n"
    ))
  }
  
  return(data)
}



get_luca <- function(type, terms){
  #' Get Last Universal Common Ancestor
  #' type: (str) taxon, habitat, phenotype, use
  #' terms: (str) vector
  #' :return: (str) LUCA
  
  luca <- concept[[type]]$id[1]
  ancestors <- terms %>% lapply(get_property, type = type, property = "ancestors")
  short <- ancestors %>% sapply(length) %>% min
  
  if(short > 1){
    common <- ancestors %>% 
      sapply(function(x) x[1:short]) %>% 
      apply(1, function(x) length(unique(x))-1) %>% 
      as.logical %>% which
    common <- ifelse(length(common), min(common)-1, short)
    luca <- ancestors[[1]][common]
  }
  
  return(luca)
}




get_relations <- function(taxid = NULL, obtid = NULL, type = NULL, source = '', qps = FALSE){
  #' Pull in Florilege's relations resource.
  #' taxid: (str, NULL) ncbi: taxid
  #' obtid: (str, NULL) OBT: obtid
  #' type: (str, NULL) habitat, phenotype, use
  #' source: (str) PubMed, CIRM, DSMZ, GenBank
  #' qps: (bool)
  #' :return: (tibble) taxid, obtid, type, source, taxon_forms, obt_forms, docs
  
  source <- switch((source == '')+1, source, NULL)
  
  names(taxid) <- rep("taxid", length(taxid))
  names(obtid) <- rep("obtid", length(obtid))
  
  main <- paste0(config$PARAMETERS$API_HOST, "/search/relations")
  query <- c(
    taxid,
    obtid,
    list(
      source = source,
      qps = qps,
      type = type
    )
  )
  
  request <- GET(url = main, query = query)
  request_api_url <- request$url
  print("request")
  print(request)

  response <- FALSE
  data <- tibble(
    taxid = character(),
    obtid = character(),
    taxroot = character(),
    obtroot = character(),
    type = character(),
    source = character(),
    taxon_forms = list(),
    obt_forms = list(),
    docs = list()
  )

  if(request$status_code == 200){
    response <- request %>% 
      content(as = "text", encoding = "utf-8")
    
    data <- response %>% 
      fromJSON %>% as_tibble
    
    if( (response != "[]") && DEBUG ){
      cat(paste(
        "\nget_relations: Not enough data pulled:",
        taxid, obtid, "\n"
      ))
    }
  } else {
    cat("\nServer error\n")
    data <- paste("Error",request$status_code,warn_for_status(request))
  }

  return(data)
}

get_request_api_url <- function(){
  output <- request_api_url
  return(output) 
}

get_join_relations <- function(leftType, leftId, rightType, rightId, join, source = '', qps = FALSE){
  #' Pull in Florilege's join relations
  #' left, right, join: (list) cpt, root
  #' source: (str) PubMed, CIRM, DSMZ, GenBank
  #' qps: (bool)
  #' :return: (tibble) leftType, leftId, leftSource, leftDocs, joinType, joinId, right**
  
  source <- switch((source == '')+1, source, NULL)
  
  names(leftId) <- rep("left-root", length(leftId))
  names(rightId) <- rep("right-root", length(rightId))

  main <- paste0(config$PARAMETERS$API_HOST, "/search/join-relations")
  query <- c(
    leftId,
    rightId,
    list(
      "left-type" = leftType,
      "right-type" = rightType,
      "join-type" = join$cpt,
      "join-root" = join$id,
      source = source,
      qps = qps
      )
  )

  request <- GET(url = main, query = query)
  request_api_url <- request$url
  print("request")
  print(request)

  response <- FALSE
  data <- tibble(
    leftType = character(),
    leftRoot = character(),
    leftId = character(),
    leftSource = character(),
    leftDocs = list(),
    joinType = character(),
    joinId = character(),
    rightType = character(),
    rightRoot = character(),
    rightId = character(),
    rightSource = character(),
    rightDocs = list()
  )

  if(request$status_code == 200){
    response <- request %>% 
      content(as = "text", encoding = "utf-8")

    data <- response %>% 
      fromJSON %>% as_tibble

    if( (response != "[]") && DEBUG ){
      cat(paste(
        "\nget_join_relations: Not enough data pulled:",
        left$id, right$id, "join on", join$id,"\n"
      ))
    }
  } else {
    cat("\nServer error\n")
    data <- paste("Error",request$status_code,warn_for_status(request))
  }

  return(data)
}



aggregate_value <- function(taxid, obtid, type, source = '', qps = F, doc = T){
  #' Aggregation of request
  #' *: get_relations args
  #' docs: (bool) aggregate on docs
  #' :return: (tibble) value

  request <- get_relations(taxid, obtid, type, source, qps)
  
  if(is_tibble(request) && nrow(request)){
    request <- request %>%
      select(taxroot, obtroot, type, docs) %>%
      rename(taxid = taxroot) %>%
      rename(obtid = obtroot)
  
    if(doc){
      request <- request %>%
        mutate(value = lengths(docs))
    } else {
      request <- request %>%
        mutate(value = 1)
    }
    request <- request %>%
      group_by(taxid,obtid,type) %>%
      summarise(value = sum(value))
  }
  return(request)
}



join_value <- function(leftType, leftId, rightType, rightId, jt, jid = NULL, source = '', qps = F, doc = T){
  #' Aggregation of join request
  #' base: (tibble) df with all relations
  #' *: left, right and join | type and id
  #' docs: (bool) aggregate on docs
  #' :return: (tibble) joind, leftValue, rightValue, value

  request <- get_join_relations(
    leftType, leftId, rightType, rightId,
    list(id = jid, cpt = jt),
    source, qps
  ) 
  
  if(is_tibble(request) && nrow(request)){
    request <- request %>%
     select(leftType, leftRoot, rightType, rightRoot, joinType, joinId, leftDocs, rightDocs) %>%
      rename(leftId = leftRoot) %>%
      rename(rightId = rightRoot)

    request <- request %>% 
      {
        if(doc){
            mutate(
              .,
              leftValue = lengths(leftDocs),
              rightValue = lengths(rightDocs),
              value = leftValue + rightValue
            )
        } else {
          mutate(
            .,
            leftValue = 1,
            rightValue = 1,
            value = 1
          )
        }
      }
  
    request <- request %>% 
      select(-leftDocs, -rightDocs) %>%
      group_by_all %>%
      summarise(value = sum(value))
  }
  return(request)
}




filterQuery <- function(inputs, source = '', qps = F, doc = F){
  #' Relationships between items in a list
  #' inputs: (list) of left, right, join: id, cpt, list
  #' *: get_join_relations args
  #' :return: (tibble) for A and B /join: cpt_$, id_$, value*
  
  left <- inputs[[1]]
  right <- inputs[[2]]
  join <- switch((length(inputs[[3]])>0)+1, NULL, inputs[[3]])
  
  if(is.valid(left$id) & is.null(left$list)){
    left_child <- get_property(left$id, left$cpt, "children")
    left$list <- if(length(left_child)>0) {left_child} else {left$id}
  }
  
  if(is.valid(right$id) & is.null(right$list)){
    right_child <- get_property(right$id, right$cpt, "children")
    right$list <- if(length(right_child)>0) {right_child} else {right$id}
  }
  
  
  indirect <- all(c(left$cpt, right$cpt)%in%ontobiotope) | all(c(left$cpt, right$cpt)=="taxon")

  if(indirect){
    join <- if(is.valid(join)) join else list(cpt = "habitat")
    
    base <- expand.grid(list(
      leftType = left$cpt,
      leftId = left$list,
      rightType = right$cpt,
      rightId = right$list
    )) %>% mutate_all(as.character)
    
    if(nrow(base)){
      formated <-join_value(
        left$cpt, left$list, right$cpt, right$list,
        join$cpt, source = source, qps = qps, doc = doc
      )
    }else{
      formated <- base %>% 
        mutate(joinType = join$cpt) %>%
        mutate(
        .,
        leftValue = numeric(),
        rightValue = numeric(),
        value = numeric()
      )
    }
    if(is_tibble(formated) && nrow(formated)){
      formated <- formated %>% select(
      cpt_A = leftType, id_A = leftId, value_A = leftValue,
      cpt_join = joinType, id_join = joinId,
      cpt_B = rightType, id_B = rightId, value_B = rightValue,
      value
      )
    }

  } else {
    leftTaxon <- left$cpt == 'taxon'
    taxon <- if(leftTaxon) left else right
    obt <- if(leftTaxon) right else left
    
    base <- expand.grid(list(
      taxid = taxon$list,
      obtid = obt$list,
      type = obt$cpt
    )) %>% mutate_all(as.character)

    if(nrow(base)){
      formated <- aggregate_value(
          taxon$list, obt$list, obt$cpt,
          source, qps, doc
        )

    }else{
      formated <- base %>%
        mutate(., value = numeric())
    }

    if(is_tibble(formated) && nrow(formated)){
      formated <-formated %>%
        mutate(taxontype = "taxon") %>% {
          if(leftTaxon){
            select(
              ., cpt_A = taxontype, id_A = taxid,
              cpt_B = type, id_B = obtid, value
            )
          }else{
            select(
              ., cpt_A = type, id_A = obtid,
              cpt_B = taxontype, id_B = taxid, value
            )
          }
        } %>% filter(value != 0)
    }
  }

  return(formated)
}



plot_diagram <- function(formated, doc = T){
  #' Sankey plot Florilege's relations resource.
  #' formated: (tibble) cpt_A, id_A, cpt_B, id_B, /join, value*
  #' doc: (bool)
  #' :return: (networkD3) sankey plot w/ Shiny input

  indirect <- "id_join"%in%names(formated)
  if(indirect){
    formated <- formated %>% 
      group_by(cpt_A, id_A, cpt_B, id_B) %>%
      summarise(value = sum(value)) %>%
      ungroup
    }

  nodes <- tibble(
    id = c(formated$id_A, formated$id_B),
    posX = c(rep(0, nrow(formated)), rep(1, nrow(formated))),
    group = as.factor(c(formated$cpt_A, formated$cpt_B))
  ) %>% unique %>% rowwise %>% 
    mutate(
      names = get_property(id, as.character(group), "name")
    ) %>% as.data.frame
  
  edges <- tibble(
    source = formated$id_A, # IMPORTANT: define pos 0+1=1 of cpt
    target = formated$id_B, # IMPORTANT: define pos 1+1=2 of cpt
    value = formated$value
  ) %>% mutate(
    IDsource = match(source, nodes$id) - 1,
    IDtarget = match(target, nodes$id) - 1
  ) %>% as.data.frame
  
  type_color <- paste(" d3.scaleOrdinal()
    .domain(['taxon', 'habitat', 'phenotype', 'use'])
    .range(['",
                      paste(
                        config$PARAMETERS$COLOR_TAXON,
                        config$PARAMETERS$COLOR_HABITAT,
                        config$PARAMETERS$COLOR_PHENOTYPE,
                        config$PARAMETERS$COLOR_USE,
                        sep="', '"),
                      "'])", sep="")

  sankey <- sankeyNetwork(
    Links = edges, Nodes = nodes,
    Source = "IDsource", Target = "IDtarget", Value = "value",
    NodeID = "names", NodeGroup = "group", NodePosX = "posX",
    linkColor = '#dfe6e9', nodeWidth = 30,
    nodePadding = 15, colourScale = type_color, fontSize = 14,
    zoom = FALSE, numberFormat = ",.0f", showNodeValues = FALSE,
    highlightChildLinks = TRUE
  )
  
  sankey$x$nodes$id <- nodes$id
  event <- 'function() {
    d3.selectAll(".node").on("mousedown.drag", null);
    d3.selectAll(".node").on("click",function(d) {
      Shiny.onInputChange("nodeID", [d.id, d.group, d.posX+1]);
    })
  }'
  diagram <- onRender(sankey, event)
  
  return(diagram)
}


  part_diagram <- function(formated, direction = "A"){
    #' Sankey plot Florilege's indirect part relations resource.
    #' formated: (tibble) cpt_A, id_A, cpt_B, id_B, /join, value*
    #' direction: (str) A or B
    #' :return: (networkD3) sankey plot w/ Shiny input
    
    isA <- direction == "A"
    
    formated <- formated %>% 
      select(ends_with(direction), ends_with("_join")) %>% 
      select(cpt = 1, id = 2, value = 3, everything()) %>% 
      group_by(cpt, id, cpt_join, id_join) %>% 
      summarise(value = sum(value))
    #    arrange(desc(id_join))
    
    nodes <- tibble(
      id = c(formated$id, formated$id_join),
      posX = c(rep(if(isA) 0 else 1, nrow(formated)), rep(if(isA) 1 else 0, nrow(formated))),
      group = as.factor(c(formated$cpt, formated$cpt_join))
    ) %>% unique %>% rowwise %>% 
      mutate(
        names = get_property(id, as.character(group), "name")
      ) %>% as.data.frame

    edges <- tibble(
      source = if(isA) formated$id else formated$id_join,
      target = if(isA) formated$id_join else formated$id,
      value = formated$value
    ) %>% mutate(
      IDsource = match(source, nodes$id) - 1,
      IDtarget = match(target, nodes$id) - 1
    ) %>% as.data.frame

    type_color <- paste(" d3.scaleOrdinal()
    .domain(['taxon', 'habitat', 'phenotype', 'use'])
    .range(['",
                        paste(
                          config$PARAMETERS$COLOR_TAXON,
                          config$PARAMETERS$COLOR_HABITAT,
                          config$PARAMETERS$COLOR_PHENOTYPE,
                          config$PARAMETERS$COLOR_USE,
                          sep="', '"),
                        "'])", sep="")

    sankey <- sankeyNetwork(
      Links = edges, Nodes = nodes,
      Source = "IDsource", Target = "IDtarget", Value = "value",
      NodeID = "names", NodeGroup = "group", NodePosX = "posX",
      nodeWidth = 20, nodePadding = 20, colourScale = type_color,
      fontSize = 14, highlightChildLinks = TRUE,
      zoom = FALSE, numberFormat = ",.0f", showNodeValues = FALSE,
      iterations = 0
    )
    
    return(sankey)
    
  }
