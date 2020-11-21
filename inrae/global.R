############################# LIBRARIES ################################

suppressMessages(library(htmlwidgets))
suppressMessages(library(httr))
suppressMessages(library(jsonlite))
suppressMessages(library(tidyverse))
suppressMessages(library(ontologyIndex))
suppressMessages(library(sankeyD3))

############################## APPENDIX ################################

source("scripts/appendix.R")

############################# VARIABLES ################################

concept <<- list(
  habitat = get_ontology("../../data/BioNLP-OST+EnovFood-Habitat.obo"),
  phenotype = get_ontology("../../data/BioNLP-OST+EnovFood-Phenotype.obo"),
  use = get_ontology("../../data/Use_V2.obo"),
  taxon = get_ontology("../../data/microorganisms.obo")
)


concept_choices <- concept %>%
  lapply(function(x) x$name %>% invert %>% .[order(nchar(names(.)))])

ontobiotope <<- c("habitat", "phenotype", "use")

DEBUG <<- FALSE


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
  base <- "http://migale.jouy.inra.fr/florilege-api/api/get/"
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
  
  main <- "http://migale.jouy.inra.fr/florilege-api/api/search/relations"
  query <- list(
    source = source,
    taxid = taxid,
    qps = qps,
    obtid = obtid,
    type = type
  )
  
  
  request <- GET(url = main, query = query)

  response <- FALSE
  data <- tibble(
    taxid = character(),
    obtid = character(),
    type = character(),
    source = character(),
    taxon_forms = list(),
    obt_forms = list(),
    docs = list()
  )
  
  if(request$status_code == 200){
    response <- request %>% 
      content(as = "text", encoding = "utf-8")
    response <- response != "[]"
  }else if (request$status_code == 400){
    cat("\nBad Request - Check arguments\n")
  } else {
    cat("\nServer error\n")
  }
  
  if(response){
    data <- request %>% 
      content(as = "text", encoding = "utf-8") %>% 
      fromJSON %>% as_tibble
  } else {
    if(DEBUG) cat(paste(
      "\nget_relations: Not enough data pulled:",
      taxid, obtid, "\n"
    ))
  }
  
  return(data)
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

  main <- "http://migale.jouy.inra.fr/florilege-api-dev/api/search/join-relations"
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
  }else if (request$status_code == 400){
    cat("\nBad Request - Check arguments\n")
  } else {
    cat("\nServer error\n")
  }
  
  if(response != "[]"){
    data <- response %>% 
      fromJSON %>% as_tibble
  } else {
    if(DEBUG) cat(paste(
      "\nget_join_relations: Not enough data pulled:",
      left$id, right$id, "join on", join$id,"\n"
    ))
  }
  
  return(data)
}



aggregate_value <- function(taxid, obtid, type, source = '', qps = F, doc = T){
  #' Aggregation of request
  #' *: get_relations args
  #' docs: (bool) aggregate on docs
  #' :return: (tibble) value

  request <- get_relations(taxid, obtid, type, source, qps) %>% pull(docs)
  if(doc){
    aggregation <- tibble(value = request %>% lengths %>% sum)
  } else {
    aggregation <- tibble(value = request %>% length)
  }
  
  return(aggregation)
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
  ) %>% select(joinId, leftDocs, rightDocs)

  aggregation <- request %>% {
    if(doc){
        mutate(
          .,
          leftValue = lengths(leftDocs),
          rightValue = lengths(rightDocs),
          value = lengths(list(union(leftDocs, rightDocs)))
        )
    } else {
      mutate(
        .,
        leftValue = 1,
        rightValue = 1,
        value = 1
      )
    }
  } %>% select(-leftDocs, -rightDocs)
  
  return(aggregation)
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
    
    formated <- base%>% 
      mutate(joinType = join$cpt) %>% {
        if(nrow(.)){
          group_by_all(.) %>% do(join_value(
            left$cpt, left$list, right$cpt, right$list,
            join$cpt, source = source, qps = qps, doc = doc
          )) %>% ungroup
        }else{
          mutate(
            .,
            leftValue = numeric(),
            rightValue = numeric(),
            value = numeric()
          )
        }
      } %>% select(
        cpt_A = leftType, id_A = leftId, value_A = leftValue,
        cpt_join = joinType, id_join = joinId,
        cpt_B = rightType, id_B = rightId, value_B = rightValue,
        value
      )
  
  } else {
    leftTaxon <- left$cpt == 'taxon'
    taxon <- if(leftTaxon) left else right
    obt <- if(leftTaxon) right else left
    
    base <- expand.grid(list(
      taxid = taxon$list,
      obtid = obt$list,
      type = obt$cpt
    )) %>% mutate_all(as.character)

    formated <- base %>% {
        if(nrow(.)){
          group_by_all(.) %>% do(aggregate_value(
            .$taxid, .$obtid, .$type,
            source, qps, doc
          )) %>% ungroup
        }else{
          mutate(., value = numeric())
        }
      } %>% mutate(taxontype = "taxon") %>% {
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
  
  
  type_color <- " d3.scaleOrdinal() 
    .domain(['taxon', 'habitat', 'phenotype', 'use'])
    .range(['#69b3a2', 'steelblue', '#242654', '#5cf1bb'])
  "
  
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
    
    type_color <- " d3.scaleOrdinal() 
    .domain(['taxon', 'habitat', 'phenotype', 'use'])
    .range(['#69b3a2', 'steelblue', '#242654', '#5cf1bb'])
  "
    
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
