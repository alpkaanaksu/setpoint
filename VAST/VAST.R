library(DiagrammeR)

construct <- function(id) {
  list(
    id = id,
    names = c()
  )
}

VAST <- function(...) {
  construct_params <- list(...)
  list(
    constructs = construct_params,
    relationships = list(),
    analysts = list(),
    fimm = FALSE
  )
}

find_node_by_type <- function (graph, type_id) {
  get_node_ids(graph, conditions = type == type_id)
}

render_namings <- function (vast) {
  diagram <- create_graph()
  
  for (construct in vast$constructs) {
    diagram <- diagram |>
      add_node(
        label = construct$id,
        type = construct$id,
        node_aes = node_aes(
          fixedsize = FALSE, 
          shape = "rectangle",
          fillcolor = "#ffffff"
        )
      )
    
    for (name in construct$names) {
      diagram <- diagram |>
        add_node(
          label = paste0("'", name, "'"),
          type = paste0(construct$id, "_name"),
          node_aes = node_aes(
            fixedsize = FALSE, 
            shape = "rectangle",
            fillcolor = "#ffffff"
          )
        )
      
      ids <- get_node_ids(diagram)
      id <- ids[length(ids)]
      
      print(id)
      
      diagram <- diagram |>
        add_edge(
          from = find_node_by_type(diagram, construct$id),
          to = id,
          edge_aes = edge_aes(
            label = "n"
          )
        )
    }
    
  }
  
  render_graph(diagram, layout = "nicely")
}

render_vast <- function (vast) {
  diagram <- create_graph()
  
  for (construct in vast$constructs) {
    diagram <- diagram |>
      add_node(
        label = construct$id,
        type = construct$id,
        node_aes = node_aes(
          fixedsize = FALSE, 
          shape = "rectangle",
          fillcolor = "#ffffff"
        )
      )
  }
  
  for (relationship in vast$relationships) {
    diagram <- diagram |>
      add_edge(
        from = find_node_by_type(diagram, relationship$from),
        to = find_node_by_type(diagram, relationship$to),
        edge_aes = edge_aes(
          label = relationship$type
        )
      )
  }
  
  title <- ""
  
  title <- paste(
    title,
    "Analysts:",
    paste(vast$analysts, collapse = ", ")
  )
  
  if (vast$fimm) {
    title <- paste0(title, " | FIMM")
  }
    
  render_graph(
    diagram,
    layout = "nicely",
    title = title
  )
}

'%-n->%' <- function (construct, name) {
  construct$names <- c(construct$names, name)
  construct
}

'%-c->%' <- function (construct1, construct2) {
  list(
    type = "c",
    from = construct1$id,
    to = construct2$id
  )
}

'%-p->%' <- function (construct1, construct2) {
  list(
    type = "p",
    from = construct1$id,
    to = construct2$id
  )
}


rel <- function(vast, rel) {
  vast$relationships[[length(vast$relationships) + 1]] <- rel
  vast
}

analysts <- function(vast, ...) {
  names <- c(...)
  vast$analysts <- names
  vast
}

fimm <- function(vast) {
  vast$fimm <- TRUE
  vast
}




