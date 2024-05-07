library(shiny)
library(igraph)
library(shinyalert)

sliceInput <- function(text){
  text <- trimws(text)
  text2 <- strsplit(text, "\n")[[1]]
  lines <- gsub("\\(|\\)", "", text2)
  return(lines)
}

checkGrammar <- function(text, opt){
    if(!opt){
    if(grepl("S", text) && grepl("->", text) && grepl("[^A-Z]", text)){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    if(grepl("S", text) && grepl(".", text)){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
}

checkDeterministic <- function(trans, st) {
  
  alphabet <- unique(trans[["symbol"]])

  nodeData <- data.frame(nodeFill = integer(0), nodeFrame = integer(0))
  edgeData <- c()
  
  for(state in st){
    
    # Fills
    # Nada
    fill <- 1
    frame <- 1
    
    # Inicial
    if(state == "S"){ fill <- 2 }
    
    # Final
    if(grepl("[.]", state)){ fill <- 3 }
    
    # Ambas
    if(state == "S."){ fill <- 4 }
    
    # Frames
    # Falta | No falta
    stateEdges <- trans[trans$from == state, "symbol"]


    if (anyDuplicated(stateEdges)) { outside <- 2 }
    outside <- ifelse(all(alphabet %in% gsub("\u03B5", "", stateEdges)), 1, 2)
    nodeData <- rbind(nodeData, data.frame(nodeFill = fill, nodeFrame = outside))
    
  }
  
  
  
  for (i in 1:nrow(trans)) {
    # Edges
    #normal
    edge <- 1
    # Vacío
    # Repetido | Normal|
    
    #is_duplicate <- duplicated(trans[i, c("symbol")]) | duplicated(trans[i, c("symbol")], fromLast = TRUE)
    is_duplicate <- duplicated(trans$from[i]) | duplicated(trans$from[i], fromLast = TRUE)

    # Mark duplicates with value 2 in frame

    edge <- ifelse(is_duplicate, 3, 1)
    if (trans[i, "symbol"] == "\u03B5" || trans[i, "symbol"] == "ε") { edge <- 2 }
    
    edgeData <- c(edgeData, edge)
  }
  
  return(list(nodeData, edgeData))
}

convertGrammarToDeterministic <- function(text){
  transitions <- data.frame(from = character(0), to = character(0), symbol = character(0))
  colorData <- data.frame(nodeFill = integer(0), nodeFrame = integer(0), edgeColor = integer(0))
  states <- character(0)
  
  for(rule in text){
    
    if(rule == ""){next}
    
    rule_parts <- strsplit(rule, ",")[[1]]
    
    from_state <- trimws(rule_parts[1])
    
    to_state <- trimws(rule_parts[2])

    symbols <- trimws(rule_parts[3])
    
    if(symbols == "-"){ symbols = "\u03B5" }
    
    states <- union(states, from_state)
    states <- union(states, to_state)
    
    transitions <- rbind(transitions, data.frame(from = from_state, to = to_state, symbol = symbols))
  }
  
  colorData <- checkDeterministic(transitions, states)
  
  nodes <- colorData[[1]]  # Accessing the nodeData from colorData
  edges <- colorData[[2]] # Etc

  fillMap <- c("white","green","red","yellow")
  frameMap <- c("black","pink")
  edgeMap <-c("black","grey","#9300FF")
  
  # Access individual values from nodes
  fill <- nodes$nodeFill
  frame <- nodes$nodeFrame
  
  nodeColor <- fillMap[fill]
  edgeColor <- edgeMap[edges]
  frameColor <- frameMap[frame]
  
  # Make the graph thingy
  graph <- graph_from_data_frame(d = transitions, vertices = states, directed = TRUE)
  
  # Customize props
  V(graph)$name <- gsub("\\.", "", V(graph)$name)
  V(graph)$color <- nodeColor
  V(graph)$frame.color <- frameColor
  V(graph)$size <- 35
  E(graph)$label.dist <- 50
  E(graph)$color <- edgeColor
  E(graph)$label <- transitions$symbol
  return(graph)
}

convertGrammarToAutomaton <- function(grammar){
  #Create DF
  transitions <- data.frame(from = character(0), to = character(0), symbol = character(0))
  #Create states vector
  states <- character(0)

  #Process the rules
  for(rule in grammar){
    if(rule == ""){next}
    
    rule_parts <- strsplit(rule, "->")[[1]]
    
    from_state <- trimws(rule_parts[1])
    
    
    to_state <- trimws(rule_parts[2])
    to_state <- gsub("[^A-Z]", "", to_state)
    
    #Get the symbols
    symbols <- trimws(rule_parts[2])
    symbols <- gsub("[^a-z]", "", symbols)
    
    if(to_state == ""){
      to_state = "Z"
    }
    
    #Join em
    states <- union(states, from_state)
    states <- union(states, to_state)
    
    transitions <- rbind(transitions, data.frame(from = from_state, to = to_state, symbol = symbols))
  }
  
  # Make the graph thingy
  graph <- graph_from_data_frame(d = transitions, vertices = states, directed = TRUE)
  
  # Customize props
  V(graph)$color <- ifelse(V(graph)$name == "S", "green", ifelse(V(graph)$name == "Z", "red", "lightblue"))
  V(graph)$size <- ifelse(V(graph)$name == "Z", 30, 20)
  E(graph)$label <- transitions$symbol
  
  return(graph)
  
}

# Define the UI
ui <- fluidPage(
  titlePanel("Grammar to Automaton"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("grammarInput", "Enter the regular grammar:",
                    placeholder = "Enter grammar rules in the format X -> Y",
                    height = "60vh", value = "(S,B.,a)\n(S,A,b)\n(B.,A,-)\n(B.,A,b)\n(B.,C,b)"),
      actionButton("convertButton", "Convert"),
      checkboxInput("optionalCheck", "Opcional")
    ),
    mainPanel(
      plotOutput("graphOutput")
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  observeEvent(input$convertButton, 
  {
    
    inputText = input$grammarInput
    optional = input$optionalCheck
    
    if(input$grammarInput != ""){
      
      grammar <- sliceInput(inputText)
      
      if(optional){
        if(checkGrammar(inputText, optional)){
          graph <- convertGrammarToDeterministic(grammar)
          output$graphOutput <- renderPlot({
            plot(graph)
          })
        }
      }
      else{
        if(checkGrammar(inputText, optional)){
          
          graph <- convertGrammarToAutomaton(grammar)
          
          output$graphOutput <- renderPlot({
            plot(graph)
          })
        }
        else{
          shinyalert(title = "Error",
                     text = "Gramática incorrecta.",
                     type = "error",
                     confirmButtonText = "OK")
        }
      }
    }
    else{
      shinyalert(
        title = "Error",
        text = "Ingrese alguna instrucción.",
        type = "error",
        confirmButtonText = "OK"
      )
    }
  })
}

# Launch the Shiny app
shinyApp(ui, server)
