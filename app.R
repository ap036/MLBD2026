library(shiny)
library(tidyverse)
library(uwot)
library(NMF)
library(mclust)

# Load and prepare data
TBnanostring <- readRDS("TBnanostring.rds")

expr <- TBnanostring %>% select(-TB_Status)
expr <- as.matrix(expr)

TBnanostring$TB_Status <- factor(TBnanostring$TB_Status)

# Prepare non-negative data for NMF
expr_shift <- sweep(expr, 2, apply(expr, 2, min), "-")

# Compute dimension reductions once
set.seed(1)

nmf_fit <- NMF::nmf(expr_shift, rank = 2, nrun = 10, seed = 1)

reductions <- list(
  PCA = prcomp(expr, center = TRUE, scale. = TRUE)$x[, 1:2],
  UMAP = uwot::umap(expr, n_neighbors = 15,
                    min_dist = 0.1, scale = TRUE),
  NMF = NMF::basis(nmf_fit)
)

# Give all reduced dimensions consistent column names
reductions <- lapply(reductions, function(m) {
  m <- as.matrix(m)
  colnames(m) <- c("Dim1", "Dim2")
  m
})

# Plot settings
tb_shapes <- c(16, 17)[as.integer(TBnanostring$TB_Status)]

cluster_cols <- c(
  "#E41A1C", "#377EB8", "#4DAF4A",
  "#984EA3", "#FF7F00", "#FFFF33",
  "#A65628", "#F781BF", "#999999"
)

# UI
ui <- fluidPage(
  titlePanel("TB NanoString: Dimension Reduction and K-means"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "method",
        "Dimension reduction",
        choices = c("PCA", "UMAP", "NMF")
      ),
      
      numericInput(
        "clusters",
        "Number of clusters (k)",
        value = 2,
        min = 2,
        max = 9
      )
    ),
    
    mainPanel(
      plotOutput("plot1"),
      
      h4("Clusters vs. TB Status"),
      tableOutput("crosstab"),
      
      h4("Adjusted Rand Index"),
      verbatimTextOutput("ari")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Select dimension reduction
  selectedData <- reactive({
    reductions[[input$method]]
  })
  
  # Perform k-means on selected reduced dimensions
  clusters <- reactive({
    set.seed(42)
    
    stats::kmeans(
      selectedData(),
      centers = input$clusters,
      nstart = 25
    )
  })
  
  # Plot
  output$plot1 <- renderPlot({
    
    plot(
      selectedData(),
      col = cluster_cols[clusters()$cluster],
      pch = tb_shapes,
      cex = 1.5,
      xlab = paste(input$method, "Dimension 1"),
      ylab = paste(input$method, "Dimension 2"),
      main = paste(input$method, "with K-means Clustering")
    )
    
    # Mark cluster centers
    points(
      clusters()$centers,
      pch = 4,
      cex = 3,
      lwd = 3
    )
    
    # Cluster legend
    legend(
      "topright",
      legend = paste("Cluster", seq_len(input$clusters)),
      col = cluster_cols[seq_len(input$clusters)],
      pch = 16,
      title = "K-means clusters"
    )
    
    # TB status legend
    legend(
      "bottomleft",
      legend = levels(TBnanostring$TB_Status),
      pch = c(16, 17),
      col = "black",
      title = "TB Status"
    )
  })
  
  # Cross-tabulation
  output$crosstab <- renderTable({
    
    tab <- table(
      Cluster = clusters()$cluster,
      TB_Status = TBnanostring$TB_Status
    )
    
    tab_df <- data.frame(
      Cluster = rownames(tab),
      as.data.frame.matrix(tab),
      check.names = FALSE
    )
    
    rownames(tab_df) <- NULL
    tab_df
  })
  
  # Adjusted Rand Index
  output$ari <- renderText({
    
    round(
      mclust::adjustedRandIndex(
        clusters()$cluster,
        TBnanostring$TB_Status
      ),
      3
    )
  })
}

shinyApp(ui = ui, server = server)

