library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(cluster)
library(factoextra)
library(plotly)
library(maps)
Sys.setlocale("LC_TIME", "C")

data <- read.csv("https://raw.githubusercontent.com/zombie-007/world_happiness/refs/heads/main/World-happiness-report-2024.csv")

data_cleaned <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
scaled_data <- as.data.frame(scale(select(data_cleaned, where(is.numeric))))

# PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
scores <- as.data.frame(pca_result$x)

par(mar = c(.1, .1, .1, .1))
fa.parallel(scaled_data, n.obs=dim(scaled_data)[1], fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")

pcs <- principal(scaled_data, nfactors=3, score=TRUE)
pcs_scores <- round(as.data.frame(pcs$scores), 3)

rc1_weight <- 0.51 / (0.51 + 0.18 + 0.14)
rc2_weight <- 0.18 / (0.51 + 0.18 + 0.14)
rc3_weight <- 0.14 / (0.51 + 0.18 + 0.14)

pcs_scores$Happiness_Score <- pcs_scores$RC1 * rc1_weight +
  pcs_scores$RC2 * rc2_weight +
  pcs_scores$RC3 * rc3_weight

happiness_data <- cbind(scaled_data, pcs_scores)
happiness_data$Rank <- rank(-happiness_data$Happiness_Score, ties.method = "min")
happiness_data$Country.name <- data$Country.name
happiness_data$Regional.indicator <- data$Regional.indicator

# Map
world_map <- maps::map("world", plot = FALSE, fill = TRUE)
world_map <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

world_map$region <- tolower(world_map$ID)
happiness_data$region <- tolower(happiness_data$Country.name)
happiness_data$region <- recode(happiness_data$region,
                                "united states" = "usa",
                                "czechia" = "czech republic",
                                "united kingdom" = "uk",
                                "taiwan province of china" = "taiwan",
                                "hong kong s.a.r. of china" = "hong kong",
                                "congo (brazzaville)" = "republic of congo",
                                "turkiye" = "turkey",
                                "state of palestine" = "palestine",
                                "eswatini" = "swaziland",
                                "congo (kinshasa)" = "democratic republic of the congo")

map_data_merged <- left_join(world_map, happiness_data, by = "region")

# Shiny
ui <- fluidPage(
  titlePanel("2024 World Happiness Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Options"),
      p("Explore global happiness scores in 2024 with interactive visualizations."),
      hr(),
      h4("Instructions"),
      p("1. Click on the map to see country details."),
      p("2. View PCA and Clustering analysis in tabs below."),
      p("3. Browse original data in the table view."),
      hr(),
      h4("Explore Advanced Insights"),
      p("Dive deeper into world happiness data with Principal Component Analysis (PCA) and K-means clustering. Visualize and understand complex patterns with ease.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("worldMap", height = 600),
                 verbatimTextOutput("countryInfo")),
        tabPanel("PCA Analysis",
                 plotOutput("pcaPlot", height = 400),
                 verbatimTextOutput("pcaDetails")),
        tabPanel("Clustering Analysis",
                 plotOutput("clusterPlot", height = 400),
                 verbatimTextOutput("clusterDetails")),
        tabPanel("Data Table",
                 dataTableOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$worldMap <- renderLeaflet({
    leaflet(map_data_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlGnBu", domain = map_data_merged$Happiness_Score)(Happiness_Score),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        layerId = ~region,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          bringToFront = TRUE
        ),
        label = ~paste0(
          "Country: ", Country.name, ",", "\n",
          "Happiness Score: ", round(Happiness_Score, 2), ",", "\n",
          "Rank: ", Rank
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = colorNumeric("YlGnBu", domain = map_data_merged$Happiness_Score),
        values = ~Happiness_Score,
        title = "Happiness Score"
      )
  })
  
  observeEvent(input$worldMap_shape_click, {
    click <- input$worldMap_shape_click
    if (!is.null(click$id)) {
      selected_country <- map_data_merged %>%
        filter(region == click$id) %>%
        select(Country.name, Happiness_Score, Rank) %>%
        distinct()
      if (nrow(selected_country) > 0) {
        output$countryInfo <- renderText({
          paste0(
            "Country: ", selected_country$Country.name, "\n",
            "Happiness Score: ", round(selected_country$Happiness_Score, 2), "\n",
            "Rank: ", selected_country$Rank
          )
        })
      }
    }
  })

  output$pcaPlot <- renderPlot({
    pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
    fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
      ggtitle("Principal Component Variance Interpretation Plot")
  })

  output$pcaDetails <- renderText({
    pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
    loadings <- pca_result$rotation
    details <- capture.output(print(loadings))
    paste(details, collapse = "\n")
  })

  output$clusterPlot <- renderPlot({
    kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
    fviz_cluster(
      list(data = scaled_data, cluster = kmeans_result$cluster),
      geom = "point",
      ellipse.type = "convex", 
      palette = "jco",
      ggtheme = theme_minimal(),
      stand = FALSE
    ) +
      ggtitle("K-Means Clustering Result Plot")
  })

  output$clusterDetails <- renderText({
    kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
    paste0("Cluster centers:\n", paste(capture.output(print(kmeans_result$centers)), collapse = "\n"))
  })

  output$dataTable <- renderDataTable({
    datatable(happiness_data[,c('Country.name','Regional.indicator','Happiness_Score','Rank')], options = list(pageLength = 10))
  })
}

shinyApp(ui, server)

