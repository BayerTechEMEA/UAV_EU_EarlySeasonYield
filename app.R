# Installer les packages nécessaires si ce n'est pas déjà fait
# install.packages(c("shiny", "dplyr", "ggplot2", "DT"))

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Lire le fichier CSV
file_path <- "UAV_QC.csv"
UAV_QC <- read.csv(file_path)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("UAV data analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select year:", 
                  choices = unique(UAV_QC$year), 
                  selected = 2024),
      
      selectInput("homesite_name", "Select Homesite:",  
                  choices = unique(UAV_QC$homesite_name), 
                  selected = "", 
                  multiple = FALSE),  
      
      uiOutput("field_name_ui"),  
      
      sliderInput("color_threshold", "Color threshold (Yield %):", 
                  min = 0, max = 200, value = 100, step = 10),  
      
      sliderInput("ccvu_breaks", "CCVU range:", 
                  min = 0, max = 0.5, value = 0.1, step = 0.1),
      sliderInput("iemeg_breaks", "IEMEG range:", 
                  min = 0, max = 50, value = 10, step = 10)
      # Suppression du bouton "Apply Filters"
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 textOutput("total_plots"),
                 plotOutput("yield_plot", height = "850px", width = "1200px")),
        tabPanel("Table", 
                 DTOutput("filtered_table"))  # Ajout du tableau
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Filtrer les field_name selon le homesite_name sélectionné
  output$field_name_ui <- renderUI({
    filtered_data <- UAV_QC %>%
      filter(homesite_name == input$homesite_name)  
    
    selectInput("field_name", "Select Field Name:",  
                choices = unique(filtered_data$field_name), 
                selected = "", 
                multiple = FALSE)  
  })
  
  # Réactiver les données filtrées
  filtered_data <- reactive({
    UAV_QC %>%
      filter(year == input$year,
             homesite_name == input$homesite_name,  
             field_name == input$field_name,  
             Filter != "Discard",              
             !is.na(CCVU),                    
             !is.na(IEMEG)) %>%
      group_by(set_name, field_name) %>%
      mutate(
        mean_yield = mean(Yield, na.rm = TRUE),  
        yield_percentage = (Yield / mean_yield) * 100  # Calculer yield_percentage ici
      ) %>%
      ungroup()  # Dégroupage pour éviter des problèmes lors de l'affichage
  })
  
  # Afficher le nombre total de plots après filtrage
  output$total_plots <- renderText({
    total_plots <- nrow(filtered_data())  
    paste("Total plot number:", total_plots)
  })
  
  # Générer le graphique
  output$yield_plot <- renderPlot({
    req(filtered_data())  
    
    result <- filtered_data() %>%
      group_by(set_name, field_name) %>%
      mutate(
        CCVU_class = cut(CCVU, breaks = seq(0, 5, by = input$ccvu_breaks), include.lowest = TRUE),
        IEMEG_class = cut(IEMEG, breaks = seq(0, 300, by = input$iemeg_breaks), include.lowest = TRUE),
        Combined_class = paste(CCVU_class, IEMEG_class, sep = "_")
      ) %>%
      group_by(Combined_class) %>%
      mutate(
        percentage_is_deactivated = sum(IsDeactivated == TRUE) / n() * 100
      ) %>%
      ungroup() %>%
      group_by(CCVU_class, IEMEG_class) %>%
      summarise(
        Mean_Yield_Percentage = mean(yield_percentage, na.rm = TRUE),
        Count = n(),
        Total = nrow(result),
        Percentage = (Count / Total),
        Deactivated_Count = sum(IsDeactivated == TRUE),
        Deactivated_Percentage = (Deactivated_Count / Count) * 100,
        .groups = "drop"
      )
    
    ggplot(result, aes(x = CCVU_class, y = IEMEG_class, fill = Mean_Yield_Percentage)) +
      geom_tile(color = "white") +
      geom_text(
        aes(label = sprintf("Yield: %.1f%%\nPct: %.1f%%\nDeact: %.1f%%", 
                            Mean_Yield_Percentage, Percentage, Deactivated_Percentage)),
        size = 4  
      ) +
      scale_fill_gradientn(
        colors = c("red", "grey", "green"),
        values = scales::rescale(c(50, input$color_threshold, 140)),  
        name = "Yield %"
      ) +
      labs(
        x = "CCVU",
        y = "IEMEG",
        fill = "Moyenne Yield",
        title = "Moyenne de Yield (%) et distribution des classes"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  
        axis.text.y = element_text(size = 14),  
        axis.title = element_text(face = "bold", size = 16),  
        plot.title = element_text(size = 18, face = "bold")  
      )
  })
  
  # Afficher le tableau de données filtrées
  output$filtered_table <- renderDT({
    req(filtered_data())
    
    filtered_data() %>%
      mutate(
        Yield = round(Yield),  # Arrondir Yield
        yield_percentage = round(yield_percentage)  # Arrondir yield_percentage
      ) %>%
      arrange(desc(CCVU)) %>%  # Trier par CCVU décroissant
      select(year, homesite_name, field_name, CCVU, IEMEG, 
             Yield, yield_percentage, columns, ranges, IsDeactivated, set_name, deactivationreason_coded) 
  }, options = list(pageLength = 10))  # Options pour le tableau
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)