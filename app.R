# install.packages(c("shiny", "dplyr", "ggplot2", "DT"))

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Lire le fichier CSV
UAV_QC <- read.csv("UAV_QC.csv")

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
                  min = 0, max = 200, value = 100, step = 1),  
      
      sliderInput("ccvu_breaks", "CCVU range:", 
                  min = 0, max = 0.5, value = 0.1, step = 0.05),
      sliderInput("iemeg_breaks", "IEMEG range:", 
                  min = 0, max = 50, value = 10, step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 textOutput("total_plots"),
                 textOutput("mean_ccvu"),  
                 plotOutput("yield_plot", height = "850px", width = "1200px")),
        
        tabPanel("Table", 
                 DTOutput("filtered_data_table"))  # Ajout d'un tableau pour les données filtrées
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
                multiple = FALSE)  
  })
  
  # Réactiver les données filtrées
  filtered_data <- reactive({
    req(input$field_name)  # Assurez-vous que field_name est sélectionné
    
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
        yield_percentage = (Yield / mean_yield) * 100  
      ) %>%
      ungroup()  
  })
  
  # Afficher le nombre total de plots après filtrage
  output$total_plots <- renderText({
    total_plots <- nrow(filtered_data())  
    paste("Total plot number:", total_plots)
  })
  
  # Afficher le CCVU moyen
  output$mean_ccvu <- renderText({
    req(filtered_data())  # Assurez-vous que les données filtrées sont disponibles
    
    mean_ccvu <- mean(filtered_data()$CCVU, na.rm = TRUE)  # Calculer le CCVU moyen
    paste("Mean CCVU:", round(mean_ccvu, 2))  # Afficher le CCVU moyen arrondi à 2 décimales
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
        Total = n(),  # Remplacez nrow(result) par n()
        Deactivated_Count = sum(IsDeactivated == TRUE),
        Deactivated_Percentage = (Deactivated_Count / Count) * 100,
        Percentage = (Count / sum(Count)) * 100,  # Calculer le pourcentage par rapport au total
        .groups = "drop"
      ) %>%
      filter(Count > 1)  # Exclure les classes avec Count = 1
    
    # Vérifier si le résultat est vide après le filtrage
    if (nrow(result) == 0) {
      ggplot() + 
        labs(title = "Aucune donnée à afficher") + 
        theme_minimal()
    } else {
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
        geom_vline(xintercept = 4.5, linetype = "dashed", color = "blue") +
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
    }
  })
  
  # Afficher le tableau des données filtrées
  output$filtered_data_table <- renderDT({
    req(filtered_data())  # Assurez-vous que les données filtrées sont disponibles

    
    filtered_data() %>%
      select(field_name, columns, ranges, year, Yield, UAV_SC, CCVU, FNSI, IEMEG, deactivationreason_coded) %>%
      datatable(options = list(pageLength = 100, autoWidth = TRUE), rownames = FALSE)  
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)