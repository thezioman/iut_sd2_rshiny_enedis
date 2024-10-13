# app.R

library(shiny)
library(shinydashboard)
library(shinymanager)
library(shinyWidgets) 
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(shinyjs)
library(htmlwidgets)
library(webshot)


# Charger les données 
df <-  read.csv("C:/Users/ugo/Downloads/logements_69.csv", header = TRUE, dec = ".", sep = ",", stringsAsFactors = FALSE)

# Prétraitement des données
df$Emission_GES_5_usages <- as.numeric(df$Emission_GES_5_usages)
df$Date_réception_DPE <- as.Date(df$Date_réception_DPE, format = "%Y-%m-%d")

# Conversion des coordonnées
df_map <- df %>%
  mutate(
    lat = as.numeric(sapply(strsplit(`X_geopoint`, ","), `[`, 1)),
    lon = as.numeric(sapply(strsplit(`X_geopoint`, ","), `[`, 2))
  )

# Simplification des coordonnées pour optimisation
df_map <- df_map %>%
  mutate_at(vars(lat, lon), round, digits = 5)  # Limiter la précision à 5 décimales

# Générer les choix pour le filtre des codes postaux
codepostent <- table(df$Code_postal_.BAN.)
codepost <- names(codepostent)
codepost_choices <- sort(unique(df$Code_postal_.BAN.))

# Définir les identifiants pour l'authentification
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin", "user"),
  stringsAsFactors = FALSE
)

# Interface Utilisateur (UI) avec shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de Bord Logements"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem("Résumé", tabName = "resume", icon = icon("dashboard")),
      menuItem("Carte", tabName = "carte", icon = icon("map")),
      menuItem("Analyses", tabName = "analyses", icon = icon("chart-bar"))
    ),
    hr(),
    # Filtre global pour le Code Postal
    pickerInput(
      inputId = "postal_code",
      label = "Sélectionnez le(s) Code(s) Postal(aux) :",
      choices = codepost_choices,
      selected = codepost_choices,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    ),
    # Bouton pour réinitialiser les filtres
    actionButton("reset_filters", "Réinitialiser les Filtres")
  ),
  dashboardBody(
    useShinyjs(),
    # CSS personnalisé pour ajuster la hauteur des boxes
    tags$head(
      tags$style(HTML("
        .small-box { height: 100px; }
      "))
    ),
    tabItems(
      # Onglet Résumé
      tabItem(tabName = "resume",
              fluidRow(
                # Value Boxes
                valueBoxOutput("nb_communes"),
                valueBoxOutput("moyenne_emission"),
                valueBoxOutput("chauffage_utilise"),
                valueBoxOutput("intervalle_dpe")
              ),
              fluidRow(
                box(
                  title = "Téléchargements",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  downloadButton("downloadData", "Télécharger les données")
                ),
                box(
                  title = "Diagramme en Barres des Étiquettes DPE",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("barplot"),
                  downloadButton("downloadBarplot", "Télécharger le Graphique (Barplot)")
                )
              ),
              fluidRow(
                box(
                  title = "Table de Données",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("dataTable")
                )
              )
      ),
      # Onglet Carte
      tabItem(tabName = "carte",
              fluidRow(
                box(
                  title = "Carte des Logements",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("map"),
                  textInput("search_address", "Rechercher une adresse:", ""),
                  actionButton("search_btn", "Rechercher")
                )
              )
              
      ),
      # Onglet Analyses
      tabItem(tabName = "analyses",
              fluidRow(
                box(
                  title = "Coût Total par Type d'Énergie",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("cout_vs_energie"),
                  downloadButton("download_cout_vs_energie", "Télécharger le Graphique")
                ),
                box(
                  title = "Étiquette DPE en Fonction du Type d'Habitation",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("dpe_vs_habitation"),
                  downloadButton("download_dpe_vs_habitation", "Télécharger le Graphique")
                )
              ),
              fluidRow(
                box(
                  title = "Type d'Énergie en Fonction de l'Étiquette DPE",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("energie_vs_dpe"),
                  downloadButton("download_energie_vs_dpe", "Télécharger le Graphique")
                ),
                box(
                  title = "Coût Total en Fonction du Type de Bâtiment",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("cout_vs_batiment"),
                  downloadButton("download_cout_vs_batiment", "Télécharger le Graphique")
                )
              ),
              fluidRow(
                box(
                  title = "Répartition des Types d'Énergie par Commune",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("energie_par_commune"),
                  downloadButton("download_energie_par_commune", "Télécharger le Graphique")
                ),
                box(
                  title = "Corrélation entre Qualité d'Isolation et Émissions GES",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("isolation_vs_ges"),
                  downloadButton("download_isolation_vs_ges", "Télécharger le Graphique")
                )
              ),
              fluidRow(
                box(
                  title = "Répartition des Bâtiments par Qualité d'Isolation",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("isolation_par_batiment"),
                  downloadButton("download_isolation_par_batiment", "Télécharger le Graphique")
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Authentification avec shinymanager
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Réinitialiser les filtres lorsque le bouton est cliqué
  observeEvent(input$reset_filters, {
    updatePickerInput(
      session,
      "postal_code",
      selected = codepost_choices
    )
  })
  
  # Filtrer les données en fonction du code postal sélectionné
  filtered_data <- reactive({
    req(input$postal_code) # Assure que le filtre n'est pas vide
    df %>%
      filter(Code_postal_.BAN. %in% input$postal_code)
  })
  
  # Value Boxes
  output$nb_communes <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$Nom__commune_.BAN.)),
      subtitle = "Nombre de Communes",
      icon = icon("city"),
      color = "aqua"
    )
  })
  
  output$moyenne_emission <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Emission_GES_5_usages, na.rm = TRUE)),
      subtitle = "Moyenne d'Émission GES",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$intervalle_dpe <- renderValueBox({
    valeur <- paste(min(filtered_data()$Date_réception_DPE, na.rm = TRUE), 
                    "à", 
                    max(filtered_data()$Date_réception_DPE, na.rm = TRUE))
    valueBox(
      value = valeur,
      subtitle = "Intervalle des Dates de Réception DPE",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })
  
  output$chauffage_utilise <- renderValueBox({
    chauffage <- table(filtered_data()$Type_énergie_principale_chauffage)
    most_used_heating <- names(which.max(chauffage))
    valueBox(
      value = most_used_heating,
      subtitle = "Moyen de Chauffage le Plus Utilisé",
      icon = icon("fire"),
      color = "red"
    )
  })
  
  # Diagramme en Barres des Étiquettes GES
  barplot_plot <- reactive({
    data_to_plot <- filtered_data()
    freq <- as.data.frame(table(data_to_plot$Etiquette_DPE))
    freq$Freq <- freq$Freq * 0.001
    
    ggplot(freq, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("darkgreen", "green", "lightgreen", "yellow", "orange", "darkorange", "red")) +
      labs(title = "Répartitions des Étiquettes DPE",
           x = "Étiquette DPE",
           y = "Nombre de Logements (en milliers)") +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#f0f0f0"),
        panel.background = element_rect(fill = "#f0f0f0"),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$barplot <- renderPlot({
    barplot_plot()
  })
  
  # Téléchargement du Diagramme en Barres
  output$downloadBarplot <- downloadHandler(
    filename = function() {
      paste("barplot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = barplot_plot(), device = "png")
    }
  )
  
  # Téléchargement des Données Filtrées
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("donnees_filtrees-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Table de Données Interactive
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Afficher la carte Leaflet
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(df_map$lon, na.rm = TRUE), lat = mean(df_map$lat, na.rm = TRUE), zoom = 12)
  })
  
  # Mise à jour des marqueurs avec leafletProxy et clustering
  observe({
    leafletProxy("map", data = df_map) %>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat,
        popup = ~paste0(
          "<b>Adresse: </b>", Adresse_.BAN., "<br>",
          "<b>Commune: </b>", Nom__commune_.BAN., "<br>",
          "<b>Étiquette GES: </b>", Etiquette_GES, "<br>",
          "<b>Étiquette DPE: </b>", Etiquette_DPE, "<br>",
          "<b>Coût total 5 usages: </b>", Coût_total_5_usages, "€", "<br>",
          "<b>Surface habitable: </b>", Surface_habitable_logement, "m²"
        ),
        clusterOptions = markerClusterOptions()  # Activation du clustering
      )
  })
  
  # Fonction de recherche d'adresse
  observeEvent(input$search_btn, {
    req(input$search_address)  # Assure que le champ de recherche n'est pas vide
    
    # Recherche d'adresses correspondant au texte saisi (insensible à la casse)
    result <- df_map %>%
      filter(grepl(tolower(input$search_address), tolower(Adresse_.BAN.), fixed = TRUE))
    
    if(nrow(result) == 0){
      showModal(modalDialog(
        title = "Aucun résultat trouvé",
        paste("Aucune adresse ne correspond à :", input$search_address),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # Utiliser le premier résultat trouvé
      selected <- result[1, ]
      
      # Centrer la carte sur l'adresse sélectionnée
      leafletProxy("map") %>%
        setView(lng = selected$lon, lat = selected$lat, zoom = 18) %>%
        addPopups(
          lng = selected$lon, 
          lat = selected$lat,
          popup = paste0(
            "<b>Adresse: </b>", selected$Adresse_.BAN., "<br>",
            "<b>Commune: </b>", selected$Nom__commune_.BAN., "<br>",
            "<b>Étiquette GES: </b>", selected$Etiquette_GES, "<br>",
            "<b>Étiquette DPE: </b>", selected$Etiquette_DPE, "<br>",
            "<b>Coût total 5 usages: </b>", selected$Coût_total_5_usages, "€", "<br>",
            "<b>Surface habitable: </b>", selected$Surface_habitable_logement, "m²"
          )
        )
    }
  })
  
  # Diagramme de Dispersion
  scatter_plot <- reactive({
    data_to_plot <- filtered_data()
    
    
    ggplot(data_to_plot, aes(x = Surface_habitable_logement, y = Emission_GES_5_usages)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Relation entre Surface Habitable et Emission GES",
           x = "Surface Habitable (m²)",
           y = "Emission GES (kg)") +
      theme_minimal()
  })
  
  output$scatterPlot <- renderPlot({
    scatter_plot()
  })
  
  # Téléchargement du Diagramme de Dispersion
  output$downloadScatter <- downloadHandler(
    filename = function() {
      paste("scatterplot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if(!is.null(scatter_plot())) {
        ggsave(file, plot = scatter_plot(), device = "png")
      }
    }
  )
  
  # Diagramme en Secteurs
  pie_chart <- reactive({
    data_to_plot <- filtered_data()
    pie_data <- as.data.frame(table(data_to_plot$Type_énergie_principale_chauffage))
    
    ggplot(pie_data, aes(x = "", y = Freq, fill = Var1)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Répartition des Types de Chauffage",
           fill = "Type de Chauffage") +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#f0f0f0"),
        text = element_text(color = "black"),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$pieChart <- renderPlot({
    pie_chart()
  })
  
  # Téléchargement du Diagramme en Secteurs
  output$downloadPie <- downloadHandler(
    filename = function() {
      paste("piechart-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = pie_chart(), device = "png")
    }
  )
  
  
  #  Coût Total par Type d'Énergie
  output$cout_vs_energie <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Type_énergie_principale_chauffage) %>%
      summarise(Cout_total_moyen = mean(Coût_total_5_usages, na.rm = TRUE))
    
    ggplot(plot_data, aes(x = Type_énergie_principale_chauffage, y = Cout_total_moyen, fill = Type_énergie_principale_chauffage)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Coût Total Moyen par Type d'Énergie",
           x = "Type d'Énergie",
           y = "Coût Total Moyen (€)") +
      theme(legend.position = "none")
  })
  
  output$download_cout_vs_energie <- downloadHandler(
    filename = function() { "cout_vs_energie.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Type_énergie_principale_chauffage) %>%
        summarise(Cout_total_moyen = mean(Coût_total_5_usages, na.rm = TRUE))
      
      png(file, width = 800, height = 600)
      print(
        ggplot(plot_data, aes(x = Type_énergie_principale_chauffage, y = Cout_total_moyen, fill = Type_énergie_principale_chauffage)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Coût Total Moyen par Type d'Énergie",
               x = "Type d'Énergie",
               y = "Coût Total Moyen (€)") +
          theme(legend.position = "none")
      )
      dev.off()
    }
  )
  
  # Étiquette DPE en Fonction du Type d'Habitation
  output$dpe_vs_habitation <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Type_bâtiment, Etiquette_DPE) %>%
      summarise(Count = n())
    
    ggplot(plot_data, aes(x = Type_bâtiment, y = Count, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Répartition des Étiquettes DPE par Type d'Habitation",
           x = "Type d'Habitation",
           y = "Nombre de Logements",
           fill = "Étiquette DPE") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_dpe_vs_habitation <- downloadHandler(
    filename = function() { "dpe_vs_habitation.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Type_bâtiment, Etiquette_DPE) %>%
        summarise(Count = n())
      
      png(file, width = 800, height = 600)
      print(
        ggplot(plot_data, aes(x = Type_bâtiment, y = Count, fill = Etiquette_DPE)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          labs(title = "Répartition des Étiquettes DPE par Type d'Habitation",
               x = "Type d'Habitation",
               y = "Nombre de Logements",
               fill = "Étiquette DPE") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      dev.off()
    }
  )
  
  # Type d'Énergie en Fonction de l'Étiquette DPE
  output$energie_vs_dpe <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Etiquette_DPE, Type_énergie_principale_chauffage) %>%
      summarise(Count = n())
    
    ggplot(plot_data, aes(x = Etiquette_DPE, y = Count, fill = Type_énergie_principale_chauffage)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Répartition des Types d'Énergie par Étiquette DPE",
           x = "Étiquette DPE",
           y = "Nombre de Logements",
           fill = "Type d'Énergie") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_energie_vs_dpe <- downloadHandler(
    filename = function() { "energie_vs_dpe.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Etiquette_DPE, Type_énergie_principale_chauffage) %>%
        summarise(Count = n())
      
      png(file, width = 800, height = 600)
      print(
        ggplot(plot_data, aes(x = Etiquette_DPE, y = Count, fill = Type_énergie_principale_chauffage)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          labs(title = "Répartition des Types d'Énergie par Étiquette DPE",
               x = "Étiquette DPE",
               y = "Nombre de Logements",
               fill = "Type d'Énergie") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      dev.off()
    }
  )
  
  # Coût Total en Fonction du Type de Bâtiment
  output$cout_vs_batiment <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Type_bâtiment) %>%
      summarise(Cout_total_moyen = mean(Coût_total_5_usages, na.rm = TRUE))
    
    ggplot(plot_data, aes(x = Type_bâtiment, y = Cout_total_moyen, fill = Type_bâtiment)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Coût Total Moyen par Type de Bâtiment",
           x = "Type de Bâtiment",
           y = "Coût Total Moyen (€)") +
      theme(legend.position = "none")
  })
  
  output$download_cout_vs_batiment <- downloadHandler(
    filename = function() { "cout_vs_batiment.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Type_bâtiment) %>%
        summarise(Cout_total_moyen = mean(Coût_total_5_usages, na.rm = TRUE))
      
      png(file, width = 800, height = 600)
      print(
        ggplot(plot_data, aes(x = Type_bâtiment, y = Cout_total_moyen, fill = Type_bâtiment)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Coût Total Moyen par Type de Bâtiment",
               x = "Type de Bâtiment",
               y = "Coût Total Moyen (€)") +
          theme(legend.position = "none")
      )
      dev.off()
    }
  )
  
  # Répartition des Types d'Énergie par Commune
  output$energie_par_commune <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Nom__commune_.BAN., Type_énergie_principale_chauffage) %>%
      summarise(Count = n())
    
    ggplot(plot_data, aes(x = Nom__commune_.BAN., y = Count, fill = Type_énergie_principale_chauffage)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(title = "Répartition des Types d'Énergie par Commune",
           x = "Commune",
           y = "Nombre de Logements",
           fill = "Type d'Énergie") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_energie_par_commune <- downloadHandler(
    filename = function() { "energie_par_commune.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Nom__commune_.BAN., Type_énergie_principale_chauffage) %>%
        summarise(Count = n())
      
      png(file, width = 1200, height = 800)
      print(
        ggplot(plot_data, aes(x = Nom__commune_.BAN., y = Count, fill = Type_énergie_principale_chauffage)) +
          geom_bar(stat = "identity", position = "stack") +
          theme_minimal() +
          labs(title = "Répartition des Types d'Énergie par Commune",
               x = "Commune",
               y = "Nombre de Logements",
               fill = "Type d'Énergie") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      dev.off()
    }
  )
  
  # Corrélation entre Qualité d'Isolation et Émissions GES
  output$isolation_vs_ges <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Qualité_isolation_enveloppe) %>%
      summarise(Emission_GES_moyenne = mean(Emission_GES_5_usages, na.rm = TRUE))
    
    ggplot(plot_data, aes(x = Qualité_isolation_enveloppe, y = Emission_GES_moyenne, fill = Qualité_isolation_enveloppe)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Émissions GES Moyennes par Qualité d'Isolation",
           x = "Qualité d'Isolation",
           y = "Émissions GES Moyennes (kg)") +
      theme(legend.position = "none")
  })
  
  output$download_isolation_vs_ges <- downloadHandler(
    filename = function() { "isolation_vs_ges.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Qualité_isolation_enveloppe) %>%
        summarise(Emission_GES_moyenne = mean(Emission_GES_5_usages, na.rm = TRUE))
      
      png(file, width = 800, height = 600)
      print(
        ggplot(plot_data, aes(x = Qualité_isolation_enveloppe, y = Emission_GES_moyenne, fill = Qualité_isolation_enveloppe)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Émissions GES Moyennes par Qualité d'Isolation",
               x = "Qualité d'Isolation",
               y = "Émissions GES Moyennes (kg)") +
          theme(legend.position = "none")
      )
      dev.off()
    }
  )
  
  # Répartition des Bâtiments par Qualité d'Isolation
  output$isolation_par_batiment <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Qualité_isolation_enveloppe, Type_bâtiment) %>%
      summarise(Count = n())
    
    ggplot(plot_data, aes(x = Type_bâtiment, y = Count, fill = Qualité_isolation_enveloppe)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Répartition des Bâtiments par Qualité d'Isolation",
           x = "Type de Bâtiment",
           y = "Nombre de Logements",
           fill = "Qualité d'Isolation") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_isolation_par_batiment <- downloadHandler(
    filename = function() { "isolation_par_batiment.png" },
    content = function(file) {
      plot_data <- filtered_data() %>%
        group_by(Qualité_isolation_enveloppe, Type_bâtiment) %>%
        summarise(Count = n())
      
      png(file, width = 800, height = 600)
      print(
        ggplot(plot_data, aes(x = Type_bâtiment, y = Count, fill = Qualité_isolation_enveloppe)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          labs(title = "Répartition des Bâtiments par Qualité d'Isolation",
               x = "Type de Bâtiment",
               y = "Nombre de Logements",
               fill = "Qualité d'Isolation") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      dev.off()
    }
  )
  
}

# Appliquer l'authentification à l'UI
ui_auth <- secure_app(ui, enable_admin = FALSE)

# Lancer l'application
shinyApp(ui = ui_auth, server = server)
