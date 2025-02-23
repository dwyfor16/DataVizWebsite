if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(scales)){install.packages("scales"); library(scales)}
if(!require(sysfonts)){install.packages("sysfonts"); library(sysfonts)}
if(!require(showtext)){install.packages("showtext"); library(showtext)}
if(!require(ggpp)){install.packages("ggpp"); library(ggpp)}
if(!require(grid)){install.packages("grid"); library(grid)}
if(!require(magick)){install.packages("magick"); library(magick)}
if(!require(shiny)){install.packages("shiny"); library(shiny)}
if(!require(shinythemes)){install.packages("shinythemes"); library(shinythemes)}
if(!require(reactable)){install.packages("reactable"); library(reactable)}
if(!require(plotly)){install.packages("plotly"); library(plotly)}

# Font settings
font_add_google("Roboto Mono", "roboto_mono")
font <- "roboto_mono"
showtext_auto()

# Custom ggplot theme function
Custom_Style <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_text(family=font, size=20, face="bold", color="#FFFFFF"),
    plot.subtitle = ggplot2::element_text(family=font, size=20, color="#FFFFFF"),
    plot.caption = ggplot2::element_text(family=font, size=12, color="#FFFFFF"),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(family=font, size=12, face="bold", color="#FFFFFF"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font, size=9, color="#FFFFFF"),
    axis.text = ggplot2::element_text(family = font, size=10, color="#FFFFFF"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10), size = 8),
    axis.line = ggplot2::element_line(colour = alpha('#FFFFFF', 0.5), size = 0.5),
    axis.title = ggplot2::element_text(family=font, size=12, face="bold", color="#FFFFFF"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = element_rect(fill = "#073642", color = "#073642"),
    plot.background = element_rect(fill = "#073642", color = "#073642"),
    legend.background = element_rect(fill = "#073642", color = "#073642")
  )
}

# Color settings
winner_colors <- c(
  "Wales" = "darkred", "Ireland" = "#00985A", "Scotland" = "#000080", 
  "England" = "white", "France" = "#21304D", "Italy" = "#1f4fa2", 
  "Australia" = alpha("#f0af00", 0.8), "New Zealand" = "black", 
  "South Africa" = "#006400", "Argentina" = "#43A1D5", 
  "Draw" = "#4D4D4D"
)


# Load the data
results <- read.csv("results.csv")

# Prepare the data
TenNationResults <- results %>% 
  select(date, competition, home_team, away_team, home_score, away_score, world_cup) %>% 
  filter(!str_detect(competition, "warm")) %>%
  mutate(Who_Won = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    home_score == away_score ~ "Draw"
  )) %>% 
  mutate(Who_Won = factor(Who_Won, levels = names(winner_colors))) %>%
  mutate(decade = floor(year(as.Date(date)) / 10) * 10, .after = date) %>% 
  group_by(decade) %>% 
  arrange(date) %>%  
  ungroup()

# Get unique team names
AllTeams <- sort(unique(c(TenNationResults$home_team, TenNationResults$away_team)))

# UI
ui <- fluidPage(
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Roboto+Mono:ital,wght@0,100..700;1,100..700&display=swap');
    body {
      font-family: 'Roboto Mono', serif;
      background-color: #073642;  /* Set background color */
      color: white;  /* Set default text color to white */
    }
    .sidebar {
      background-color: #073642;  /* Set sidebar background color */
      color: black;  /* Set sidebar text color to black */
      font-family: 'Roboto Mono', serif;  /* Set sidebar font */
    }
    .navbar {
      background-color: #b58900;
      color: white;
      font-family: 'Roboto Mono', serif;  /* Set navbar font */
    }
    h1, h2, h3, h4, h5, h6 {
      color: white;
      font-family: 'Roboto Mono', serif;  /* Set heading fonts */
    }
    p {
      color: white;
      font-family: 'Roboto Mono', serif;  /* Set paragraph fonts */
    }
    
  ")),
  # theme = shinytheme("darkly"),  # Solar theme for cohesive dark design
  
  fluidRow(
    column(12,
           HTML("<h3>Welcome to the Rugby Results WebApp!</h3>"),
           HTML("<p>Here are some instructions to help you get started:</p>"),
           HTML("<ul>
                    <li>Use the sidebar to select different teams and see how they matchup.</li>
                    <li>Hover over the tiles in the graph to see the year the game was played.</li>
                    <li>You can download a png by clicking the 'Download Graph Button below'.</li>
                </ul>")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("home_team", "Select Home Team:", choices = AllTeams, selected = "Wales"),
      selectInput("away_team", "Select Away Team:", choices = AllTeams, selected = "England"),
      style = "background-color: #073642;",
      width = 5
      
      
    ),
    mainPanel(
      reactableOutput("resultTable"),  # Set height for table
      plotlyOutput(outputId = "resultPlot", height = "600px"),
      downloadButton("downloadPlot", "Download Graph"),
      width = 11
    )
    
  )
  
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    req(input$home_team, input$away_team)
    
    # Check if the same team is selected for both
    validate(
      need(input$home_team != input$away_team, 
           "Home Team and Away Team cannot be the same! Please select two different teams.")
    )
    
    TenNationResults %>%
      filter(
        (home_team == input$home_team & away_team == input$away_team) |
          (home_team == input$away_team & away_team == input$home_team)
      )
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Historical_Results_", input$home_team, "_vs_", input$away_team, ".png", sep = "")
    },
    content = function(file) {
      df <- filteredData() %>% 
        group_by(decade) %>% 
        mutate(game_number_in_decade = row_number()) %>% 
        ungroup()
      
      World_Cup_Games <- df %>% 
        filter(world_cup == "True") %>% 
        select(decade, game_number_in_decade)
      
      min_year <- format(as.Date(min(df$date)), "%Y")
      max_year <- format(as.Date(max(df$date)), "%Y")
      
      graph_winner <- unique(df$Who_Won) 
      graph_colours <- winner_colors[names(winner_colors) %in% graph_winner]
      
      p1 <- ggplot(df, aes(y = game_number_in_decade, x = factor(decade), fill = Who_Won)) +
        geom_tile(color = "black", x =2, y=2) +
        scale_fill_manual(values = graph_colours) +
        geom_text(aes(label = paste0(home_score, " - ", away_score)),  
                  color = alpha("#363636", 0.75),  
                  size = 4,
                  fontface = "bold", show.legend = FALSE) +
        scale_x_discrete(labels = function(x) paste0(x, "s")) +
        labs(
          x = "Decade",
          y = "Number of Games",
          fill = "Victor",
          title = paste0("Historical Results of ", input$home_team,  " versus ", input$away_team, " (", min_year,"-", max_year, ")"),
          subtitle = "Excludes World Cup Warm Up Games",
          caption = "Yellow = World Cup Game"
        ) +
        Custom_Style() +
        theme(text = element_text(family = "Roboto Mono"))
      
      if (nrow(World_Cup_Games) > 0) {
        p1 <- p1 + 
          geom_tile(data = World_Cup_Games, 
                    aes(x = factor(decade), y = game_number_in_decade), 
                    color = "yellow",
                    linewidth = 0.5, 
                    fill = NA,
                    height = 1, 
                    width = 1)  
      }
      
      ggsave(file, plot = p1, width = 12, height = 8, dpi = 300)
    }
  )
  
  
  output$resultPlot <- renderPlotly({
    df <- filteredData() %>% 
      group_by(decade) %>% 
      mutate(game_number_in_decade = row_number()) %>% 
      ungroup()
    
    # Identify the World Cup Games
    
    World_Cup_Games <- df %>% 
      filter(world_cup == "True") %>% 
      select(decade, game_number_in_decade)
    
    min_year <- format(as.Date(min(df$date)), "%Y")
    max_year <- format(as.Date(max(df$date)), "%Y")
    
    graph_winner <- unique(df$Who_Won) 
    graph_colours <- winner_colors[names(winner_colors) %in% graph_winner]
    
    p1 <- ggplot(df, aes(y = game_number_in_decade, x = factor(decade), fill = Who_Won)) +
      geom_tile(aes(text = paste0("Year: ", format(as.Date(date), "%Y"))), color = "black") +  # Black border around tiles
      scale_fill_manual(values = graph_colours) +  # Apply custom colors
      geom_text(aes(label = paste0(home_score, "-", away_score)),  
                color = alpha("darkgrey", 0.75),  
                size = 4,
                fontface = "bold", show.legend = FALSE) +
      scale_x_discrete(labels = function(x) paste0(x, "s")) +  # Add 's' to decade labels
      labs(
        x = "Decade",
        y = "Number of Games",
        fill = "Victor",
        title = paste0("Historical Results of ", input$home_team,  " versus ", input$away_team, " (", min_year,"-", max_year, ")"),
        subtitle = "Hover over cell to see year of the game",
        caption = "Magenta = World Cup Game (Excludes World Cup Warm Up Games)"
      ) +
      Custom_Style() +
      theme(text = element_text(family = "Roboto"))
    
    
    if (nrow(World_Cup_Games) > 0) {
      p1 <- p1 + 
        geom_tile(data = World_Cup_Games, 
                  aes(x = factor(decade), y = game_number_in_decade), 
                  color = "magenta",
                  linewidth = 0.75, 
                  fill = NA,
                  height = 1, # Adjust height to make the yellow boxes smaller
                  width = 1)   # Adjust width to make the yellow boxes smaller
    }
    
    
    p1_plotly <- ggplotly(p1, tooltip = "text") %>%
      layout(
        margin = list(l = 50, r = 50, b = 100, t = 50),
        autosize = TRUE,  # Allow the plot to resize automatically
        title = list(
          font = list(family = "Roboto", size = 28, color = "white")  # Set the title font to Roboto
        ),
        xaxis = list(
          title = list(
            text = "Decade",
            font = list(family = "Roboto", size = 14, color = "white")  # Set the x-axis title font to Roboto
          ),
          tickfont = list(family = "Roboto", size = 12, color = "white")  # Set the x-axis tick font to Roboto
        ),
        yaxis = list(
          title = list(
            text = "Number of Games",
            font = list(family = "Roboto", size = 14, color = "white")  # Set the y-axis title font to Roboto
          ),
          tickfont = list(family = "Roboto", size = 12, color = "white")  # Set the y-axis tick font to Roboto
        ),
        legend = list(
          title = list(
            text = "Team",
            font = list(family = "Roboto", size = 14, color = "white")  # Set the legend title font to Roboto
          ),
          font = list(family = "Roboto", size = 12, color = "white")  # Set the legend items font to Roboto
        ),
        annotations = list(
          x = 1, y = 0, text = "Magenta = World Cup Game (Excludes World Cup Warm Up Games)",  # Change the position here
          xref = 'paper', yref = 'paper', showarrow = FALSE,
          xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
          font = list(family = "Roboto", size = 10, color = "magenta")
        )
      )
    
    
    
  })
  
  # Render Styled Table with reactable
  output$resultTable <- renderReactable({
    df <- filteredData()
    
    table_data <- df %>%
      filter(Who_Won != "Draw") %>%
      group_by(Who_Won) %>%
      summarise(
        `Number of Games Won` = n(),
        `Biggest Win (Margin)` = max(abs(home_score - away_score), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        `Total Games Played` = nrow(df),
        `Win Percentage` = (`Number of Games Won` / `Total Games Played`) * 100
      ) %>%
      arrange(desc(`Number of Games Won`)) %>%
      rename(Country = Who_Won) %>%
      mutate(`Win Percentage` = percent(`Win Percentage` / 100))
    
    reactable(
      table_data,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      theme = reactableTheme(
        color = "#FFFFFF",
        backgroundColor = "#073642",
        borderColor = "#073642",
        stripedColor = "#073642",
        highlightColor = "#073642",
        cellPadding = "8px 12px",
        style = list(fontFamily = "Arial", fontSize = "16px")
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
