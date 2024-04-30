library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

energy_forecast_data <- read_csv("/Users/richardokeefe/Desktop/Randomization_Senior_Project/YearvsForecast/Data/energy_forecast_data.csv") |>
  arrange(Year) |>
  mutate(Sector_fuel = case_when(Sector_fuel == "COM" ~ "Commercial",
                                 Sector_fuel == "IND" ~ "Industrial",
                                 Sector_fuel == "RES" ~ "Residential",
                                 Sector_fuel == "TRANS" ~ "Transportation",
                                 Sector_fuel == "BIO" ~ "Biomass",
                                 Sector_fuel == "COAL" ~ "Coal",
                                 Sector_fuel == "HYDP" ~ "Hydropower",
                                 Sector_fuel == "LF" ~ "Liquid fuels",
                                 Sector_fuel == "LFOP" ~ "Liquid fuels and other petroleum",
                                 Sector_fuel == "NP" ~ "Nuclear power",
                                 Sector_fuel == "ORE" ~ "Other renewable energy",
                                 Sector_fuel == "PP" ~ "Petroleum products",
                                 Sector_fuel == "RE" ~ "Renewable energy",
                                 Sector_fuel == "CLLC" ~ "Crude oil lease condensate",
                                 Sector_fuel == "DNG" ~ "Dry natural gas",
                                 Sector_fuel == "BIOWOTH" ~ "Biomass/wood/other",
                                 Sector_fuel == "BMW" ~ "Biogenic municipal waste",
                                 Sector_fuel == "GEO" ~ "Geothermal",
                                 Sector_fuel == "HYDRO" ~ "Conventional hydropower",
                                 Sector_fuel == "MSW" ~ "MSW",
                                 Sector_fuel == "PV" ~ "Solar PV",
                                 Sector_fuel == "ST" ~ "Solar thermal",
                                 Sector_fuel == "WIND" ~ "Wind",
                                 Sector_fuel == "OTH" ~ "Other consumption",
                                 .default = "Unknown"),
         ConsProd = case_when(ConsProd == "consumption" ~ "Consumption",
                              ConsProd == "production" ~ "Production",
                              ConsProd == "regen" ~ "Renewable electricity generation"))

Actual_data = energy_forecast_data |>
  filter(Actual_forecast == "Actual") |>
  group_by(Year, ConsProd, Sector_fuel) |>
  slice_max(AEOyear) |>
  select(-Actual_forecast, -AEOyear) |>
  ungroup() |>
  rename(Actual = n)

Forecast_data = energy_forecast_data |>
  filter(Actual_forecast == "Forecast") |>
  select(-Actual_forecast) |>
  rename(Forecast = n)

Joined_data = left_join(Forecast_data, Actual_data)

Actual_data_Plot4 = energy_forecast_data |>
  filter(Actual_forecast == "Actual") |>
  group_by(Year, ConsProd, Sector_fuel) |>
  slice_max(AEOyear) |>
  mutate(Forecast_Year = Actual_forecast) |>
  ungroup()

Actual_data_Plot4_Reduced = Actual_data_Plot4 |>
  select(-Actual_forecast, -AEOyear, -Forecast_Year) |>
  rename(Actual_n = n)

Forecast_data_Plot4 = energy_forecast_data |>
  filter(Actual_forecast == "Forecast") |>
  mutate(Forecast_Year = as.character(Year))

Forecast_data_Plot4 = left_join(Forecast_data_Plot4, Actual_data_Plot4_Reduced)

Joined_data_Plot4 = bind_rows(Forecast_data_Plot4, Actual_data_Plot4)

ui <- navbarPage(
  titlePanel(""),
  tabPanel("Plots by Single Forecast Years",
           sidebarLayout(
             position = "left",
             sidebarPanel(
               h3("Scatterplot inputs"),
               br(),
               selectInput(inputId = "Year_Plot1",
                           label = "Select year to show forecasts for",
                           choices = unique(filter(Joined_data, 
                                                   Actual != "NA" &
                                                   Forecast != "NA")$Year)),
               br(),
               selectInput(inputId = "ConsProd_Plot1",
                           label = "Choose to show data on consumption, production, or renewable electricity generation",
                           choices = unique(energy_forecast_data$ConsProd)),
               br(),
               selectInput(inputId = "Sector_fuel_Plot1",
                           label = "Choose sector to show data for",
                           choices = unique(energy_forecast_data$Sector_fuel)),
               br(),
               selectInput(inputId = "Response_Plot1",
                           label = "Select response variable",
                           choices = c("Energy forecast (Quad BTU)" = 0,
                                       "Energy forecast error (Quad BTU)" = 1,
                                       "Energy forecast percent error" = 2))),
             mainPanel(plotlyOutput("scatterplot1"))
           )),
  tabPanel("Plots by Multiple Forecast Years",
           sidebarLayout(
             position = "left",
             sidebarPanel(
               h3("Scatterplot inputs"),
               br(),
               selectizeInput(inputId = "Year_Plot4",
                              label = "Select years to show forecasts for (max 5 years at once)",
                              selected = 1983,
                              multiple = TRUE,
                              choices = unique(filter(Joined_data, 
                                                      Actual != "NA" &
                                                        Forecast != "NA")$Year),
                              options = list(maxItems = 5)),
               br(),
               selectInput(inputId = "ConsProd_Plot4",
                           label = "Choose to show data on consumption, production, or renewable electricity generation",
                           choices = unique(energy_forecast_data$ConsProd)),
               br(),
               selectInput(inputId = "Sector_fuel_Plot4",
                           label = "Choose sector to show data for",
                           choices = unique(energy_forecast_data$Sector_fuel)),
               br(),
               selectInput(inputId = "Response_Plot4",
                           label = "Select response variable",
                           choices = c("Energy forecast (Quad BTU)" = 0,
                                       "Energy forecast error (Quad BTU)" = 1,
                                       "Energy forecast percent error" = 2))),
             mainPanel(plotlyOutput("scatterplot4"))
           )),
  tabPanel("Plots by Sector",
           sidebarLayout(
             position = "left",
             sidebarPanel(
               h3("Scatterplot inputs"),
               br(),
               selectInput(inputId = "ConsProd_Plot2",
                           label = "Choose to show data on consumption, production, or renewable electricity generation",
                           choices = unique(energy_forecast_data$ConsProd)),
               br(),
               selectInput(inputId = "Sector_fuel_Plot2", 
                           label = "Choose sector to show data for",
                           choices = unique(energy_forecast_data$Sector_fuel)),
               br(),
               sliderInput(inputId = "f",
                           label = "Select the number of years into the future from each year a forecast was made to compute forecast errors from",
                           min = 1, max = 20, value = 1),
               br(),
               selectInput(inputId = "Response_Plot2",
                           label = "Select response variable",
                           choices = c("Energy forecast error (Quad BTU)" = 0,
                                       "Energy forecast percent error" = 1))),
             mainPanel(plotlyOutput("scatterplot2"))
             )),
  tabPanel("Plots by Consumption, Production, and Renewable Electricity Generation",
           sidebarLayout(
             position = "left",
             sidebarPanel(
               h3("Scatterplot inputs"),
               br(),
               selectInput(inputId = "ConsProd_Plot3",
                           label = "Choose to show data on consumption, production, or renewable electricity generation",
                           choices = unique(energy_forecast_data$ConsProd)),
             ),
             mainPanel(plotlyOutput("scatterplot3"))
             ))
)

server <- function(input, output, session) {
  
  observe({
    u = filter(Joined_data,
               Year %in% input$Year_Plot4 &
                 Actual != "NA" &
                 Forecast != "NA")
    updateSelectInput(session,
                      "ConsProd_Plot4",
                      label = "Choose to show data on consumption, production, or renewable energy generation",
                      choices = unique(u$ConsProd))
  })
  
  observe({
    v = filter(Joined_data,
               Year %in% input$Year_Plot4 &
                 ConsProd == input$ConsProd_Plot4 &
                 Actual != "NA" &
                 Forecast != "NA" &
                 Sector_fuel != "Unknown")
    updateSelectInput(session,
                      "Sector_fuel_Plot4",
                      label = "Choose sector to show data for",
                      choices = unique(v$Sector_fuel))
  })
  
  observe({
    w = filter(Joined_data,
               ConsProd == input$ConsProd_Plot2 &
                 Actual != "NA" &
                 Forecast != "NA" &
                 Sector_fuel != "Unknown")
    updateSelectInput(session,
                      "Sector_fuel_Plot2",
                      label = "Choose sector to show data for",
                      choices = unique(w$Sector_fuel))
  })
  
  observe({
    x = filter(Joined_data,
               Year == input$Year_Plot1 &
                 Actual != "NA" &
                 Forecast != "NA")
    updateSelectInput(session,
                      "ConsProd_Plot1",
                      label = "Choose to show data on consumption, production, or renewable energy generation",
                      choices = unique(x$ConsProd))
  })
  
  observe({
    y = filter(Joined_data,
               Year == input$Year_Plot1 &
                 ConsProd == input$ConsProd_Plot1 &
                 Actual != "NA" &
                 Forecast != "NA" &
                 Sector_fuel != "Unknown")
    updateSelectInput(session,
                      "Sector_fuel_Plot1",
                      label = "Choose sector to show data for",
                      choices = unique(y$Sector_fuel))
  })
  
  observe({
    z = Joined_data |> 
      mutate(YearminusAEOyear = Year - AEOyear) |> 
      filter(ConsProd == input$ConsProd_Plot2 &
               Sector_fuel == input$Sector_fuel_Plot2 &
               Actual != "NA" &
               Forecast != "NA")
    updateSliderInput(session,
                      "f",
                      label = "Select the number of years into the future from each year a forecast was made to compute forecast errors from",
                      min = 1, max = max(z$YearminusAEOyear), value = 1)
  })
  
  observe({
    updateSelectInput(session,
                      "Response_Plot1",
                      label = "Select response variable",
                      choices = case_when(input$ConsProd_Plot1 == "Renewable electricity generation" ~ c("Energy forecast (Billion kWh)" = 0,
                                                                                                         "Energy forecast error (Billion kWh)" = 1,
                                                                                                         "Energy forecast percent error" = 2),
                                          input$ConsProd_Plot1 != "Renewable electricity generation" ~ c("Energy forecast (Quad BTU)" = 0,
                                                                                                         "Energy forecast error (Quad BTU)" = 1,
                                                                                                         "Energy forecast percent error" = 2)))
  })
  
  observe({
    updateSelectInput(session,
                      "Response_Plot2",
                      label = "Select response variable",
                      choices = case_when(input$ConsProd_Plot2 == "Renewable electricity generation" ~ c("Energy forecast error (Billion kWh)" = 0,
                                                                                                         "Energy forecast percent error" = 1),
                                          input$ConsProd_Plot2 != "Renewable electricity generation" ~ c("Energy forecast error (Quad BTU)" = 0,
                                                                                                         "Energy forecast percent error" = 1)))
  })
  
  observe({
    updateSelectInput(session,
                      "Response_Plot4",
                      label = "Select response variable",
                      choices = case_when(input$ConsProd_Plot4 == "Renewable electricity generation" ~ c("Energy forecast (Billion kWh)" = 0,
                                                                                                         "Energy forecast error (Billion kWh)" = 1,
                                                                                                         "Energy forecast percent error" = 2),
                                          input$ConsProd_Plot4 != "Renewable electricity generation" ~ c("Energy forecast (Quad BTU)" = 0,
                                                                                                         "Energy forecast error (Quad BTU)" = 1,
                                                                                                         "Energy forecast percent error" = 2)))
  })
  
  output$scatterplot1 <- renderPlotly(ggplotly(ggplot(filter(energy_forecast_data,
                                                  Year == input$Year_Plot1 &
                                                  ConsProd == input$ConsProd_Plot1 &
                                                  Sector_fuel == input$Sector_fuel_Plot1 &
                                                  Actual_forecast == "Forecast"),
                                            aes(x = AEOyear,
                                                y = case_when(input$Response_Plot1 == 0 ~ n,
                                                              input$Response_Plot1 == 1 ~ n - slice_max(filter(energy_forecast_data,
                                                                                                               Year == input$Year_Plot1 &
                                                                                                               ConsProd == input$ConsProd_Plot1 &
                                                                                                               Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                               Actual_forecast == "Actual"),
                                                                                                        AEOyear)$n,
                                                              input$Response_Plot1 == 2 ~ (n - slice_max(filter(energy_forecast_data,
                                                                                                                Year == input$Year_Plot1 &
                                                                                                                ConsProd == input$ConsProd_Plot1 &
                                                                                                                Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                Actual_forecast == "Actual"),
                                                                                                         AEOyear)$n)/slice_max(filter(energy_forecast_data,
                                                                                                                                      Year == input$Year_Plot1 &
                                                                                                                                      ConsProd == input$ConsProd_Plot1 &
                                                                                                                                      Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                                      Actual_forecast == "Actual"),
                                                                                                                               AEOyear)$n),
                                                text = case_when(input$Response_Plot1 == 0 & input$ConsProd_Plot1 != "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nForecast for", Year, ":", round(n, 2), "Quad BTU"),
                                                                 input$Response_Plot1 == 0 & input$ConsProd_Plot1 == "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nForecast for", Year, ":", round(n, 2), "Billion kWh"),
                                                                 input$Response_Plot1 == 1 & input$ConsProd_Plot1 != "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nError in forecast for", Year, ":", round(n - slice_max(filter(energy_forecast_data,
                                                                                                                                                                             Year == input$Year_Plot1 &
                                                                                                                                                                               ConsProd == input$ConsProd_Plot1 &
                                                                                                                                                                               Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                                                                               Actual_forecast == "Actual"),
                                                                                                                                                                      AEOyear)$n, 2), "Quad BTU"),
                                                                 input$Response_Plot1 == 1 & input$ConsProd_Plot1 == "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nError in forecast for", Year, ":", round(n - slice_max(filter(energy_forecast_data,
                                                                                                                                                                                          Year == input$Year_Plot1 &
                                                                                                                                                                                            ConsProd == input$ConsProd_Plot1 &
                                                                                                                                                                                            Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                                                                                            Actual_forecast == "Actual"),
                                                                                                                                                                                   AEOyear)$n, 2), "Billion kWh"),
                                                                 input$Response_Plot1 == 2 ~ paste("AEO year:", AEOyear, "\nPercent error in forecast for", Year, ":", round((n - slice_max(filter(energy_forecast_data,
                                                                                                                                                                              Year == input$Year_Plot1 &
                                                                                                                                                                                ConsProd == input$ConsProd_Plot1 &
                                                                                                                                                                                Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                                                                                Actual_forecast == "Actual"),
                                                                                                                                                                       AEOyear)$n)/slice_max(filter(energy_forecast_data,
                                                                                                                                                                                                    Year == input$Year_Plot1 &
                                                                                                                                                                                                      ConsProd == input$ConsProd_Plot1 &
                                                                                                                                                                                                      Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                                                                                                      Actual_forecast == "Actual"),
                                                                                                                                                                                             AEOyear)$n, 2))))) +
                                        geom_point() +
                                        geom_hline(aes(yintercept = if_else(input$Response_Plot1 < 1, slice_max(filter(energy_forecast_data,
                                                                                                                          Year == input$Year_Plot1 &
                                                                                                                          ConsProd == input$ConsProd_Plot1 &
                                                                                                                          Sector_fuel == input$Sector_fuel_Plot1 &
                                                                                                                          Actual_forecast == "Actual"),
                                                                                                                AEOyear)$n,
                                                                             0))) +
                                        labs(x = "AEO year",
                                             y = case_when(input$Response_Plot1 == 0 ~ "Energy forecast",
                                                           input$Response_Plot1 == 1 ~ "Energy forecast error",
                                                           input$Response_Plot1 == 2 ~ "Energy forecast percent error")), 
                                        tooltip = "text"))
  
  output$scatterplot4 <- renderPlotly(ggplotly(ggplot(filter(Joined_data_Plot4,
                                                             ((Year %in% input$Year_Plot4 &
                                                                 Actual_forecast == "Forecast") |
                                                                (Year <= max(input$Year_Plot4) &
                                                                   Actual_forecast == "Actual")) &
                                                               ConsProd == input$ConsProd_Plot4 &
                                                               Sector_fuel == input$Sector_fuel_Plot4),
                                                      aes(x = if_else(Actual_forecast == "Forecast",
                                                                      AEOyear,
                                                                      Year),
                                                          y = case_when(input$Response_Plot4 == 0 ~ n,
                                                                        input$Response_Plot4 == 1 & Actual_forecast == "Forecast" ~ n - Actual_n,
                                                                        input$Response_Plot4 == 2 & Actual_forecast == "Forecast" ~ (n - Actual_n)/Actual_n),
                                                          text = case_when(input$Response_Plot4 == 0 & Actual_forecast == "Forecast" & input$ConsProd_Plot4 != "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nForecast for", Year, ":", round(n, 2), "Quad BTU"),
                                                                           input$Response_Plot4 == 0 & Actual_forecast == "Forecast" & input$ConsProd_Plot4 == "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nForecast for", Year, ":", round(n, 2), "Billion kWh"),
                                                                           input$Response_Plot4 == 0 & Actual_forecast == "Actual" & input$ConsProd_Plot4 != "Renewable electricity generation" ~ paste("Actual", input$Sector_fuel_Plot4, input$ConsProd_Plot4, "for", Year, ":", round(n, 2), "Quad BTU"),
                                                                           input$Response_Plot4 == 0 & Actual_forecast == "Actual" & input$ConsProd_Plot4 == "Renewable electricity generation" ~ paste("Actual", input$Sector_fuel_Plot4, input$ConsProd_Plot4, "for", Year, ":", round(n, 2), "Billion kWh"),
                                                                           input$Response_Plot4 == 1 & Actual_forecast == "Forecast" & input$ConsProd_Plot4 != "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nError in forecast for", Year, ":", round(n - Actual_n, 2), "Quad BTU"),
                                                                           input$Response_Plot4 == 1 & Actual_forecast == "Forecast" & input$ConsProd_Plot4 == "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nError in forecast for", Year, ":", round(n - Actual_n, 2), "Billion kWh"),
                                                                           input$Response_Plot4 == 2 & Actual_forecast == "Forecast" & input$ConsProd_Plot4 != "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nPercent error in forecast for", Year, ":", round((n - Actual_n)/Actual_n, 2), "Quad BTU"),
                                                                           input$Response_Plot4 == 2 & Actual_forecast == "Forecast" & input$ConsProd_Plot4 == "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nPercent error in forecast for", Year, ":", round((n - Actual_n)/Actual_n, 2), "Billion kWh")),
                                                          color = Forecast_Year)) +
                                                 geom_line(aes(group = Forecast_Year)) +
                                                 geom_point() +
                                                 labs(x = "AEO year",
                                                      y = case_when(input$Response_Plot4 <= 1 & input$ConsProd_Plot4 != "Renewable electricity generation" ~ "Quad BTU",
                                                                    input$Response_Plot4 <= 1 & input$ConsProd_Plot4 == "Renewable electricity generation" ~ "Billion kWh",
                                                                    input$Response_Plot4 == 2 ~ "Percent error")), 
                                               tooltip = "text"))
  
  output$scatterplot2 <- renderPlotly(ggplotly(ggplot(filter(Joined_data,
                                                  Year == AEOyear + input$f &
                                                  ConsProd == input$ConsProd_Plot2 &
                                                  Sector_fuel == input$Sector_fuel_Plot2),
                                           aes(x = AEOyear,
                                               y = case_when(input$Response_Plot2 == 0 ~ Forecast - Actual,
                                                             input$Response_Plot2 == 1 ~ (Forecast - Actual)/Actual),
                                               text = case_when(input$Response_Plot2 == 0 & input$ConsProd_Plot2 != "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nError in forecast for", Year, ":", round(Forecast - Actual, 2), "Quad BTU"),
                                                                input$Response_Plot2 == 0 & input$ConsProd_Plot2 == "Renewable electricity generation" ~ paste("AEO year:", AEOyear, "\nError in forecast for", Year, ":", round(Forecast - Actual, 2), "Billion kWh"),
                                                                input$Response_Plot2 == 1 ~ paste("AEO year:", AEOyear, "\nPercent error in forecast for", Year, ":", round((Forecast - Actual)/Actual, 2))))) +
                                    geom_point() +
                                    geom_hline(aes(yintercept = 0)) +
                                    labs(x = "AEO year",
                                         y = if_else(input$Response_Plot2 == 0,
                                                     paste("Size of", input$f, "year into the future energy forecast error"),
                                                     paste("Size of", input$f, "year into the future energy forecast percent error"))),
                                    tooltip = "text"))
  
  output$scatterplot3 <- renderPlotly(ggplotly(ggplot(filter(Joined_data,
                                                  ConsProd == input$ConsProd_Plot3),
                                           aes(x = Actual,
                                               y = (Forecast - Actual)/Actual,
                                               text = paste("AEO year:", AEOyear, "\nForecast year:", Year, "\nActual resource size:", Actual, "\nPercent error:", round((Forecast - Actual)/Actual, 2), "\nResource:", Sector_fuel))) +
                                    geom_point() +
                                    labs(x = if_else(input$ConsProd_Plot3 == "Renewable electricity generation", 
                                             "Resource size (Billion kWh)",
                                             "Resource size (Quad BTU)"),
                                         y = "Energy forecast percent error"),
                                    tooltip = "text"))
}

shinyApp(ui = ui, server = server)