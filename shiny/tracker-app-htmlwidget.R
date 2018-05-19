library(shiny)
library(shinymaterial)
library(readr)
library(tidyverse)
library(shiny)
library(plotly)
library(DT)

curr <- read_csv("data/trackerCalcCurr.csv")
pre <- read_csv("data/trackerCalcPre.csv")
users <- read_csv("data/users.csv")

f <- function(x, y) {100 * (y / x - 1)}

ui <- material_page(
  title = "Customer Tracker App",
  nav_bar_color = "blue-grey lighten-1",
  tags$br(),
  material_row(
    material_column(
      width = 2,
      material_card(
        material_dropdown("seg", "Segment", c("Total", "Heavy", "Mainstream", "Focus1", "Focus2", "Specialty", "Diverse1", "Diverse2", "Other", "New"), color = "#78909c"),
        material_dropdown('grp', 'Group', c("Total", "Core", "Extra"), color = "#78909c"),
        material_radio_button("per", "Period", c("Week", "YTD"), color = "#78909c")
      )
    ),
    material_column(
      width = 10,
      material_row(
        material_card(
          title = "Percent Change by Week",
          plotlyOutput("plot")
        )
      ),
      material_row(
        material_card(
          title = "Data",
          dataTableOutput("data")
        )
      )
    )
  )
)


server <- function(input, output) {
  
  dat <- reactive({
    bind_cols(
      curr %>%
        filter(segment == input$seg) %>%
        select(ends_with(paste0(input$grp, input$per))) %>%
        rename_at(1:3, ~c("purchasesCurr", "itemsCurr", "dollarsCurr")),
      pre %>%
        filter(segment == input$seg) %>%
        select(ends_with(paste0(input$grp, input$per))) %>%
        rename_at(1:3, ~c("purchasesPre", "itemsPre", "dollarsPre"))
    ) %>%
      mutate(
        week = 1:52,
        dollarsPct = f(dollarsPre, dollarsCurr),
        usersPre = filter(users, segment == input$seg) %>% .$pre,
        usersCurr = filter(users, segment == input$seg) %>% .$curr,
        usersPct = f(usersPre, usersCurr),
        purUserPre = purchasesPre / usersPre,
        purUserCurr = purchasesCurr / usersCurr,
        purUserPct = f(purUserPre, purUserCurr),
        itemsPurPre = itemsPre / purchasesPre,
        itemsPurCurr = itemsCurr / purchasesCurr,
        itemsPurPct = f(itemsPurPre, itemsPurCurr),
        dollItemsPre = dollarsPre / itemsPre,
        dollItemsCurr = dollarsCurr / itemsCurr,
        dollItemsPct = f(dollItemsPre, dollItemsCurr)
      ) %>%
      filter(week <= 22) %>%
      select(
        week, dollarsPre, dollarsCurr, dollarsPct,
        usersPre, usersCurr, usersPct,
        purUserPre, purUserCurr, purUserPct,
        itemsPurPre, itemsPurCurr, itemsPurPct,
        dollItemsPre, dollItemsCurr, dollItemsPct
      )
  })
  
  pdat <- reactive({
    dat() %>%
      select(week, dollarsPct, usersPct, purUserPct, itemsPurPct, dollItemsPct) %>%
      gather(seg, metric, -week) %>%
      mutate(metric = round(metric, 2))
  })
  
  output$plot <- renderPlotly({
    p1 <- ggplot(data = filter(pdat(), seg != "dollarsPct"), aes(week, metric, fill = seg)) +
      geom_bar(stat = "Identity") + 
      geom_line(data = filter(pdat(), seg == "dollarsPct"), aes(week, metric), col = "darkgrey") +
      scale_fill_manual(values = alpha(c("darkgrey", "lightgreen", "salmon", "lightblue", "orange"), 0.5)) +
      labs(x = "Week", y = "Percent") +
      theme_minimal()
    ggplotly(p1)
  })
  
  output$data <- renderDataTable({
    dat() %>%
      select(week, dollarsPct, usersPct, purUserPct, itemsPurPct, dollItemsPct) %>%
      mutate_at(vars(dollarsPct:dollItemsPct), round, 2)
  })
  
}

shinyApp(ui = ui, server = server)









