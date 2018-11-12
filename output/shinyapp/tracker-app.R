library(shiny)
library(shinymaterial)
library(readr)
library(tidyverse)
library(formattable)
library(openxlsx)

curr <- read_csv("data/trackerCalcCurr.csv")
pre <- read_csv("data/trackerCalcPre.csv")
users <- read_csv("data/users.csv")

f <- function(x, y) {y / x - 1}

sign_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))

bar_formatter <- formatter("span",
                           style = x ~ style(
                             display = "inline-block", 
                             direction = ifelse(x > 0, "rtl", "ltr"), 
                             `border-radius` = "4px", 
                             `padding-right` = "2px",
                             `font-weight` = "bold",
                             `background-color` = csscolor(ifelse(x > 0, "palegreen", "pink")), 
                             width = percent(proportion(as.numeric(x)))))

tile_formatter <- formatter("span",
                            style = x ~ style(
                              display = "block", 
                              padding = "0 4px", 
                              `border-radius` = "4px",
                              `color` = "grey50",
                              `font-weight` = "bold",
                              `background-color` = ifelse(x > 0,
                                                          csscolor(gradient(as.numeric(x), "white", "palegreen")),
                                                          csscolor(gradient(as.numeric(x), "pink", "white")))))



ui <- material_page(
  title = "Customer Tracker App",
  nav_bar_color = "blue-grey lighten-1",
  tags$br(),
  material_row(
    material_column(
      width = 2,
      material_card(
        material_dropdown('seg', 'Segment', c("Total", "Heavy", "Mainstream", "Focus1", "Focus2", "Specialty", "Diverse1", "Diverse2", "Other", "New"), color = "#78909c"),
        material_dropdown('grp', 'Group', c("Total", "Core", "Extra"), color = "#78909c"),
        material_radio_button("per", "Period", c("Week", "YTD"), color = "#78909c")
      )
    ),
    material_column(
      width = 10,
      material_row(
        material_card(
          title = "Performance Year over Year",
          plotOutput("plot")
        )
      ),
      material_row(
        material_card(
          title = "Weekly Data",
          formattableOutput("dat")
        )
      ),
      downloadButton('downloadData', 'Download')
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
        Week = 1:52,
        RevenuePre = dollarsPre,
        RevenueCurr = dollarsCurr,
        Revenue = f(dollarsPre, dollarsCurr),
        CustomersPre = filter(users, segment == input$seg) %>% .$pre,
        CustomersCurr = filter(users, segment == input$seg) %>% .$curr,
        Customers = f(CustomersPre, CustomersCurr),
        VisitsPre = purchasesPre / CustomersPre,
        VisitsCurr = purchasesCurr / CustomersCurr,
        Visits = f(VisitsPre, VisitsCurr),
        ItemsPre = itemsPre / purchasesPre,
        ItemsCurr = itemsCurr / purchasesCurr,
        Items = f(ItemsPre, ItemsCurr),
        SpendPre = dollarsPre / itemsPre,
        SpendCurr = dollarsCurr / itemsCurr,
        Spend = f(SpendPre, SpendCurr)
      ) %>%
      filter(Week <= 22) %>%
      #arrange(desc(Week)) %>%
      select(
        Week, RevenuePre, RevenueCurr, Revenue,
        CustomersPre, CustomersCurr, Customers,
        VisitsPre, VisitsCurr, Visits,
        ItemsPre, ItemsCurr, Items,
        SpendPre, SpendCurr, Spend
      )
    
  })
  
  pdat <- reactive({
    
    dat() %>%
      select(Week, Revenue, Customers, Visits, Items, Spend) %>%
      gather(seg, metric, -Week) %>%
      mutate(metric = round(100 * metric, 2)) %>%
      mutate(seg = factor(seg, levels = c("Spend", "Items", "Visits", "Customers", "Revenue")))
    
  })

  t0 <- reactive({
    
    dat() %>%
      select(Week, RevenuePre, RevenueCurr, Revenue, Customers, Visits, Items, Spend)
    
  })
  
  t1 <- reactive({
    
    t0() %>%
      mutate_at(vars(Revenue:Spend), ~ percent(.x, digits = 1)) %>%
      mutate_at(vars(RevenuePre, RevenueCurr), ~ currency(.x, digits = 0))
    
  })
  
  p1 <- reactive({

    ggplot(filter(pdat(), seg != "Revenue"), aes(Week, metric, fill = seg)) +
      geom_bar(stat = "Identity") + 
      geom_line(data = filter(pdat(), seg == "Revenue"), aes(Week, metric), col = "darkgrey") +
      scale_fill_manual(values = alpha(c("orange", "salmon", "darkgrey", "lightgreen", "lightblue"), 0.5)) +
      labs(x = "Week", y = "Percent", title = "Percentage change by Week") +
      theme_minimal() +
      theme(legend.title=element_blank())
    
  })
  
  output$plot <- renderPlot({
    
    p1()
    
  })
  
  output$dat <- renderFormattable({
    
    formattable(t1(), list(
      Revenue = tile_formatter,
      Customers = sign_formatter,
      Visits = sign_formatter,
      Items = sign_formatter,
      Spend = sign_formatter
    ))
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
     paste("Tracker", input$seg, input$grp, input$per, sep = "-") %>%
        paste0(., ".xlsx")
    },
    content = function(file) {
      pngfile <- paste0(file, ".png")
      wb <- createWorkbook()
      addWorksheet(wb, "Summary", gridLines = FALSE)
      ggsave(pngfile, p1(), "png", width = 6, height = 3)
      insertImage(wb, "Summary", file = pngfile)
      writeData(wb, "Summary", t0(), startRow = 16)
      addWorksheet(wb, sheetName = "Data")
      writeDataTable(wb, sheet = 2, dat())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

}

shinyApp(ui = ui, server = server)
