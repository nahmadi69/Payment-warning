source("functions.R")

credentials <- data.frame(
  user = c("Finance","Katayoun"),
  password = c("MonPassPartout@1352","Naser@1363"),
  stringsAsFactors = FALSE
)



function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

# cleaning ----------------------------------------------------------------


  Exchange_rates <- reactive({
    # Try to get rates from API
    tryCatch({
      response <- GET("https://api.Exchangerate-api.com/v4/latest/USD", timeout(5))
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))
        f <- data.frame(Currency=row.names(t(data.frame(data$rates))),
                        Rate=t(data.frame(data$rates)))
        g <- f[f$Currency %in% c("IQD","RUB","EUR","INR"),]
        return(g)
      } else {
        # If API call doesn't return 200, use fallback values
        warning("API call failed with status code: ", status_code(response))
        return(get_fallback_rates())
      }
    }, error = function(e) {
      # If API call errors out completely, use fallback values
      warning("API call error: ", e$message)
      return(get_fallback_rates())
    })
  })
  get_fallback_rates <- function() {
    # Default fallback Exchange rates (you can update these to recent values)
    data.frame(
      Currency = c("IQD", "RUB", "EUR", "INR"),
      Rate = c(1310, 90, 0.92, 83.5),
      stringsAsFactors = FALSE
    )
  }
  
  output$multiplier_EUR <- renderUI({
    if (!is.null(input$file_3)) {
      rates <- Exchange_rates()
      eur_rate <- rates$Rate[rates$Currency=="EUR"]
      if(length(eur_rate) == 0) eur_rate <- 0.92  # Fallback if not found
      
      numericInput("multiplier_EUR", "USD to Euro",
                   value = round(eur_rate, 3), min = 0, max = 10)
    } 
  })
  output$multiplier_RUB <- renderUI({
    if (!is.null(input$file_3)) {
      rates <- Exchange_rates()
      rub_rate <- rates$Rate[rates$Currency=="RUB"]
      if(length(rub_rate) == 0) rub_rate <- 90  # Fallback if not found
      
      numericInput("multiplier_RUB", "USD to RUB",
                   value = round(rub_rate, 3), min = 0, max = 100)
    } 
  })
  output$multiplier_IQD <- renderUI({
    if (!is.null(input$file_3)) {
      rates <- Exchange_rates()
      iqd_rate <- rates$Rate[rates$Currency=="IQD"]
      if(length(iqd_rate) == 0) iqd_rate <- 1310  # Fallback if not found
      
      numericInput("multiplier_IQD", "USD to IQD",
                   value = round(iqd_rate, 3), min = 0, max = 2000)
    } 
  })
  output$multiplier_INR <- renderUI({
    if (!is.null(input$file_3)) {
      rates <- Exchange_rates()
      inr_rate <- rates$Rate[rates$Currency=="INR"]
      if(length(inr_rate) == 0) inr_rate <- 83.5  # Fallback if not found
      
      numericInput("multiplier_INR", "USD to INR",
                   value = round(inr_rate, 3), min = 0, max = 100)
    } 
  })
  
  
  data_Paid <-reactive({
    if (is.null(input$file_3)) {
      return(NULL)
    }
    read_xlsx(input$file_3$datapath, sheet = "Paid") 
  })

  
  data_PI <- reactive({
    if (is.null(input$file_3)) {
      return(NULL)
    }
    read_xlsx(input$file_3$datapath, sheet = "PI")%>%
      mutate(Payment_date =  ymd(paste0(Payment_year, "-", Payment_month, "-", Payment_day)),
             Payment_month= month(Payment_date, label = TRUE),
             Grand_total=Total_value + Shinpment_cost,
             Payment_value=Payment_term*Grand_total)
  })
  data_Invoice <- reactive({
    if (is.null(input$file_3)) {
      return(NULL)
    }
    read_xlsx(input$file_3$datapath, sheet = "Invoice") %>%
      mutate(Payment_date = ymd(paste0(Payment_year, "-", Payment_month, "-", Payment_day)),
             Payment_month= month(Payment_date, label = TRUE),
             Grand_total=Total_value + Shinpment_cost,
             Payment_value=Payment_term*Grand_total)
  })
  
  data_1 <- eventReactive(input$file_3,{
    Payment_data <- data_PI() %>% 
      bind_rows(data_Invoice())%>% 
      group_by(Order_number) %>% 
      filter(!(("Invoice" %in% unique(Invoice_type)) & Invoice_type=="PI"))
    
    Paid_data=data_Paid() 
    
    Payment_data=payment_function(Payment = Payment_data,Paid = Paid_data)%>% 
      mutate(Remain_payment=Payment_value-Paid_value,
             Payment_status=if_else(Paid_value>=Payment_value,"Paid","Unpaid"),
             Time_remain=as.numeric(Payment_date-today()),
             Time_remain =case_when(Payment_status == "Paid" ~ NA_real_,TRUE ~ Time_remain ),
             Time_delay=as.numeric(Paid_date-Payment_date),
             Time_delay =case_when(Payment_status == "Unpaid" ~ NA_real_,TRUE ~ Time_delay )) %>% 
      ungroup()
  })
  data_11 <- eventReactive(input$file_3,{
    read_xlsx(input$file_3$datapath, sheet = "Paid")  %>% 
      left_join(
        data_PI() %>% 
          bind_rows(data_Invoice()) %>% group_by(Order_number) %>% 
          filter(!(("Invoice" %in% unique(Invoice_type)) & Invoice_type=="PI")) %>% 
          distinct(Order_number,Invoice,Currency_type,Manufacturer,Country,Consignee)) %>% 
      mutate(Paid_date =  ymd(paste0(Paid_year, "-", Paid_month, "-", Paid_day)))
  })
  
  Year_1 <- reactive({
    result <- tryCatch({
      req(data_1())
      unique(data_1()["Year"])
    }, error = function(e) {
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
      return(NULL)
    })
    
    return(result)
  })
  output$Year_ui_1 <- renderUI({
    req(Year_1())
    selectInput("Year_1", "Please select the Year of Export", choices = c("All Years",Year_1()),multiple = TRUE, selected = "All Years")
  })
  data_1_1 <- reactive({
    req(data_1())
    d_filtered <- data_1()
    if (!is.null(input$Year_1) && !("All Years" %in% input$Year_1)) {
      d_filtered <- d_filtered %>% filter(Year %in% input$Year_1)
    }
    return(data.frame(d_filtered))
  })
  data_11_1 <- reactive({
    req(data_11())
    d_filtered <- data_11()
    if (!is.null(input$Year_1) && !("All Years" %in% input$Year_1)) {
      d_filtered <- d_filtered %>% filter(Year %in% input$Year_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  output$date_range_selector <- renderUI({
    result <- tryCatch({
      min_date_1 <- min(data_1_1()$Payment_date,na.rm=TRUE)
      max_date_1 <- max(data_1_1()$Payment_date,na.rm=TRUE)      
      min_date_2 <- min(data_11_1()$Paid_date,na.rm=TRUE)
      max_date_2 <- max(data_11_1()$Paid_date,na.rm=TRUE)
      min_date <- min(min_date_1,min_date_2,na.rm=TRUE)
      max_date <- max(max_date_1,max_date_2,na.rm=TRUE)
      dateRangeInput(
        "date_range",
        label = "Select date range",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date,
        format = "yyyy-mm-dd",
        separator = " to "
      )
    }, error = function(e) {
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
      return(NULL)  # Return NULL if there's an error
    })
    return(result)
  })
  data_2 <- reactive({
    req(data_1_1(),input$date_range)
    data_1_1() %>% filter(data_1_1()$Payment_date >= input$date_range[1] & data_1_1()$Payment_date <= input$date_range[2])
  })
  data_22 <- reactive({
    req(data_11_1(),input$date_range)
    data_11_1() %>% filter(data_11_1()$Paid_date >= input$date_range[1] & data_11_1()$Paid_date <= input$date_range[2])
  })
  
  Manufacturer_1 <- reactive({
    result <- tryCatch({
      req(data_2())
      unique(data_2()["Manufacturer"])
    }, error = function(e) {
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
      return(NULL)
    })
    
    return(result)
  })
  output$Manufacturer_ui_1 <- renderUI({
    req(Manufacturer_1())
    selectInput("Manufacturer_1", "Please select the manufacturers", choices = c("All manufacturers",Manufacturer_1()),multiple = TRUE, selected = "All manufacturers")
  })
  data_4 <- reactive({
    req(data_2())
    d_filtered <- data_2()
    if (!is.null(input$Manufacturer_1) && !("All manufacturers" %in% input$Manufacturer_1)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_1)
    }
    return(data.frame(d_filtered))
  })
  data_44 <- reactive({
    req(data_22())
    d_filtered <- data_22()
    if (!is.null(input$Manufacturer_1) && !("All manufacturers" %in% input$Manufacturer_1)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_1)
    }
    return(data.frame(d_filtered))
  })
  
  Country_1 <- reactive({
    result <- tryCatch({
      req(data_2())
      unique(data_2()["Country"])
    }, error = function(e) {
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
      return(NULL)  
    })
    
    return(result)
  })
  output$Country_ui_1 <- renderUI({
    req(Country_1())
    selectInput("Country_1", "Please select the Countries", choices = c("All Countries",Country_1()),multiple = TRUE, selected = "All Countries")
  })
  data_5 <- reactive({
    req(data_4())
    d_filtered <- data_4()
    
    if (!("All Countries" %in% input$Country_1)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_1)
    }
    return(data.frame(d_filtered))
  })
  data_55 <- reactive({
    req(data_44())
    d_filtered <- data_44()
    
    if (!("All Countries" %in% input$Country_1)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_1)
    }
    return(data.frame(d_filtered))
  })
  
  Consignee_1 <- reactive({
    result <- tryCatch({
      req(data_2())
      unique(data_2()["Consignee"])
    }, error = function(e) {
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
      return(NULL)  
    })
    
    return(result)
  })
  output$Consignee_ui_1 <- renderUI({
    req(Consignee_1())
    selectInput("Consignee_1", "Please select the Consignees", choices = c("All Consignees",Consignee_1()),multiple = TRUE, selected = "All Consignees")
  })
  data_6 <- eventReactive(input$calcButton_1,{
    req(data_5())
    d_filtered <- data_5()
    
    if (!("All Consignees" %in% input$Consignee_1)) {
      d_filtered <- d_filtered %>% filter(Consignee %in% input$Consignee_1)
    }
    return(data.frame(d_filtered))
  })
  data_66 <- eventReactive(input$calcButton_1,{
    req(data_55())
    d_filtered <- data_55()
    
    if (!("All Consignees" %in% input$Consignee_1)) {
      d_filtered <- d_filtered %>% filter(Consignee %in% input$Consignee_1)
    }
    return(data.frame(d_filtered))
  })

  
# Payment plots ------------------------------------------------------------
  data_plot_currency=reactive({
    req(data_6(),data_66())
    
    d=data_6() %>%
      mutate(Payment_term=round(Payment_term,2)) %>%
      select("Order_number","Invoice"
             ,"Invoice_type","Manufacturer","Country"
             ,"Consignee","Account_detail"
             ,"Payment_term","Total_value"
             ,"Shinpment_cost","Grand_total"
             ,"Payment_value","Paid_value","Remain_payment","Currency_type"
             ,"Payment_date","Time_remain","Time_delay","Payment_status")
    
    d1=data_66() %>%
      group_by(Currency_type)  %>%
      summarise(Paid= sum(Paid_value,na.rm=TRUE)) %>% 
      mutate(Paid=if_else(is.na(Paid),0,Paid))
    d2=d %>%
      group_by(Currency_type) %>%
      filter( Time_remain<0) %>%
      summarise(Overdue= sum(Remain_payment,na.rm=TRUE)) %>% 
      mutate(Overdue=if_else(is.na(Overdue),0,Overdue))
    d3=d %>%
      group_by(Currency_type) %>%
      filter( Time_remain>=0) %>%
      summarise(Future= sum(Remain_payment,na.rm=TRUE))%>% 
      mutate(Future=if_else(is.na(Future),0,Future))
    
    data_plot_currency=d1 %>%
      filter(!is.na(Currency_type))%>% 
      full_join(d2) %>% 
      full_join(d3) %>% 
      mutate(Future=if_else(is.na(Future),0,Future),
             Overdue=if_else(is.na(Overdue),0,Overdue),
             Paid=if_else(is.na(Paid),0,Paid))%>% 
      mutate(Total=Paid+Overdue+Future)%>% 
      gather("Paid_type","Value",-Currency_type)
    
    return(data_plot_currency)
  })
  output$plot_currency <- renderPlotly({
    req(data_plot_currency())
    data_plot_currency <- data_plot_currency()
    
    # Create a formatter that only adds thousand separators without currency symbols
    comma_formatter <- scales::comma_format(big.mark = ",")
    
    # Format the display values with only thousand separators
    data_plot_currency <- data_plot_currency %>%
      mutate(
        formatted_value = comma_formatter(Value)
      )
    
    p <- ggplot(data_plot_currency, 
                aes(x = Currency_type, y = Paid_type, fill = Value,
                    text = paste("Value type:", Paid_type, 
                                 "<br>Currency:", Currency_type,
                                 "<br>Value:", formatted_value))) +
      geom_tile() +
      scale_fill_gradient(low = "#BCE0DA", high = "#FFD3B6") +
      labs(x = "Currency type", y = "Value type", fill = "Value") +
      theme_minimal() +
      theme(legend.position = "none") +  # This line removes the legend
      geom_text(aes(label = formatted_value), 
                color = "black", size = 3.5)
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(b = 70, l = 70, t = 50, r = 50))
  })  
 { # output$plot_currency <- renderPlotly({
  #   req(data_plot_currency())
  #   data_plot_currency <- data_plot_currency() %>% 
  #     mutate(Currency_type = factor(Currency_type))
  #   
  #   # Create separate plots for each currency
  #   currency_list <- split(data_plot_currency, data_plot_currency$Currency_type)
  #   plot_list <- list()
  #   
  #   for (i in seq_along(currency_list)) {
  #     currency_df <- currency_list[[i]]
  #     
  #     p <- plot_ly(currency_df) %>%
  #       add_bars(
  #         x = "Paid",
  #         y = ~Paid,
  #         name = "Paid",
  #         legendgroup = "Paid",
  #         showlegend = i == 1,
  #         marker = list(color = "#EF9C66"),
  #         text = ~comma(round(Paid, 0)),
  #         textposition = "inside"
  #       ) %>%
  #       add_bars(
  #         x = "Overdue",
  #         y = ~Overdue,
  #         name = "Overdue",
  #         legendgroup = "Overdue",
  #         showlegend = i == 1,
  #         marker = list(color = "#FCDC94"),
  #         text = ~comma(round(Overdue, 0)),
  #         textposition = "inside"
  #       ) %>%
  #       add_bars(
  #         x = "Future",
  #         y = ~Future,
  #         name = "Future",
  #         legendgroup = "Future",
  #         showlegend = i == 1,
  #         marker = list(color = "#78ABA8"),
  #         text = ~comma(round(Future, 0)),
  #         textposition = "inside"
  #       ) %>%
  #       layout(
  #         yaxis = list(title = "", showgrid = TRUE),
  #         xaxis = list(title = "", tickangle = -45),
  #         margin = list(b = 100, t = 30),
  #         annotations = list(
  #           list(
  #             x = 0.5,
  #             y = -0.3,
  #             text = currency_df$Currency_type[1],
  #             showarrow = FALSE,
  #             xref = "paper",
  #             yref = "paper",
  #             font = list(size = 12)
  #           )
  #         ),
  #         showlegend = FALSE
  #       )
  #     
  #     plot_list[[i]] <- p
  #   }
  #   
  #   # Combine plots horizontally
  #   p <- subplot(
  #     plot_list,
  #     nrows = 1,
  #     shareY = FALSE,
  #     shareX = FALSE
  #   ) %>%
  #     layout(
  #       legend = list(orientation = "h", x = 0.5, y = 1.1),
  #       margin = list(b = 120)
  #     )
  #   
  #   p
  # })  
}
  
  data_plot_currency11=reactive({
    req(data_66())

    data_plot_currency=data_66()  %>%
      group_by(paid_type,Manufacturer,Currency_type) %>%
      summarise(Paid= sum(Paid_value,na.rm=TRUE))%>% 
      mutate(Manufacturer=factor(Manufacturer))


    return(data_plot_currency)
  })
  output$plot_11 <- renderPlotly({
    req(data_plot_currency11())
    data_plot_currency11 <- data_plot_currency11()
    col <- c("#D2691E","#163020","#B6C4B6","#304D30","#EEF0E5")[1:length(unique(data_plot_currency11$Manufacturer))]
    data_list <- split(data_plot_currency11, data_plot_currency11$Currency_type)
    plot_list <- list()
    for(i in 1:length(data_list)){
      max_paid <- max(data_list[[i]]$Paid) * 1.2
      plot_list[[i]] <- plot_ly(data_list[[i]], 
                                x = ~reorder(paid_type, -Paid), 
                                y = ~Paid, 
                                color = ~Manufacturer, 
                                colors = col, 
                                type = 'bar', 
                                barmode = 'group', 
                                showlegend =  FALSE) %>% 
        layout(xaxis = list(title = "Year"), 
               yaxis = list(title = "", tickformat = "$,.0f", range = c(0, max_paid)), 
               bargap = 0.1, 
               font = list(size = 12), 
               margin = list(l = 50, r = 50, t = 50, b = 50), 
               annotations = list(
                 list(
                   text = names(data_list)[i], 
                   x = 6, 
                   y = 0, 
                   showarrow = TRUE, 
                   font = list(size = 10)
                 ),
                 list(
                   text = names(data_list)[i], 
                   x = 0.5, 
                   y = -0.2, 
                   showarrow = FALSE, 
                   font = list(size = 10)
                 )
               ))
    }
    
    fig <- subplot(plot_list, nrows = length(data_list), margin = 0.05, shareY = TRUE) %>% 
      layout(legend = list(traceorder = "group", itemsizing = "constant"), 
             showlegend = TRUE)
    fig
  })
  
  data_plot_delay=reactive({
    req(data_6())
    
    d=data_6()%>%
      select("Consignee","Time_delay") %>%
      group_by(Consignee)  %>%
      summarise(delay= mean(Time_delay,na.rm=TRUE))
    data_plot_delay=d
    return(data_plot_delay)
  })
  output$plot_delay <- renderPlotly({
    req(data_plot_delay())
    data_plot_delay <- data_plot_delay() %>% mutate(x=as.character(Consignee))
    
    p <- ggplot(data_plot_delay, aes(x = Consignee, y = delay, fill = delay > 0)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c("#76BA99","#EB586F")) +
      theme_minimal() +
      labs(title = "Mean Days of Payment Delay per Consignee",
           x = "Consignee",
           y = "Mean Delay (positive value means having delay)") +
      geom_hline(yintercept = 0, linetype = "dashed")+ # Adds a dashed line at y=0
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p)
  })
  
  data_Manufacturer_1=reactive({
    req(data_66())
    
    d=data_66() 
    
    data_plot_currency <- d %>% 
      filter(!is.na(Currency_type) & !is.na(Manufacturer))%>%
      group_by(Currency_type, Manufacturer) %>%
      summarise(Paid = sum(Paid_value, na.rm = TRUE), .groups = 'drop') %>%
      complete(Currency_type, Manufacturer, fill = list(Paid = 0))
    

    
    
    # data_plot_currency=d1 %>% left_join(d2) %>% left_join(d3)
    return(data_plot_currency)
  })
  output$plot_Manufacturer_1 <- renderPlotly({
    req(data_Manufacturer_1())
    data_Manufacturer_1 <- data_Manufacturer_1()
    
    # Create a formatter that only adds thousand separators without currency symbols
    comma_formatter <- scales::comma_format(big.mark = ",")
    
    # Format the display values with only thousand separators
    data_Manufacturer_1 <- data_Manufacturer_1 %>%
      mutate(
        formatted_value = comma_formatter(Paid)
      )
    
    p <- ggplot(data_Manufacturer_1, 
                aes(x =Currency_type , y = Manufacturer , fill = Paid,
                    text = paste("Manufacturer:", Manufacturer, 
                                 "<br>Currency:", Currency_type,
                                 "<br>Value:", formatted_value))) +
      geom_tile() +
      scale_fill_gradient(low = "#BCE0DA", high = "#FFD3B6") +
      labs(x = "Currency type", y = "Manufacturer", fill = "Paid Value") +
      theme_minimal() +
      theme(legend.position = "none") +  # This line removes the legend
      geom_text(aes(label = formatted_value), 
                color = "black", size = 3.5)
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(b = 70, l = 70, t = 50, r = 50))
  })  
  
  data_Manufacturer_2=reactive({
    req(data_66())
    
    d=data_66()
    
    d <- d %>% 
      filter(!is.na(Order_number)) %>% 
      mutate(Paid_value=Paid_value/Exchange_paid_day)%>% 
      group_by(Manufacturer) %>% 
      summarise(Paid = sum(Paid_value, na.rm = TRUE))%>% 
      # left_join(data.frame(Currency_type=c("USD","Euro","IQD","RUB","INR"),
      #                      Exchange_rate=c(1,input$multiplier_EUR,input$multiplier_IQD
      #                                      ,input$multiplier_RUB,input$multiplier_INR))) %>% 
      # ungroup() %>% 
      # group_by(Manufacturer)%>% 
      # summarise(Paid=sum(Paid_value,na.rm=TRUE))
    
    
    # data_plot_currency=d1 %>% left_join(d2) %>% left_join(d3)
    return(d)
  })
  output$plot_Manufacturer_2 <- renderPlotly({
    req(data_Manufacturer_2())
    df <- data_Manufacturer_2()
    
    # Ensure Paid is numeric
    df$Paid <- round(as.numeric(df$Paid),0)
    
    # Calculate percentage
    df$percent <- round(df$Paid / sum(df$Paid) * 100, 1)
    comma_formatter <- scales::comma_format(big.mark = ",")
    
    # Create custom text: "Paid value (percent%)"
    df$custom_text <- paste0(comma_formatter(df$Paid), " (", df$percent, "%)")
    
    plot_ly(df, labels = ~Manufacturer, values = ~Paid, type = 'pie',
            text = ~custom_text, textinfo = "text") %>%
      layout(
        # title = "Manufacturer Distribution",
        showlegend = TRUE,
        piecolorway = RColorBrewer::brewer.pal(9, "Set1"),
        hoverinfo = "label+text"
      )
  })
  
  data_plot_year=reactive({
    req(data_66(),data_6())
    
    d=data_66() %>% left_join(data_6() %>%
                                select(Order_number, Year) %>%
                                distinct(Order_number, .keep_all = TRUE))
    
    d <- d %>% 
      filter(!is.na(Order_number)) %>% 
      mutate(Paid_value=Paid_value/Exchange_paid_day)%>% 
      group_by(Year) %>% 
      summarise(Paid = sum(Paid_value, na.rm = TRUE))%>% 
      # left_join(data.frame(Currency_type=c("USD","Euro","IQD","RUB","INR"),
      #                      Exchange_rate=c(1,input$multiplier_EUR,input$multiplier_IQD
      #                                      ,input$multiplier_RUB,input$multiplier_INR))) %>% 
      # ungroup() %>% 
      # group_by(Year)%>% 
      # summarise(Paid=sum(Paid_value,na.rm=TRUE))
    
      return(d)
  })
  output$plot_Year <- renderPlotly({
    req(data_plot_year())
    df <- data_plot_year()
    
    # Ensure Paid is numeric
    df$Paid <- round(as.numeric(df$Paid),0)
    
    # Calculate percentage
    df$percent <- round(df$Paid / sum(df$Paid) * 100, 1)
    comma_formatter <- scales::comma_format(big.mark = ",")
    
    # Create custom text: "Paid value (percent%)"
    df$custom_text <- paste0(comma_formatter(df$Paid), " (", df$percent, "%)")
    
    plot_ly(df, labels = ~Year, values = ~Paid, type = 'pie',
            text = ~custom_text, textinfo = "text") %>%
      layout(
        # title = "Manufacturer Distribution",
        showlegend = TRUE,
        piecolorway = RColorBrewer::brewer.pal(9, "Set1"),
        hoverinfo = "label+text"
      )
  })
  
  data_Paid_type=reactive({
    req(data_6(),data_66())
    
    d=data_6() %>%
      mutate(Payment_term=round(Payment_term,2)) %>%
      select("Order_number","Invoice"
             ,"Invoice_type","Manufacturer","Country"
             ,"Consignee","Account_detail"
             ,"Payment_term","Total_value"
             ,"Shinpment_cost","Grand_total"
             ,"Payment_value","Paid_value","Remain_payment","Currency_type"
             ,"Payment_date","Time_remain","Time_delay","Payment_status","Exchange")
    
    d1=data_66() %>%
      mutate(Paid_value=Paid_value/Exchange_paid_day)%>% 
      # group_by(Currency_type)  %>%
      summarise(Paid= sum(Paid_value,na.rm=TRUE)) %>% 
      mutate(Currency_type=1)
    d2=d %>%
      mutate(Remain_payment=Remain_payment/Exchange)%>% 
      # group_by(Currency_type) %>%
      filter( Time_remain<0) %>%
      summarise(Overdue= sum(Remain_payment,na.rm=TRUE)) %>% 
      mutate(Currency_type=1)
    d3=d %>%
      mutate(Remain_payment=Remain_payment/Exchange)%>% 
      # group_by(Currency_type) %>%
      filter( Time_remain>=0) %>%
      summarise(Future= sum(Remain_payment,na.rm=TRUE))%>% 
      mutate(Currency_type=1)
    
    d=d1 %>% 
      full_join(d2) %>% 
      full_join(d3) %>% 
      mutate(Future=if_else(is.na(Future),0,Future),
             Overdue=if_else(is.na(Overdue),0,Overdue),
             Paid=if_else(is.na(Paid),0,Paid))%>% 
      gather("Paid_type","Value",-Currency_type)
    # 
    # d=data.frame(data_plot_currency()) %>% filter(Paid_type!="Total") %>% 
    #   # left_join(data.frame(Currency_type=c("USD","Euro","IQD","RUB","INR"),
    #   #                      Exchange_rate=c(1,input$multiplier_EUR,input$multiplier_IQD
    #   #                                      ,input$multiplier_RUB,input$multiplier_INR))) %>% 
    #   mutate(Value=Value/Exchange)%>% 
    #   group_by(Paid_type)%>% 
    #   summarise(Value=sum(Value,na.rm=TRUE))
    
    
    return(d)
  })
  output$plot_Paid_type <- renderPlotly({ 
    req(data_Paid_type())
    df <- data_Paid_type()
    # Ensure Paid is numeric
    df$Value <- round(as.numeric(df$Value),0)
    
    # Calculate percentage
    df$percent <- round(df$Value / sum(df$Value) * 100, 1)
    comma_formatter <- scales::comma_format(big.mark = ",")
    
    # Create custom text: "Paid value (percent%)"
    df$custom_text <- paste0(comma_formatter(df$Value), " (", df$percent, "%)")
    
    plot_ly(df, labels = ~Paid_type, values = ~Value, type = 'pie',
            text = ~custom_text, textinfo = "text") %>%
      layout(
        # title = "Paid_type Distribution",
        showlegend = TRUE,
        piecolorway = RColorBrewer::brewer.pal(9, "Set1"),
        hoverinfo = "label+text"
      )
  })  
  
# Payment tables ------------------------------------------------------------
  
  data_table_payment=reactive({
    req(data_6())
    
    data_6()    %>% 
      # left_join(data.frame(Currency_type=c("USD","Euro","IQD","RUB","INR"),
      #                      Exchange_rate=c(1,input$multiplier_EUR,input$multiplier_IQD
      #                                      ,input$multiplier_RUB,input$multiplier_INR)))%>% 
      mutate(
        Payment_value_USD=round(Payment_value/Exchange,0),
        Paid_value_USD=round(Paid_value/Exchange,0),
        Remain_payment_USD=round(Remain_payment/Exchange,0),
        across("Grand_total", ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = "")),
        across("Payment_value", ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = "")),
        across("Remain_payment", ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = "")),
        across("Paid_value", ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = ""))
      ) %>%
      mutate(Payment_term=round(Payment_term,2),Payment_status=if_else(is.na(Payment_status),"Unpaid",Payment_status)) %>% 
      select("Order_number","Invoice","Year"       
             ,"Invoice_type","Manufacturer","Country"       
             ,"Consignee","Account_detail"
             ,"Currency_type","Grand_total"
             ,"Payment_date","Time_remain","Time_delay","Payment_status",
             "Exchange","Payment_value","Payment_value_USD","Paid_value","Paid_value_USD",
             "Remain_payment","Remain_payment_USD","Paid_date") %>% 
      arrange(Order_number)
  })
  
  output$data_1 <- renderReactable({
    req(data_table_payment())
    
    reactable(
      data_table_payment(),
      pagination = TRUE,
      defaultPageSize = 15,
      fullWidth=TRUE,
      # theme = reactable::reactableTheme("bootstrap"),
      style = list(
        fontSize = 12,
        fontFamily = "Arial",
        border = "1px solid #ddd"
      )
      ,
      # Add theme for the top border
      theme = reactableTheme(
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "blue"
        )
      ),
      columns = list(
        Time_remain = colDef(
          name = htmltools::HTML("Time Remain"), 
          minWidth = 100,  
          style = function(value) {
            if (is.na(value)) {
              "background-color: #FFFFFF;"  
            } else if (value < 0) {
              "background-color: #D04848;"  
            } else if (value <= 7) {
              "background-color: orange;"  
            } else if (value <= 30) {
              "background-color: yellow;"  
            } else {
              "background-color: white;"  
            }
          },
          class = "border-right"
        ),
        Payment_value_USD = colDef(
          format = colFormat(currency = "USD", separators = TRUE, digits = 0)
        ),
        Paid_value_USD = colDef(
          format = colFormat(currency = "USD", separators = TRUE, digits = 0)
        ),
        Remain_payment_USD = colDef(
          format = colFormat(currency = "USD", separators = TRUE, digits = 0)
        )
      )
    )
  })
  
  output$downloadTable1_0 <- downloadHandler(
    filename = function() {
      paste("Invoice & PI_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_table_payment() 
                , file, row.names = FALSE)
    }
  )  
  
  output$downloadTable1_1 <- downloadHandler(
    filename = function() {
      paste("Overdue_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_table_payment() %>% filter(Time_remain<=0)
                , file, row.names = FALSE)
    }
  )
  
  output$downloadTable1_2 <- downloadHandler(
    filename = function() {
      paste("Next week payment_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_table_payment() %>% filter(Time_remain>0 & Time_remain<=7)
                , file, row.names = FALSE)
    }
  )
  
  output$data_2 <- renderReactable({
    req(data_66())
    
    reactable(
      data_66() %>%
        # left_join(data.frame(
        #   Currency_type = c("USD", "Euro", "IQD", "RUB", "INR"),
        #   Exchange_rate = c(1, input$multiplier_EUR, input$multiplier_IQD,
        #                     input$multiplier_RUB, input$multiplier_INR)
        # )) %>%
        mutate(
          Paid_value_USD = round(Paid_value / Exchange_paid_day, 0),
          Paid_value = formatC(Paid_value, format = "f", digits = 0, big.mark = ",")
        ) %>%
        select(Order_number, Paid_ID,Year, Invoice, Currency_type, Manufacturer, Country,
               Consignee, paid_type, Paid_date, Paid_value,Exchange_paid_day, Paid_value_USD)
      ,
      columns = list(
        Paid_value_USD = colDef(
          format = colFormat(currency = "USD", separators = TRUE, digits = 0)
        )
      ),
      pagination = TRUE,
      defaultPageSize = 15,
      fullWidth = TRUE,
      style = list(
        fontSize = 12,
        fontFamily = "Arial",
        border = "1px solid #ddd"
      ),
      theme = reactableTheme(
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "blue"
        )
      )
    )
  })
  
  output$downloadTable2_0 <- downloadHandler(
    filename = function() {
      paste("Paid data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_66() %>%
                  # left_join(data.frame(
                  #   Currency_type = c("USD", "Euro", "IQD", "RUB", "INR"),
                  #   Exchange_rate = c(1, input$multiplier_EUR, input$multiplier_IQD,
                  #                     input$multiplier_RUB, input$multiplier_INR)
                  # )) %>%
                  mutate(
                    Paid_value_USD = round(Paid_value / Exchange_paid_day, 0),
                    Paid_value = formatC(Paid_value, format = "f", digits = 0, big.mark = ",")
                  ) %>%
                  select(Order_number, Paid_ID,Year, Invoice, Currency_type, Manufacturer, Country,
                         Consignee, paid_type, Paid_date, Paid_value,Exchange_paid_day, Paid_value_USD) 
                , file, row.names = FALSE)
    }
  )
  

  
  # output$data_4 <- renderReactable({
  #   req(data_Paid_type())
  # 
  #   reactable(
  #     data_Paid_type(),
  #     pagination = TRUE,
  #     defaultPageSize = 15,
  #     fullWidth=TRUE,
  #     # theme = reactable::reactableTheme("bootstrap"),
  #     style = list(
  #       fontSize = 12,
  #       fontFamily = "Arial",
  #       border = "1px solid #ddd"
  #     )
  #     ,
  #     # Add theme for the top border
  #     theme = reactableTheme(
  #       headerStyle = list(
  #         "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
  #         "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
  #         borderColor = "blue"
  #       )
  #     )
  #   )
  # })

# Text boxes --------------------------------------------------------------
  
{  # output$sum_total <- renderUI({
  #   req(data_66())
  #   
  #   # Process data
  #   d <- data_66() %>% 
  #     filter(!is.na(Order_number)) %>% 
  #     group_by(Currency_type) %>% 
  #     summarise(Paid_value = sum(Paid_value, na.rm = TRUE))
  #   
  #   # Function to safely get currency value
  #   get_currency <- function(currency) {
  #     value <- d$Paid_value[d$Currency_type == currency]
  #     ifelse(length(value) > 0 && !is.na(value), 
  #            format(round(value, 1), big.mark = ","), 
  #            "0.0")
  #   }
  #   
  #   HTML(
  #     paste0(
  #       '<div class="currency-summary">
  #       <div class="currency-item">
  #         <span>Dollar:</span><strong>', get_currency("USD"), '</strong>
  #       </div>
  #       <div class="currency-item">
  #         <span>Euro:</span><strong>', get_currency("Euro"), '</strong>
  #       </div>
  #       <div class="currency-item">
  #         <span>Ruble:</span><strong>', get_currency("RUB"), '</strong>
  #       </div>
  #       <div class="currency-item">
  #         <span>Dinar:</span><strong>', get_currency("IQD"), '</strong>
  #       </div>
  #     </div>'
  #     )
  #   )
  # })
  # 
  # output$sum_USD <- renderUI({
  #   req(data_66())
  #   
  #   # Process data
  #   d <- data_66() %>% 
  #     filter(!is.na(Order_number)) %>% 
  #     group_by(Currency_type) %>% 
  #     summarise(Paid_value = sum(Paid_value, na.rm = TRUE))%>% 
  #     left_join(data.frame(Currency_type=c("USD","Euro","IQD","RUB","INR"),
  #                          Exchange_rate=c(1,input$multiplier_EUR,input$multiplier_IQD
  #                                          ,input$multiplier_RUB,input$multiplier_INR))) %>% 
  #     mutate(Paid_value=Paid_value/Exchange_rate)%>% 
  #     ungroup() %>% 
  #     summarise(Paid_value=sum(Paid_value,na.rm=TRUE))
  #   
  #   # Function to safely get currency value
  #   get_currency <- function(data) {
  # 
  #     d=as.numeric(data[1,1])
  #     ifelse(length(d) > 0 && !is.na(d), 
  #            format(round(d, 1), big.mark = ","), 
  #            "0.0")
  #   }
  #   HTML(
  #     paste0(
  #       '<div class="currency-summary">
  #       <div class="currency-item">
  #         <span>Dollar:</span><strong>', get_currency(d), '</strong>
  #       </div>
  #       
  #     </div>'
  #     )
  #   )
  # })
}
  output$combined_currency_summary <- renderUI({
    req(data_66())
    
    # Original data processing for currencies
    currency_data <- data_66() %>% 
      filter(!is.na(Order_number)) %>% 
      group_by(Currency_type) %>% 
      summarise(Paid_value = sum(Paid_value, na.rm = TRUE))
    
    # Original data processing for USD total
    total_usd <- data_66() %>% 
      filter(!is.na(Order_number)) %>% 
      mutate(Paid_value = Paid_value / Exchange_paid_day) %>% 
      # group_by(Currency_type) %>% 
      summarise(Paid_value = sum(Paid_value, na.rm = TRUE)) %>% 
      # left_join(data.frame(
      #   Currency_type = c("USD", "Euro", "IQD", "RUB", "INR"),
      #   Exchange_rate = c(1, input$multiplier_EUR, input$multiplier_IQD,
      #                     input$multiplier_RUB, input$multiplier_INR)
      # )) %>% 
      # ungroup() %>% 
      # summarise(Paid_value = sum(Paid_value, na.rm = TRUE)) %>% 
      pull(Paid_value)
    
    total_usd_1 <- data_66() %>% 
      filter(!is.na(Order_number)) %>% 
      mutate(Paid_value = Paid_value / Exchange) %>% 
      summarise(Paid_value = sum(Paid_value, na.rm = TRUE)) %>% 
      pull(Paid_value)
    
    # Formatting functions
    format_value <- function(value) {
      format(round(value, 1), big.mark = ",", scientific = FALSE)
    }
    
    get_currency <- function(currency) {
      value <- currency_data$Paid_value[currency_data$Currency_type == currency]
      ifelse(length(value) > 0 && !is.na(value), format_value(value), "0.0")
    }
    
      HTML(
        paste0(
          '<div class="combined-currency-card">
        <div class="currency-breakdown">
          <div class="currency-item">
            <i class="fas fa-dollar-sign currency-icon usd"></i>
            <div class="currency-details">
              <span>Dollar</span>
              <strong>', get_currency("USD"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-euro-sign currency-icon euro"></i>
            <div class="currency-details">
              <span>Euro</span>
              <strong>', get_currency("Euro"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-ruble-sign currency-icon rub"></i>
            <div class="currency-details">
              <span>Ruble</span>
              <strong>', get_currency("RUB"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-donate currency-icon dinar"></i>
            <div class="currency-details">
              <span>Dinar</span>
              <strong>', get_currency("IQD"), '</strong>
            </div>
          </div>
          <div class="currency-item">  <!-- NEW INR BLOCK -->
            <i class="fas fa-rupee-sign currency-icon inr"></i>
            <div class="currency-details">
              <span>Rupee</span>
              <strong>', get_currency("INR"), '</strong>
            </div>
          </div>
        </div>
        
        <div class="total-summary">
          <div class="total-content">
            <i class="fas fa-globe-americas total-icon"></i>
            <div class="total-details">
              <span>Total Value</span>
              <h2>$', format_value(total_usd), '</h2>
              <small>Equivalent in USD (based on paid date exchange rate)</small>
            </div>
            <div class="total-details">
              <h2>$', format_value(total_usd_1), '</h2>
              <small>Equivalent in USD (based on invoice date exchange rate)</small>
            </div>
          </div>
        </div>
      </div>'
        )
      )
    
  })
  
  output$combined_currency_summary_1 <- renderUI({
    req(data_6())
    
    # Original data processing for currencies
    currency_data <- data_6() %>% 
      filter(!is.na(Order_number) ) %>% 
      group_by(Currency_type) %>% 
      summarise(Paid_value = sum(Payment_value, na.rm = TRUE))
    
    # Original data processing for USD total
    total_usd <- data_6() %>% 
      filter(!is.na(Order_number)) %>% 
      mutate(Payment_value = Payment_value / Exchange) %>% 
      # group_by(Currency_type) %>% 
      summarise(Paid_value = sum(Payment_value, na.rm = TRUE)) %>% 
      # left_join(data.frame(
      #   Currency_type = c("USD", "Euro", "IQD", "RUB", "INR"),
      #   Exchange_rate = c(1, input$multiplier_EUR, input$multiplier_IQD,
      #                     input$multiplier_RUB, input$multiplier_INR)
      # )) %>% 
      # ungroup() %>% 
      # summarise(Paid_value = sum(Paid_value, na.rm = TRUE)) %>% 
      pull(Paid_value)
    
    # Formatting functions
    format_value <- function(value) {
      format(round(value, 1), big.mark = ",", scientific = FALSE)
    }
    
    get_currency <- function(currency) {
      value <- currency_data$Paid_value[currency_data$Currency_type == currency]
      ifelse(length(value) > 0 && !is.na(value), format_value(value), "0.0")
    }
    
    HTML(
      paste0(
        '<div class="combined-currency-card">
        <div class="currency-breakdown">
          <div class="currency-item">
            <i class="fas fa-dollar-sign currency-icon usd"></i>
            <div class="currency-details">
              <span>Dollar</span>
              <strong>', get_currency("USD"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-euro-sign currency-icon euro"></i>
            <div class="currency-details">
              <span>Euro</span>
              <strong>', get_currency("Euro"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-ruble-sign currency-icon rub"></i>
            <div class="currency-details">
              <span>Ruble</span>
              <strong>', get_currency("RUB"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-donate currency-icon dinar"></i>
            <div class="currency-details">
              <span>Dinar</span>
              <strong>', get_currency("IQD"), '</strong>
            </div>
          </div>
          <div class="currency-item">  <!-- NEW INR BLOCK -->
            <i class="fas fa-rupee-sign currency-icon inr"></i>
            <div class="currency-details">
              <span>Rupee</span>
              <strong>', get_currency("INR"), '</strong>
            </div>
          </div>
        </div>
        
        <div class="total-summary">
          <div class="total-content">
            <i class="fas fa-globe-americas total-icon"></i>
            <div class="total-details">
              <span>Total Value</span>
              <h2>$', format_value(total_usd), '</h2>
              <small>Equivalent in USD</small>
            </div>
          </div>
        </div>
      </div>'
      )
    )
    
  })
  
  output$combined_currency_summary_2 <- renderUI({
    req(data_6())
    
    # Original data processing for currencies
    currency_data <- data_6() %>% 
      filter(!is.na(Order_number) &  Time_remain<0) %>% 
      group_by(Currency_type) %>% 
      summarise(Paid_value = sum(Remain_payment, na.rm = TRUE))
    
    # Original data processing for USD total
    total_usd <- data_6() %>% 
      filter(!is.na(Order_number) &  Time_remain<0) %>% 
      mutate(Remain_payment = Remain_payment / Exchange) %>% 
      # group_by(Currency_type) %>% 
      summarise(Paid_value = sum(Remain_payment, na.rm = TRUE)) %>% 
      # left_join(data.frame(
      #   Currency_type = c("USD", "Euro", "IQD", "RUB", "INR"),
      #   Exchange_rate = c(1, input$multiplier_EUR, input$multiplier_IQD,
      #                     input$multiplier_RUB, input$multiplier_INR)
      # )) %>% 
      # ungroup() %>% 
      # summarise(Paid_value = sum(Paid_value, na.rm = TRUE)) %>% 
      pull(Paid_value)
    
    # Formatting functions
    format_value <- function(value) {
      format(round(value, 1), big.mark = ",", scientific = FALSE)
    }
    
    get_currency <- function(currency) {
      value <- currency_data$Paid_value[currency_data$Currency_type == currency]
      ifelse(length(value) > 0 && !is.na(value), format_value(value), "0.0")
    }
    
    HTML(
      paste0(
        '<div class="combined-currency-card">
        <div class="currency-breakdown">
          <div class="currency-item">
            <i class="fas fa-dollar-sign currency-icon usd"></i>
            <div class="currency-details">
              <span>Dollar</span>
              <strong>', get_currency("USD"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-euro-sign currency-icon euro"></i>
            <div class="currency-details">
              <span>Euro</span>
              <strong>', get_currency("Euro"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-ruble-sign currency-icon rub"></i>
            <div class="currency-details">
              <span>Ruble</span>
              <strong>', get_currency("RUB"), '</strong>
            </div>
          </div>
          <div class="currency-item">
            <i class="fas fa-donate currency-icon dinar"></i>
            <div class="currency-details">
              <span>Dinar</span>
              <strong>', get_currency("IQD"), '</strong>
            </div>
          </div>
          <div class="currency-item">  <!-- NEW INR BLOCK -->
            <i class="fas fa-rupee-sign currency-icon inr"></i>
            <div class="currency-details">
              <span>Rupee</span>
              <strong>', get_currency("INR"), '</strong>
            </div>
          </div>
        </div>
        
        <div class="total-summary">
          <div class="total-content">
            <i class="fas fa-globe-americas total-icon"></i>
            <div class="total-details">
              <span>Total Value</span>
              <h2>$', format_value(total_usd), '</h2>
              <small>Equivalent in USD</small>
            </div>
          </div>
        </div>
      </div>'
      )
    )
    
  })
  
  
}

