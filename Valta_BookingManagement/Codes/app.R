# Property Management Dashboard - R Shiny (Enhanced Version)
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(openxlsx)
# Sample data generation (replace with your actual data loading)
source('DataProcess.R')

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Property Management Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Revenue Analysis", tabName = "revenue", icon = icon("dollar-sign")),
      menuItem("Revenue ADR Occ", tabName = "adrocc", icon = icon("dollar-sign")),
      menuItem("Occupancy", tabName = "occupancy", icon = icon("calendar-alt")),
      menuItem("Booking Platforms", tabName = "platforms", icon = icon("laptop")),
      menuItem("Detailed Reports", tabName = "reports", icon = icon("chart-line")),
      menuItem("Guest Management", tabName = "guests", icon = icon("users")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    ),
    
    # Filters
    dateRangeInput("dateRange", 
                   "Date Range:",
                   start = as.Date("2025-01-01"),#Sys.Date() - 90,
                   end = as.Date("2025-12-31")),#Sys.Date() + 90),
    
    selectizeInput("propertyFilter", 
                   "Properties:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "All Properties")),
    
    selectizeInput("platformFilter", 
                   "Booking Platforms:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "All Platforms"))
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalReservations"),
                valueBoxOutput("totalRevenue"),
                valueBoxOutput("totalPayout")
              ),
              fluidRow(
                valueBoxOutput("avgNightly"),
                valueBoxOutput("avgStay"),
                valueBoxOutput("occupancyRate")
              ),
              fluidRow(
                box(
                  title = "Monthly Revenue vs Payout", status = "primary", solidHeader = TRUE,
                  width = 8, height = 460,
                  plotlyOutput("monthlyRevenuePayout")
                ),
                box(
                  title = "Booking Platform Distribution", status = "info", solidHeader = TRUE,
                  width = 4, height = 460,
                  plotlyOutput("platformDistribution")
                )
              ),
              fluidRow(
                box(
                  title = "Top Properties by Revenue", status = "success", solidHeader = TRUE,
                  width = 12, height = 460,
                  plotlyOutput("topProperties")
                )
              )
      ),
      
      # Revenue Analysis Tab
      tabItem(tabName = "revenue",
              fluidRow(
                box(
                  title = "Revenue Breakdown", status = "success", solidHeader = TRUE,
                  width = 6, height = 450,
                  plotlyOutput("revenueBreakdown")
                ),
                box(
                  title = "Cleaning Fee Analysis", status = "warning", solidHeader = TRUE,
                  width = 6, height = 450,
                  plotlyOutput("cleaningFees")
                )
              ),
              fluidRow(
                box(
                  title = "Revenue Trend Analysis", status = "primary", solidHeader = TRUE,
                  width = 8, height = 450,
                  plotlyOutput("revenueTrend")
                ),
                box(
                  title = "Revenue Yearly Trend", status = "primary", solidHeader = TRUE,
                  width = 4, height = 450,
                  plotlyOutput("revenueYear")
                )
              ),
              fluidRow(
                box(
                  title = "Revenue by Property & Platform", status = "info", solidHeader = TRUE,
                  width = 12, height = 500,
                  plotlyOutput("revenueHeatmap")
                )
              )
      ),
      
      # Revenue ADR Tab
      tabItem(tabName = "adrocc",
              fluidRow(
                box(
                  title = "Yearly Summary", status = "success", solidHeader = TRUE,
                  width = 8, height = 450,
                  plotlyOutput("yearlyrevenue")
                ),
                box(
                  title = "Yearly Trend", status = "warning", solidHeader = TRUE,
                  width = 4, height = 450,
                  plotlyOutput("yearlyrevenueplot")
                )
              ),
             fluidRow(
                box(
                  title = "Monthly Property Summary", status = "info", solidHeader = TRUE,
                  width = 12, height = 500,
                  plotlyOutput("monthlyproperty")
                )
              )
      ),
      
      # Booking Platforms Tab
      tabItem(tabName = "platforms",
              fluidRow(
                box(
                  title = "Platform Performance", status = "primary", solidHeader = TRUE,
                  width = 6, height = 500,
                  plotlyOutput("platformPerformance")
                ),
                box(
                  title = "Platform Comparison Table", status = "warning", solidHeader = TRUE,
                  width = 6, height = 500,
                  DT::dataTableOutput("platformComparison")
                )
              ),
              fluidRow(
                box(
                  title = "Platform Trend", status = "info", solidHeader = TRUE,
                  width = 8, height = 500,
                  plotlyOutput("platformTrend")
                ),
                box(
                  title = "Platform Yearly", status = "primary", solidHeader = TRUE,
                  width = 4, height = 500,
                  plotlyOutput("platformYear")
                )
              )
      ),
      
      # Occupancy Tab
      tabItem(tabName = "occupancy",
              fluidRow(
                box(
                  title = "Occupancy Rate by Property", status = "primary", solidHeader = TRUE,
                  width = 8, height = 630,
                  plotlyOutput("occupancyTimeline")
                ),
                box(
                  title = "Occupancy Rate Table", status = "warning", solidHeader = TRUE,
                  width = 4, height = 630,
                  DT::dataTableOutput("occupancyRateTable")
                )
              ),
              fluidRow(
                box(
                  title = "Daily Occupancy Rate", status = "info", solidHeader = TRUE,
                  width = 8, height = 500,
                  plotlyOutput("dailyOccupancy")
                ),
                box(
                  title = "Occupancy Stats", status = "success", solidHeader = TRUE,
                  width = 4, height = 500,
                  plotlyOutput("occupancyStats")
                )
              )
      ),
      
      # Detailed Reports Tab
      tabItem(tabName = "reports",
              fluidRow(
                box(
                  title = "Property Performance Summary", status = "primary", solidHeader = TRUE,
                  width = 12, 
                  DT::dataTableOutput("propertyReport")
                )
              )
      ),
      
      # Guest Management Tab
      tabItem(tabName = "guests",
              fluidRow(
                box(
                  title = "Guest Reservations", status = "primary", solidHeader = TRUE,
                  width = 12, 
                  DT::dataTableOutput("guestManagement")
                )
              )
              )
      ),
      # Raw Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "All Reservations", status = "primary", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("reservationsTable")
                )
              )
      )
    )
  )

# Server
server <- function(input, output, session) {
  
  # Load data (replace with your data loading logic)
  raw_data <- reactive({
    import_data()  # Replace with: read.csv("your_data.csv") or database connection
  })
  
  property_data <- reactive({
    property_input()$cohost  # Replace with: read.csv("your_data.csv") or database connection
  })
  
  # Update filter choices
  observe({
    data <- raw_data()
    Property <- property_data()
    data <- data %>% filter(Listing %in% Property$Listing)
    
    # Update property choices
    properties <- sort(Property$Listing)
    updateSelectizeInput(session, "propertyFilter", choices = properties)
    
    # Update platform choices
    booking_platforms <- sort(unique(data$booking_platform))
    updateSelectizeInput(session, "platformFilter", choices = booking_platforms)
    
    # Update status choices
    #statuses <- sort(unique(data$booking_status))
    #updateSelectizeInput(session, "statusFilter", choices = statuses)
  })
  
  # Filtered data
  filtered_data <- reactive({
    data <- raw_data()
    Property <- property_data()
    data <- data %>% filter(Listing %in% Property$Listing)
    
    # Date filter
    data <- data[data$checkout_date >= input$dateRange[1] & 
                   data$checkin_date <= input$dateRange[2], ]
    
    # Property filter
    if (!is.null(input$propertyFilter) && length(input$propertyFilter) > 0) {
      data <- data[data$Listing %in% input$propertyFilter, ]
    }
    
    # Platform filter
    if (!is.null(input$platformFilter) && length(input$platformFilter) > 0) {
      data <- data[data$booking_platform %in% input$platformFilter, ]
    }

    return(data)
  })
 
  # Filtered all years data
  filtered_year_data <- reactive({
    data <- raw_data() 
    Property <- property_data()
    data <- data %>% 
      filter(checkin_date >= '2023-01-01' & Listing %in% Property$Listing)
    
    # Property filter
    if (!is.null(input$propertyFilter) && length(input$propertyFilter) > 0) {
      data <- data[data$Listing %in% input$propertyFilter, ]
    }
    
    # Platform filter
    if (!is.null(input$platformFilter) && length(input$platformFilter) > 0) {
      data <- data[data$booking_platform %in% input$platformFilter, ]
    }
    
    return(data)
  })

  # Filtered occupancy data
  filtered_occupancy_data <- reactive({
    data <- filtered_data()
    daily <- data %>% select(checkin_date,checkout_date,Confirmation.Code,Listing,
                             DailyListingPrice,AvgDailyRate) %>%
      rowwise() %>%
      mutate(date = purrr::pmap(list(checkin_date, checkout_date),
                                ~ seq(from = ..1, to = ..2, by = "day")[-1])) %>% # nights only
      unnest(date) %>%
      ungroup() %>%
      filter(date>=input$dateRange[1] & date<=input$dateRange[2]) %>%
      mutate(yearmonth = format(date,"%Y-%m")) %>%
      group_by(Listing,yearmonth) %>%
      reframe(nights=n(),
              Revenue = sum(DailyListingPrice),
              AvgDailyRate = sum(AvgDailyRate)/n()) %>% 
      mutate(occupancyRate=round(nights/days_in_month(as.Date(paste0(yearmonth,"-01"))),2))
    return(daily)
  })
  
  # Filtered occupancy data
  filtered_occupancy_allyear_data <- reactive({
    data <- filtered_year_data()
    daily <- data %>% select(checkin_date,checkout_date,Confirmation.Code,Listing,
                             DailyListingPrice,AvgDailyRate) %>%
      rowwise() %>%
      mutate(date = purrr::pmap(list(checkin_date, checkout_date),
                                ~ seq(from = ..1, to = ..2, by = "day")[-1])) %>% # nights only
      unnest(date) %>%
      ungroup() %>%
      filter(date>=input$dateRange[1] & date<=input$dateRange[2]) %>%
      mutate(yearmonth = format(date,"%Y-%m")) %>%
      group_by(Listing,yearmonth) %>%
      reframe(nights=n(),
              Revenue = sum(DailyListingPrice),
              AvgDailyRate = sum(AvgDailyRate)/n()) %>% 
      mutate(occupancyRate=round(nights/days_in_month(as.Date(paste0(yearmonth,"-01"))),2))
    return(daily)
  })
  
  # Filtered allproperties data
  allproperties_data <- reactive({
    data <- raw_data()
    Property <- property_data()
    data <- data %>% filter(Listing %in% Property$Listing)
   
    # Date filter
    data <- data[data$checkout_date >= input$dateRange[1] & 
                 data$checkin_date <= input$dateRange[2], ]
  
    # Platform filter
    if (!is.null(input$platformFilter) && length(input$platformFilter) > 0) {
    data <- data[data$booking_platform %in% input$platformFilter, ]
    }
    return(data)
  })
  
  # Value boxes
  output$totalReservations <- renderValueBox({
    valueBox(
      value = format(nrow(filtered_data()),big.mark = ","),
      subtitle = "Total Reservations",
      icon = icon("bed"),
      color = "blue"
    )
  })
  
  output$totalRevenue <- renderValueBox({
    revenue <- sum(filtered_data()$total_revenue, na.rm = TRUE)
    valueBox(
      value = paste0("$", format(revenue, big.mark = ",")),
      subtitle = "Total Revenue",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$totalPayout <- renderValueBox({
    payout <- sum(filtered_data()$earnings, na.rm = TRUE)
    valueBox(
      value = paste0("$", format(payout, big.mark = ",")),
      subtitle = "Total Earnings",
      icon = icon("money-bill"),
      color = "yellow"
    )
  })
  
  output$avgNightly <- renderValueBox({
    avg_rate <- round(mean(filtered_data()$AvgDailyRate, na.rm = TRUE), 0)
    valueBox(
      value = paste0("$", avg_rate),
      subtitle = "Avg Nightly Rate",
      icon = icon("tag"),
      color = "purple"
    )
  })
  
  output$avgStay <- renderValueBox({
    avg_nights <- round(mean(filtered_data()$nights, na.rm = TRUE), 1)
    valueBox(
      value = paste0(avg_nights, " nights"),
      subtitle = "Average Stay",
      icon = icon("moon"),
      color = "teal"
    )
  })
  
  output$occupancyRate <- renderValueBox({
    data <- filtered_occupancy_data()
    total_possible_nights <- (input$dateRange[2]-input$dateRange[1]+1) * length(unique(data$Listing))
    occupied_nights <- sum(data$nights, na.rm = TRUE)
    occupancy <- round((occupied_nights / as.numeric(total_possible_nights)) * 100, 1)
    
    valueBox(
      value = paste0(occupancy, "%"),
      subtitle = "Occupancy Rate",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  # Monthly revenue vs payout
  output$monthlyRevenuePayout <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month) %>%
      reframe(
        Revenue = sum(total_revenue, na.rm = TRUE),
        Payout = sum(earnings, na.rm = TRUE),
        checkin_early = min(checkin_date_plot,na.rm=T)) %>%
      tidyr::pivot_longer(cols = c(Revenue, Payout), 
                          names_to = "Type", values_to = "Amount")
    
    p <- ggplot(data, aes(x = checkin_early, y = Amount, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Monthly Revenue vs Payout", x = "", y = "Amount ($)") +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_datetime(date_breaks ='3 month', labels = date_format("%Y-%m")) +
      scale_fill_manual(values = c("Revenue" = "steelblue", "Payout" = "darkgreen")) #+
      #theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  #Platform distribution
  output$platformDistribution <- renderPlotly({

    data <- filtered_data()
    if(nrow(data)>0){
    data = data %>%
      group_by(booking_platform) %>%
      reframe(count = length(unique(Confirmation.Code)))

    p <- ggplot(data, aes(x = "", y = count, fill = booking_platform)) +
      geom_bar(stat = "identity", width = 1) +
      #coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Platform Distribution", fill = "Platform")

    ggplotly(p)}
  })

  # Top properties
  output$topProperties <- renderPlotly({
    data <- filtered_data() 
    if(nrow(data)>0) {
      data = data %>%
      group_by(Listing) %>%
      reframe(revenue = sum(total_revenue, na.rm = TRUE)) %>%
      top_n(10, revenue) %>%
      arrange(desc(revenue))

    p <- ggplot(data, aes(x = reorder(Listing, revenue), y = revenue)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Properties by Revenue", x = "Property", y = "Revenue ($)") +
      scale_y_continuous(labels = dollar_format())
    
    ggplotly(p)}
  })
  
  # Revenue breakdown
  output$revenueBreakdown <- renderPlotly({
    data <- filtered_data() %>%
      reframe(
        Gross_Revenue = sum(earnings, na.rm = TRUE),
        Cleaning_Fees = sum(cleaning_fee, na.rm = TRUE)) %>%
      tidyr::pivot_longer(everything(), names_to = "Component", values_to = "Amount")
    
    p <- ggplot(data, aes(x = Component, y = Amount, fill = Component)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Revenue Components", x = "", y = "Amount ($)") +
      scale_y_continuous(labels = dollar_format()) +
      theme(egend.position = "none")
    
    ggplotly(p)
  })
  
  # cleaning fees
  output$cleaningFees <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Listing) %>%
      reframe(
        revenue = sum(total_revenue, na.rm = TRUE),
        fees = sum(cleaning_fee, na.rm = TRUE),
        fee_rate = round((fees/revenue) * 100, 1))

    p <- ggplot(data, aes(x = Listing, y = fee_rate, fill = Listing)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Cleaning Fee Rates", x = "", y = "Cleaning Fee Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

    ggplotly(p)
  })
  
  # Revenue trend
  output$revenueTrend <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month, booking_platform) %>%
      reframe(revenue = sum(total_revenue, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = month, y = revenue, color = booking_platform, group = booking_platform)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Revenue Trend by Platform", x = "Month", y = "Revenue ($)", color = "Platform") +
      scale_y_continuous(labels = dollar_format()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Revenue Yearly
  output$revenueYear <- renderPlotly({
    data <- filtered_year_data() %>%
      mutate(yearmonth = month,
             year=format(checkin_date, "%Y"),
             month = format(checkin_date, "%m")) %>%
      group_by(yearmonth,year,month) %>%
      reframe(revenue = sum(total_revenue, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = month, y = revenue, color = year, 
                          group = year)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Revenue Yearly Comparison", x = "Month", y = "Revenue ($)", 
           color = "Year") +
      scale_y_continuous(labels = dollar_format())
    
    ggplotly(p)
  })
  
  
  # Platform performance
  output$platformPerformance <- renderPlotly({
    data <- filtered_data() %>%
      group_by(booking_platform) %>%
      reframe(
        bookings = length(unique(Confirmation.Code)),
        revenue = sum(total_revenue, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = bookings, y = revenue, size = bookings, color = booking_platform)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Platform Performance", x = "Number of Bookings", y = "Revenue ($)", 
           color = "Platform", size = "Bookings") +
      scale_y_continuous(labels = dollar_format())
    
    ggplotly(p)
  })
  
  # platform trend
  output$platformTrend <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month, booking_platform) %>%
      reframe(count = length(unique(Confirmation.Code)))

    p <- ggplot(data, aes(x = month, y = count, 
                          group = booking_platform,
                          color = booking_platform)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Reservations by Platform", 
              x = "", y = "Number of Bookings") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })
  
  # platform Yearly
  output$platformYear <- renderPlotly({
    data <- filtered_year_data() %>%
      mutate(yearmonth = month,
             year=format(checkin_date, "%Y"),
             month = format(checkin_date, "%m")) %>%
      group_by(yearmonth,year,month) %>%
      reframe(count = length(unique(Confirmation.Code)))
    
    p <- ggplot(data, aes(x = month, y = count, color = year, 
                          group = year)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Reservations Yearly Comparison", x = "Month", 
           y = "Number of Bookings",color = "Year") 
    
    ggplotly(p)
  })
  
  # Platform comparison table
  output$platformComparison <- DT::renderDataTable({
    data <- filtered_data() %>%
      group_by(booking_platform) %>%
      reframe(
        Total_Bookings = length(unique(Confirmation.Code)),
        Total_Revenue = sum(total_revenue, na.rm = TRUE),
        Total_Payout = sum(earnings, na.rm = TRUE),
        Avg_Nightly_Rate = round(mean(AvgDailyRate, na.rm = TRUE), 2)) %>%
      arrange(desc(Total_Revenue))
    
    DT::datatable(data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("Total_Revenue", "Total_Payout", "Avg_Nightly_Rate")) 
    #%>%
     # DT::formatPercentage("Fee_Rate", 0)
  })
  
  # Guest management table
  output$guestManagement <- DT::renderDataTable({
    data <- filtered_data() %>%
      select(Listing, booking_platform,nights,earnings,
             checkin_date,checkout_date, guests, adults,children,infants) %>%
      arrange(desc(checkin_date),Listing)
    
    DT::datatable(data, 
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("earnings")) %>%
      DT::formatDate(c("checkin_date"))
  })
  
  # Occupancy Rate
  output$occupancyTimeline <- renderPlotly({
    data <- filtered_occupancy_data()
    data <- data %>% mutate(occupancyRate = round(occupancyRate*100,1))
    p <- ggplot(data, aes(x = yearmonth,y = occupancyRate, color = Listing,fill=Listing)) +
        geom_bar(stat='identity',position=position_dodge2()) +
      # geom_segment(aes(xend = occupancyRate, yend = Listing), 
      #             size = 4, alpha = 0.8) +
      theme_minimal() +
      labs(title = "Occupancy Rates by Property", x = "", y = "Occupancy Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Yearly summary table
  output$yearlyrevenue <- DT::renderDataTable({
     data =  filtered_occupancy_allyear_data() %>% 
      mutate(Year = as.integer(substr(yearmonth,1,4))) %>%
      group_by(Listing,Year) %>%
      reframe(Revenue = sum(Revenue,na.rm=T),
              ADR=Revenue/sum(nights,na.rm=T),
              OccRt=sum(nights,na.rm=T)/365) %>%
       mutate(OccRt = paste0(round(OccRt*100,1),"%")) %>%
      filter(Year %in% c(year(Sys.Date()),year(Sys.Date())-1)) %>%
      mutate(Time = ifelse(Year %in% year(Sys.Date()),"This_Yr","Last_Yr")) %>%
      pivot_wider(id_cols=Listing,names_from = Time, 
                  values_from = c(Revenue,ADR,OccRt)) %>% 
      select(Listing,Revenue_Last_Yr,Revenue_This_Yr, 
            ADR_Last_Yr, ADR_This_Yr,
            OccRt_Last_Yr,OccRt_This_Yr) 
     
     DT::datatable(data.frame(data),options = list(pageLength = 12, scrollX = TRUE),
                   rownames = FALSE) %>%
      DT::formatCurrency(c("Revenue_This_Yr", 
                           "ADR_Last_Yr", "ADR_This_Yr")) 
  })
  
  # Occupancy Rate table
  output$occupancyRateTable <- DT::renderDataTable({
    DT::datatable(filtered_occupancy_data() %>% 
                    select(Listing,Time=yearmonth,Revenue,ADR=AvgDailyRate,
                           OccRt=occupancyRate) %>%
                    mutate(OccRt = paste0(round(OccRt*100,1),"%")), 
                  options = list(pageLength = 12, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("ADR", "Revenue")) 
  })
  
  # Daily Occupancy Rate across all properties
  output$dailyOccupancy <- renderPlotly({
    data <- allproperties_data()
    total_properties <- length(unique(data$Listing))
    
    # Calculate daily occupancy 
    occupancy_data <- data %>% 
      rowwise() %>%
      mutate(date = purrr::pmap(list(checkin_date, checkout_date),
        ~ seq(from = ..1, to = ..2, by = "day")[-1])) %>%
      unnest(date) %>%
      filter(date>=input$dateRange[1] & date<=input$dateRange[2]) %>%
      group_by(date) %>%
      reframe(occupied_properties=length(unique(Listing))) %>%
      mutate(occupancy_rate = (occupied_properties / total_properties) * 100)
    
    p <- ggplot(occupancy_data, aes(x = date, y = occupancy_rate)) +
      geom_line(color = "darkred", linewidth = 1) +
      geom_smooth(method = "loess", color = "blue", alpha = 0.3) +
      theme_minimal() +
      labs(title = "Daily Occupancy Rate across all properties", 
           x = "", y = "Occupancy Rate (%)")
    
    ggplotly(p)
  })
  
  # Additional outputs for occupancy stats and heatmap
  output$occupancyStats <- renderPlotly({
    data <- allproperties_data()
    
    daily <- data %>% select(checkin_date,checkout_date,Confirmation.Code,Listing,
                             DailyListingPrice,AvgDailyRate) %>%
      rowwise() %>%
      mutate(date = purrr::pmap(list(checkin_date, checkout_date),
                                ~ seq(from = ..1, to = ..2, by = "day")[-1])) %>% # nights only
      unnest(date) %>%
      ungroup() %>%
      filter(date>=input$dateRange[1] & date<=input$dateRange[2]) %>%
      mutate(yearmonth = format(date,"%Y-%m")) %>%
      group_by(Listing,yearmonth) %>%
      reframe(nights=n())
    
    data <- daily %>%
      group_by(Listing) %>%
      reframe(occupancy_nights = sum(nights, na.rm = TRUE))
    
    data$highlight = "normal"
    # Property filter
    if (!is.null(input$propertyFilter) && length(input$propertyFilter) > 0) {
      data$highlight = ifelse(data$Listing %in% input$propertyFilter,"highlight","normal")
    }  
    
    p <- ggplot(data, aes(x = reorder(Listing, occupancy_nights), 
                          y = occupancy_nights,fill=highlight)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("highlight" = "red", "normal" = "coral")) +
      theme_minimal() +
      labs(title = "Total Nights by Property", x = "Property", y = "Total Nights") +
      theme(legend.position = "none")

    ggplotly(p)
  })
  
  output$revenueHeatmap <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Listing, booking_platform) %>%
      reframe(revenue = sum(total_revenue, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = booking_platform, y = Listing, fill = revenue)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue", labels = dollar_format()) +
      theme_minimal() +
      labs(title = "Revenue Heatmap: Property vs Platform", 
           x = "Booking Platform", y = "Property", fill = "Revenue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Property report
  output$propertyReport <- DT::renderDataTable({
    data <- filtered_data() %>%
      group_by(Listing) %>%
      reframe(
        Total_Bookings = length(unique(Confirmation.Code)),
        Total_Nights = sum(nights, na.rm = TRUE),
        Avg_Stay = round(mean(nights, na.rm = TRUE), 1),
        Total_Revenue = sum(total_revenue, na.rm = TRUE),
        Total_Payout = sum(earnings, na.rm = TRUE),
        Avg_Nightly_Rate = round(mean(AvgDailyRate, na.rm = TRUE), 2),
        Top_Platform = names(sort(table(booking_platform), decreasing = TRUE))[1]) %>%
      arrange(desc(Total_Revenue))
    
    DT::datatable(data, 
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("Total_Revenue", "Total_Payout", "Avg_Nightly_Rate")) %>%
      DT::formatRound("Avg_Stay", 1)
  })
  
  # Raw data table
  output$reservationsTable <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                  options = list(pageLength = 25, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("AvgDailyRate", "earnings", "total_revenue", 
                           "cleaning_fee")) %>%
      DT::formatDate(c("checkin_date", "checkout_date"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)