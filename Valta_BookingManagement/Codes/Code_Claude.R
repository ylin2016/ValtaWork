# Property Management Dashboard - R Shiny (Enhanced Version)
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Sample data generation (replace with your actual data loading)
generate_sample_data <- function() {
  set.seed(123)
  properties <- c("Ocean View Villa", "Mountain Cabin", "City Loft", "Beach House", "Lake Cottage")
  booking_platforms <- c("Airbnb", "VRBO", "Booking.com", "Direct", "Expedia")
  booking_sources <- c("Organic Search", "Paid Ads", "Social Media", "Referral", "Direct")
  booking_statuses <- c("Confirmed", "Checked-in", "Checked-out", "Cancelled", "Pending")
  
  # Generate sample reservations
  reservations <- data.frame(
    property_id = paste0("PROP_", sprintf("%03d", rep(1:5, each = 60))),
    property_name = rep(properties, each = 60),
    checkin_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), 300, replace = TRUE),
    guest_name = paste(sample(c("John", "Jane", "Mike", "Sarah", "David", "Lisa", "Tom", "Anna"), 300, replace = TRUE),
                       sample(c("Smith", "Johnson", "Brown", "Davis", "Wilson", "Miller", "Taylor", "Anderson"), 300, replace = TRUE)),
    guest_email = paste0(tolower(sample(c("john", "jane", "mike", "sarah", "david", "lisa", "tom", "anna"), 300, replace = TRUE)),
                         sample(1:999, 300, replace = TRUE), "@",
                         sample(c("gmail.com", "yahoo.com", "hotmail.com", "outlook.com"), 300, replace = TRUE)),
    guest_phone = paste0("+1-", sample(200:999, 300, replace = TRUE), "-", sample(100:999, 300, replace = TRUE), "-", sample(1000:9999, 300, replace = TRUE)),
    booking_platform = sample(booking_platforms, 300, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    booking_source = sample(booking_sources, 300, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.15, 0.15)),
    booking_status = sample(booking_statuses, 300, replace = TRUE, prob = c(0.4, 0.2, 0.25, 0.1, 0.05)),
    stringsAsFactors = FALSE
  )
  
  # Add checkout dates (2-14 days after checkin)
  reservations$checkout_date <- reservations$checkin_date + sample(2:14, nrow(reservations), replace = TRUE)
  
  # Add financial fields
  reservations$nights <- as.numeric(reservations$checkout_date - reservations$checkin_date)
  reservations$nightly_rate <- sample(80:350, nrow(reservations), replace = TRUE)
  reservations$gross_revenue <- reservations$nights * reservations$nightly_rate
  
  # Add cleaning fees and taxes
  reservations$cleaning_fee <- sample(c(0, 25, 50, 75, 100), nrow(reservations), replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))
  reservations$taxes <- round(reservations$gross_revenue * 0.08, 2)  # 8% tax
  reservations$total_revenue <- reservations$gross_revenue + reservations$cleaning_fee + reservations$taxes
  
  # Platform fees (different rates for different platforms)
  platform_fee_rates <- c("Airbnb" = 0.15, "VRBO" = 0.12, "Booking.com" = 0.18, "Direct" = 0.03, "Expedia" = 0.20)
  reservations$platform_fee <- round(reservations$total_revenue * platform_fee_rates[reservations$booking_platform], 2)
  reservations$total_payout <- reservations$total_revenue - reservations$platform_fee
  
  # Add month for grouping
  reservations$month <- format(reservations$checkin_date, "%Y-%m")
  
  return(reservations)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Property Management Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Revenue Analysis", tabName = "revenue", icon = icon("dollar-sign")),
      menuItem("Booking Platforms", tabName = "platforms", icon = icon("laptop")),
      menuItem("Guest Management", tabName = "guests", icon = icon("users")),
      menuItem("Occupancy", tabName = "occupancy", icon = icon("calendar-alt")),
      menuItem("Detailed Reports", tabName = "reports", icon = icon("chart-line")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    ),
    
    # Filters
    dateRangeInput("dateRange", 
                   "Date Range:",
                   start = Sys.Date() - 90,
                   end = Sys.Date() + 90),
    
    selectizeInput("propertyFilter", 
                   "Properties:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "All Properties")),
    
    selectizeInput("platformFilter", 
                   "Booking Platforms:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "All Platforms")),
    
    selectizeInput("statusFilter", 
                   "Booking Status:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "All Statuses"))
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
                  width = 8, height = 400,
                  plotlyOutput("monthlyRevenuePayout")
                ),
                box(
                  title = "Booking Status Distribution", status = "info", solidHeader = TRUE,
                  width = 4, height = 400,
                  plotlyOutput("statusDistribution")
                )
              ),
              fluidRow(
                box(
                  title = "Top Properties by Revenue", status = "success", solidHeader = TRUE,
                  width = 12, height = 350,
                  plotlyOutput("topProperties")
                )
              )
      ),
      
      # Revenue Analysis Tab
      tabItem(tabName = "revenue",
              fluidRow(
                box(
                  title = "Revenue Breakdown", status = "success", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("revenueBreakdown")
                ),
                box(
                  title = "Platform Fee Analysis", status = "warning", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("platformFees")
                )
              ),
              fluidRow(
                box(
                  title = "Revenue Trend Analysis", status = "primary", solidHeader = TRUE,
                  width = 12, height = 400,
                  plotlyOutput("revenueTrend")
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
      
      # Booking Platforms Tab
      tabItem(tabName = "platforms",
              fluidRow(
                box(
                  title = "Platform Performance", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("platformPerformance")
                ),
                box(
                  title = "Booking Sources", status = "info", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("bookingSources")
                )
              ),
              fluidRow(
                box(
                  title = "Platform Comparison Table", status = "warning", solidHeader = TRUE,
                  width = 12, height = 400,
                  DT::dataTableOutput("platformComparison")
                )
              )
      ),
      
      # Guest Management Tab
      tabItem(tabName = "guests",
              fluidRow(
                box(
                  title = "Guest Reservations", status = "primary", solidHeader = TRUE,
                  width = 12, height = 600,
                  DT::dataTableOutput("guestManagement")
                )
              )
      ),
      
      # Occupancy Tab
      tabItem(tabName = "occupancy",
              fluidRow(
                box(
                  title = "Occupancy Timeline by Property", status = "primary", solidHeader = TRUE,
                  width = 12, height = 500,
                  plotlyOutput("occupancyTimeline")
                )
              ),
              fluidRow(
                box(
                  title = "Daily Occupancy Rate", status = "info", solidHeader = TRUE,
                  width = 8, height = 400,
                  plotlyOutput("dailyOccupancy")
                ),
                box(
                  title = "Occupancy Stats", status = "success", solidHeader = TRUE,
                  width = 4, height = 400,
                  plotlyOutput("occupancyStats")
                )
              )
      ),
      
      # Detailed Reports Tab
      tabItem(tabName = "reports",
              fluidRow(
                box(
                  title = "Property Performance Summary", status = "primary", solidHeader = TRUE,
                  width = 12, height = 600,
                  DT::dataTableOutput("propertyReport")
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
)

# Server
server <- function(input, output, session) {
  
  # Load data (replace with your data loading logic)
  raw_data <- reactive({
    generate_sample_data()  # Replace with: read.csv("your_data.csv") or database connection
  })
  
  # Update filter choices
  observe({
    data <- raw_data()
    
    # Update property choices
    properties <- sort(unique(data$property_name))
    updateSelectizeInput(session, "propertyFilter", choices = properties)
    
    # Update platform choices
    platforms <- sort(unique(data$booking_platform))
    updateSelectizeInput(session, "platformFilter", choices = platforms)
    
    # Update status choices
    statuses <- sort(unique(data$booking_status))
    updateSelectizeInput(session, "statusFilter", choices = statuses)
  })
  
  # Filtered data
  filtered_data <- reactive({
    data <- raw_data()
    
    # Date filter
    data <- data[data$checkin_date >= input$dateRange[1] & 
                   data$checkin_date <= input$dateRange[2], ]
    
    # Property filter
    if (!is.null(input$propertyFilter) && length(input$propertyFilter) > 0) {
      data <- data[data$property_name %in% input$propertyFilter, ]
    }
    
    # Platform filter
    if (!is.null(input$platformFilter) && length(input$platformFilter) > 0) {
      data <- data[data$booking_platform %in% input$platformFilter, ]
    }
    
    # Status filter
    if (!is.null(input$statusFilter) && length(input$statusFilter) > 0) {
      data <- data[data$booking_status %in% input$statusFilter, ]
    }
    
    return(data)
  })
  
  # Value boxes
  output$totalReservations <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
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
    payout <- sum(filtered_data()$total_payout, na.rm = TRUE)
    valueBox(
      value = paste0("$", format(payout, big.mark = ",")),
      subtitle = "Total Payout",
      icon = icon("money-bill"),
      color = "yellow"
    )
  })
  
  output$avgNightly <- renderValueBox({
    avg_rate <- round(mean(filtered_data()$nightly_rate, na.rm = TRUE), 0)
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
    data <- filtered_data()
    total_possible_nights <- as.numeric(max(data$checkout_date) - min(data$checkin_date)) * length(unique(data$property_name))
    occupied_nights <- sum(data$nights, na.rm = TRUE)
    occupancy <- round((occupied_nights / total_possible_nights) * 100, 1)
    
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
      summarise(
        Revenue = sum(total_revenue, na.rm = TRUE),
        Payout = sum(total_payout, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(cols = c(Revenue, Payout), names_to = "Type", values_to = "Amount")
    
    p <- ggplot(data, aes(x = month, y = Amount, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Monthly Revenue vs Payout", x = "Month", y = "Amount ($)") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_manual(values = c("Revenue" = "steelblue", "Payout" = "darkgreen")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Status distribution
  output$statusDistribution <- renderPlotly({
    data <- filtered_data() %>%
      group_by(booking_status) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(data, aes(x = "", y = count, fill = booking_status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Booking Status", fill = "Status")
    
    ggplotly(p)
  })
  
  # Top properties
  output$topProperties <- renderPlotly({
    data <- filtered_data() %>%
      group_by(property_name) %>%
      summarise(revenue = sum(total_revenue, na.rm = TRUE), .groups = "drop") %>%
      top_n(10, revenue) %>%
      arrange(desc(revenue))
    
    p <- ggplot(data, aes(x = reorder(property_name, revenue), y = revenue)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Properties by Revenue", x = "Property", y = "Revenue ($)") +
      scale_y_continuous(labels = dollar_format())
    
    ggplotly(p)
  })
  
  # Revenue breakdown
  output$revenueBreakdown <- renderPlotly({
    data <- filtered_data() %>%
      summarise(
        `Gross Revenue` = sum(gross_revenue, na.rm = TRUE),
        `Cleaning Fees` = sum(cleaning_fee, na.rm = TRUE),
        `Taxes` = sum(taxes, na.rm = TRUE),
        `Platform Fees` = sum(platform_fee, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Component", values_to = "Amount")
    
    p <- ggplot(data, aes(x = Component, y = Amount, fill = Component)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Revenue Components", x = "", y = "Amount ($)") +
      scale_y_continuous(labels = dollar_format()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  # Platform fees
  output$platformFees <- renderPlotly({
    data <- filtered_data() %>%
      group_by(booking_platform) %>%
      summarise(
        revenue = sum(total_revenue, na.rm = TRUE),
        fees = sum(platform_fee, na.rm = TRUE),
        fee_rate = round((fees/revenue) * 100, 1),
        .groups = "drop"
      )
    
    p <- ggplot(data, aes(x = booking_platform, y = fee_rate, fill = booking_platform)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Platform Fee Rates", x = "Platform", y = "Fee Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  # Revenue trend
  output$revenueTrend <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month, booking_platform) %>%
      summarise(revenue = sum(total_revenue, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = month, y = revenue, color = booking_platform, group = booking_platform)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Revenue Trend by Platform", x = "Month", y = "Revenue ($)", color = "Platform") +
      scale_y_continuous(labels = dollar_format()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Platform performance
  output$platformPerformance <- renderPlotly({
    data <- filtered_data() %>%
      group_by(booking_platform) %>%
      summarise(
        bookings = n(),
        revenue = sum(total_revenue, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(data, aes(x = bookings, y = revenue, size = bookings, color = booking_platform)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Platform Performance", x = "Number of Bookings", y = "Revenue ($)", 
           color = "Platform", size = "Bookings") +
      scale_y_continuous(labels = dollar_format())
    
    ggplotly(p)
  })
  
  # Booking sources
  output$bookingSources <- renderPlotly({
    data <- filtered_data() %>%
      group_by(booking_source) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(data, aes(x = reorder(booking_source, count), y = count, fill = booking_source)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Reservations by Source", x = "Booking Source", y = "Number of Bookings") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Platform comparison table
  output$platformComparison <- DT::renderDataTable({
    data <- filtered_data() %>%
      group_by(booking_platform) %>%
      summarise(
        Total_Bookings = n(),
        Total_Revenue = sum(total_revenue, na.rm = TRUE),
        Total_Payout = sum(total_payout, na.rm = TRUE),
        Avg_Nightly_Rate = round(mean(nightly_rate, na.rm = TRUE), 2),
        Platform_Fees = sum(platform_fee, na.rm = TRUE),
        Fee_Rate = round((sum(platform_fee, na.rm = TRUE) / sum(total_revenue, na.rm = TRUE)) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Revenue))
    
    DT::datatable(data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("Total_Revenue", "Total_Payout", "Avg_Nightly_Rate", "Platform_Fees")) %>%
      DT::formatPercentage("Fee_Rate", 0)
  })
  
  # Guest management table
  output$guestManagement <- DT::renderDataTable({
    data <- filtered_data() %>%
      select(guest_name, guest_email, guest_phone, property_name, 
             checkin_date, checkout_date, nights, booking_platform,
             booking_status, total_revenue, total_payout) %>%
      arrange(desc(checkin_date))
    
    DT::datatable(data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("total_revenue", "total_payout")) %>%
      DT::formatDate(c("checkin_date", "checkout_date"))
  })
  
  # Occupancy timeline
  output$occupancyTimeline <- renderPlotly({
    data <- filtered_data() %>%
      filter(booking_status %in% c("Confirmed", "Checked-in", "Checked-out"))
    
    p <- ggplot(data, aes(y = property_name, color = booking_platform)) +
      geom_segment(aes(x = checkin_date, xend = checkout_date, 
                       yend = property_name), 
                   size = 4, alpha = 0.8) +
      theme_minimal() +
      labs(title = "Occupancy Timeline", x = "Date", y = "Property", color = "Platform") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Property report
  output$propertyReport <- DT::renderDataTable({
    data <- filtered_data() %>%
      group_by(property_name) %>%
      summarise(
        Total_Bookings = n(),
        Confirmed = sum(booking_status == "Confirmed"),
        Checked_Out = sum(booking_status == "Checked-out"),
        Cancelled = sum(booking_status == "Cancelled"),
        Total_Nights = sum(nights, na.rm = TRUE),
        Avg_Stay = round(mean(nights, na.rm = TRUE), 1),
        Total_Revenue = sum(total_revenue, na.rm = TRUE),
        Total_Payout = sum(total_payout, na.rm = TRUE),
        Avg_Nightly_Rate = round(mean(nightly_rate, na.rm = TRUE), 2),
        Top_Platform = names(sort(table(booking_platform), decreasing = TRUE))[1],
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Revenue))
    
    DT::datatable(data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("Total_Revenue", "Total_Payout", "Avg_Nightly_Rate")) %>%
      DT::formatRound("Avg_Stay", 1)
  })
  
  # Raw data table
  output$reservationsTable <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                  options = list(pageLength = 25, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("nightly_rate", "gross_revenue", "total_revenue", 
                           "cleaning_fee", "taxes", "platform_fee", "total_payout")) %>%
      DT::formatDate(c("checkin_date", "checkout_date"))
  })
  
  # Additional outputs for occupancy stats and heatmap
  output$occupancyStats <- renderPlotly({
    data <- filtered_data() %>%
      group_by(property_name) %>%
      summarise(occupancy_nights = sum(nights, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = reorder(property_name, occupancy_nights), y = occupancy_nights)) +
      geom_bar(stat = "identity", fill = "coral") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Total Nights by Property", x = "Property", y = "Total Nights")
    
    ggplotly(p)
  })
  
  output$dailyOccupancy <- renderPlotly({
    data <- filtered_data()
    total_properties <- length(unique(data$property_name))
    
    # Calculate daily occupancy
    all_dates <- seq(from = min(data$checkin_date), to = max(data$checkout_date), by = "day")
    occupancy_data <- data.frame(date = all_dates)
    
    occupancy_data$occupied_properties <- sapply(all_dates, function(d) {
      sum(data$checkin_date <= d & data$checkout_date > d)
    })
    
    occupancy_data$occupancy_rate <- (occupancy_data$occupied_properties / total_properties) * 100
    
    p <- ggplot(occupancy_data, aes(x = date, y = occupancy_rate)) +
      geom_line(color = "darkred", size = 1) +
      geom_smooth(method = "loess", color = "blue", alpha = 0.3) +
      theme_minimal() +
      labs(title = "Daily Occupancy Rate", x = "Date", y = "Occupancy Rate (%)")
    
    ggplotly(p)
  })
  
  output$revenueHeatmap <- renderPlotly({
    data <- filtered_data() %>%
      group_by(property_name, booking_platform) %>%
      summarise(revenue = sum(total_revenue, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = booking_platform, y = property_name, fill = revenue)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue", labels = dollar_format()) +
      theme_minimal() +
      labs(title = "Revenue Heatmap: Property vs Platform", 
           x = "Booking Platform", y = "Property", fill = "Revenue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)