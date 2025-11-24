# Short-Term Rental Review Analysis Dashboard in R Shiny
# Install required packages if needed:
# install.packages(c("shiny", "dplyr", "ggplot2", "stringr", "tidyr", "DT", "jsonlite"))

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(DT)
library(jsonlite)

# ==============================================================================
# DATA PREPARATION
# ==============================================================================
reviews <- read.xlsx("/Users/ylin/My Drive/Cohost/Data and Reporting/06-Reviews/Data/PropertyReviews.xlsx")
load("/Users/ylin/My Drive/Cohost/Data and Reporting/06-Reviews/Data/negative_results.Rdata")
# ==============================================================================
# ANALYSIS FUNCTIONS
# ==============================================================================

analyze_property <- function(reviews_df) {
  reviews_df %>%
    group_by(Listing, PropertyType, Region) %>%
    summarise(
      avg_rating = mean(Overall, na.rm = TRUE),
      review_count = n(),
      bedrooms = first(BEDROOMS),
      .groups = 'drop'
    ) %>%
    mutate(bedrooms = ifelse(is.na(bedrooms), "N/A", as.character(bedrooms)))
}

extract_positives <- function(review_text) {
  if (is.na(review_text)) return(character(0))
  review_lower <- tolower(review_text)
  positives <- c()
  
  if (str_detect(review_lower, "view|water|location")) 
    positives <- c(positives, "Excellent location/views")
  if (str_detect(review_lower, "clean")) 
    positives <- c(positives, "Clean property")
  if (str_detect(review_lower, "responsive|helpful")) 
    positives <- c(positives, "Responsive host")
  if (str_detect(review_lower, "cozy|comfortable")) 
    positives <- c(positives, "Comfortable and cozy")
  if (str_detect(review_lower, "kitchen|equipped")) 
    positives <- c(positives, "Well-equipped kitchen")
  
  return(positives)
}

extract_negatives <- function(review_text) {
  if (is.na(review_text)) return(character(0))
  review_lower <- tolower(review_text)
  negatives <- c()
  
  if (str_detect(review_lower, "lockbox|key")) 
    negatives <- c(negatives, "Lockbox/check-in issues")
  if (str_detect(review_lower, "dirty|dust|musty")) 
    negatives <- c(negatives, "Cleanliness issues")
  if (str_detect(review_lower, "broken|toilet")) 
    negatives <- c(negatives, "Broken fixtures")
  if (str_detect(review_lower, "no ac|noisy")) 
    negatives <- c(negatives, "AC/noise issues")
  if (str_detect(review_lower, "couch|furniture")) 
    negatives <- c(negatives, "Furniture needs improvement")
  if (str_detect(review_lower, "refund|did not reply")) 
    negatives <- c(negatives, "Unresolved issues/refund")
  
  return(negatives)
}
extract_negatives_details <- function(property,results)
{
  issues <- results$issues_by_property[[property]]

  #cat(sprintf('\n%s:\n', property))

  # Sort by count
  negative_detail = NULL
  if(length(issues)>0)
  {
      issue_names <- names(issues)
      counts <- unlist(issues)
      sorted_idx <- order(counts, decreasing = TRUE)

      for (idx in sorted_idx) {
          issue_name <- issue_names[idx]
          count <- counts[idx]
          severity <- issue_categories[[issue_name]]$severity
    
          negative_detail = c(negative_detail,
                        sprintf('%s: %d mention(s) [%s]', 
                                gsub('_', ' ', issue_name), 
                                count,
                                paste(rep('★', severity), collapse='')))
  }}
  negative_detail
}

analyze_issues <- function(reviews_df) {
  issue_counts <- list(
    'Lockbox/Check-in' = 0,
    'Cleanliness' = 0,
    'Broken Fixtures' = 0,
    'AC/Temperature' = 0,
    'Furniture' = 0,
    'Communication' = 0
  )
  
  for (i in 1:nrow(reviews_df)) {
    review <- reviews_df$Public.Review[i]
    if (is.na(review)) next
    review_lower <- tolower(review)
    
    if (str_detect(review_lower, "lockbox|key|access")) 
      issue_counts[['Lockbox/Check-in']] <- issue_counts[['Lockbox/Check-in']] + 1
    if (str_detect(review_lower, "dirty|dust|musty|clean")) 
      issue_counts[['Cleanliness']] <- issue_counts[['Cleanliness']] + 1
    if (str_detect(review_lower, "broken|toilet|fixture")) 
      issue_counts[['Broken Fixtures']] <- issue_counts[['Broken Fixtures']] + 1
    if (str_detect(review_lower, "ac|temperature|hot|cold")) 
      issue_counts[['AC/Temperature']] <- issue_counts[['AC/Temperature']] + 1
    if (str_detect(review_lower, "couch|furniture|chair")) 
      issue_counts[['Furniture']] <- issue_counts[['Furniture']] + 1
    
    if (!is.na(reviews_df$Communication[i]) && reviews_df$Communication[i] < 3)
      issue_counts[['Communication']] <- issue_counts[['Communication']] + 1
  }
  
  data.frame(
    name = names(issue_counts),
    count = unlist(issue_counts),
    stringsAsFactors = FALSE
  ) %>% filter(count > 0)
}

segment_comparison <- function(reviews_df) {
  # By Type
  by_type <- reviews_df %>%
    group_by(PropertyType) %>%
    summarise(
      avg = mean(Overall, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(name = PropertyType)
  
  # By Region
  by_region <- reviews_df %>%
    group_by(Region) %>%
    summarise(
      avg = mean(Overall, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(name = Region)
  
  # By Bedrooms
  by_bedrooms <- reviews_df %>%
    mutate(bedroom_cat = ifelse(BEDROOMS == 1, "1BR", "2BR+")) %>%
    group_by(bedroom_cat) %>%
    summarise(
      avg = mean(Overall, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(name = bedroom_cat)
  
  list(
    by_type = by_type,
    by_region = by_region,
    by_bedrooms = by_bedrooms
  )
}

# ==============================================================================
# SHINY UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Short-Term Rental Review Analysis Dashboard"),
  
  tags$head(tags$style(HTML("
    .positive-box { background-color: #d4edda; border-left: 4px solid #28a745; padding: 10px; margin: 5px 0; }
    .negative-box { background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0; }
    .property-card { background: white; border-radius: 8px; padding: 15px; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    .metric-value { font-size: 32px; font-weight: bold; color: #007bff; }
    .metric-label { font-size: 14px; color: #6c757d; }
    .critical-alert { background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; }
  "))),
  
  tabsetPanel(
    id = "tabs",
    
    # Property Summary Tab
    tabPanel(
      "Property Summary",
      br(),
      h3("Property-Level Summary"),
      uiOutput("property_summaries")
    ),
    
    # Common Issues Tab
    tabPanel(
      "Common Issues",
      br(),
      h3("Common Issues Across Properties"),
      br(),
      plotOutput("issues_chart", height = "400px"),
      br(),
      h4("Issue Details & Recommendations"),
      uiOutput("issue_details")
    ),
    
    # Segment Analysis Tab
    tabPanel(
      "Segment Analysis",
      br(),
      h3("Segment Comparison Analysis"),
      br(),
      fluidRow(
        column(4, 
               h4("By Property Type"),
               plotOutput("segment_type", height = "300px"),
               uiOutput("type_stats")
        ),
        column(4,
               h4("By Region"),
               plotOutput("segment_region", height = "300px"),
               uiOutput("region_stats")
        ),
        column(4,
               h4("By Bedrooms"),
               plotOutput("segment_bedrooms", height = "300px"),
               uiOutput("bedroom_stats")
        )
      ),
      br(),
      div(class = "property-card",
          h4("Key Insights"),
          tags$ul(
            tags$li("Condos perform consistently well across Seattle waterfront properties"),
            tags$li("Single-family homes have fewer reviews but issues with furnishings and amenities"),
            tags$li("Seattle properties leverage waterfront location as major selling point"),
            tags$li("Eastside properties compete on convenience and space"),
            tags$li("1BR units need furniture improvements (add couches)")
          )
      )
    ),
    
    # Critical Issues Tab
    tabPanel(
      "Critical Issues",
      br(),
      h3("Critical Issues & Refund Requests"),
      uiOutput("critical_alert"),
      uiOutput("critical_issues"),
      br(),
      div(class = "property-card",
          h4("Resolution Status Summary"),
          uiOutput("resolution_summary")
      )
    )
  )
)

# ==============================================================================
# SHINY SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # Property Analysis
  property_data <- reactive({
    analyze_property(reviews)
  })

  # Render Property Summaries
  output$property_summaries <- renderUI({
    prop_analysis <- property_data()
    
    property_cards <- lapply(1:nrow(prop_analysis), function(i) {
     # for(i in 1:nrow(prop_analysis))
     # { 
     #   print(i)
      prop <- prop_analysis[i, ]
      prop_reviews <- reviews %>% filter(Listing == prop$Listing)
      
      # Extract positives and negatives
      all_reviews <- paste(prop_reviews$Public.Review, collapse = " ")
      positives <- unique(extract_positives(all_reviews))
      negatives <- unique(extract_negatives(all_reviews))
      negatives_detail <- extract_negatives_details(property=prop$Listing,results)
  
      div(class = "property-card",
          fluidRow(
            column(8,
                   h4(prop$Listing),
                   p(paste(prop$PropertyType, "|", prop$Region, "|", prop$bedrooms, "BR"))
            ),
            column(4, style = "text-align: right;",
                   div(class = "metric-value", round(prop$avg_rating, 1)),
                   div(class = "metric-label", paste(prop$review_count, "reviews"))
            )
          ),
          hr(),
          fluidRow(
            column(6,
                   h5(style = "color: #28a745;", "✓ Strengths"),
                   if (length(positives) > 0) {
                     tags$ul(lapply(positives, tags$li))
                   } else {
                     tags$p(style = "color: #6c757d;", "No specific strengths identified")
                   }
            ),
            column(6,
                   h5(style = "color: #dc3545;", "✗ Issues"),
                   if (length(negatives_detail) > 0) {
                     tags$ul(lapply(negatives_detail, tags$li))
                   } else {
                     tags$p(style = "color: #6c757d;", "No issues reported")
                   }
            )
          )
      )
     }
      
    )
    do.call(tagList, property_cards)
  })
  
  # Issues Analysis
  issues_data <- reactive({
    analyze_issues(reviews)
  })
  
  output$issues_chart <- renderPlot({
    # issues <- issues_data()
    # if (nrow(issues) == 0) return(NULL)
    # 
    # ggplot(issues, aes(x = reorder(name, count), y = count)) +
    #   geom_col(fill = "#4472C4") +
    #   coord_flip() +
    #   labs(title = "Issue Frequency", x = "", y = "Count") +
    #   theme_minimal() +
    #   theme(
    #     plot.title = element_text(size = 16, face = "bold"),
    #     axis.text = element_text(size = 12)
    #   )
    
    levels_def = issue_summary %>% group_by(severity,category) %>% reframe(n())
    issue_summary$category = factor(issue_summary$category,levels = rev(levels_def$category))
    
    ggplot(issue_summary, aes(x = reorder(issue_type, count), 
                              y = count,fill=category)) +
      geom_col() +
      geom_text(aes(label=count)) +
      coord_flip() +
      labs(title = "Issue Frequency", x = "", y = "Count",fill="Severity") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12)
      )
    
  })
  
  output$issue_details <- renderUI({
    # issues <- issues_data()
    # if (nrow(issues) == 0) return(p("No issues detected"))
    # 
    # recommendations <- list(
    #   'Lockbox/Check-in' = 'Improve check-in instructions, test lockbox codes before arrival',
    #   'Cleanliness' = 'Deep cleaning protocol needed, especially for bathrooms and linens',
    #   'Broken Fixtures' = 'Pre-arrival maintenance checks, responsive repair system',
    #   'AC/Temperature' = 'Consider central AC installation or better portable units',
    #   'Furniture' = 'Add comfortable seating, especially couches in living areas',
    #   'Communication' = 'Improve response time and follow-up on guest concerns'
    # )
    # 
    # issue_cards <- lapply(1:nrow(issues), function(i) {
    #   issue <- issues[i, ]
    #   div(style = "border-left: 4px solid #ff8c00; padding-left: 15px; margin: 10px 0;",
    #       h5(issue$name),
    #       p(paste("Mentioned in", issue$count, "review(s)")),
    #       p(style = "font-size: 14px;", 
    #         strong("Recommendation: "), 
    #         recommendations[[issue$name]])
    #   )
    # })
    issue_cards <- lapply(1:nrow(issue_summary), function(i) {
      issue <- issue_summary[i, ]
      div(style = "border-left: 4px solid #ff8c00; padding-left: 15px; margin: 10px 0;",
          h5(issue$issue_type),
          p(paste("Mentioned in", issue$count, "review(s)")),
          p(style = "font-size: 14px;", 
            strong("Recommendation: "), 
            issue$recommended_action)
      )
    })
    do.call(tagList, issue_cards)
  })
  
  # Segment Analysis
  segment_data <- reactive({
    segment_comparison(reviews)
  })
  
  output$segment_type <- renderPlot({
    data <- segment_data()$by_type
    ggplot(data, aes(x = name, y = avg)) +
      geom_col(fill = "#70AD47") +
      ylim(0, 5) +
      labs(x = "", y = "Average Rating") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10))
  })
  
  output$segment_region <- renderPlot({
    data <- segment_data()$by_region
    ggplot(data, aes(x = name, y = avg)) +
      geom_col(fill = "#4472C4") +
      ylim(0, 5) +
      labs(x = "", y = "Average Rating") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10))
  })
  
  output$segment_bedrooms <- renderPlot({
    data <- segment_data()$by_bedrooms
    ggplot(data, aes(x = name, y = avg)) +
      geom_col(fill = "#FFC000") +
      ylim(0, 5) +
      labs(x = "", y = "Average Rating") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10))
  })
  
  output$type_stats <- renderUI({
    data <- segment_data()$by_type
    tags$div(style = "font-size: 14px; margin-top: 10px;",
             lapply(1:nrow(data), function(i) {
               p(paste0(data$name[i], ": ", round(data$avg[i], 2), 
                       " avg (", data$count[i], " reviews)"))
             })
    )
  })
  
  output$region_stats <- renderUI({
    data <- segment_data()$by_region
    tags$div(style = "font-size: 14px; margin-top: 10px;",
             lapply(1:nrow(data), function(i) {
               p(paste0(data$name[i], ": ", round(data$avg[i], 2), 
                       " avg (", data$count[i], " reviews)"))
             })
    )
  })
  
  output$bedroom_stats <- renderUI({
    data <- segment_data()$by_bedrooms
    tags$div(style = "font-size: 14px; margin-top: 10px;",
             lapply(1:nrow(data), function(i) {
               p(paste0(data$name[i], ": ", round(data$avg[i], 2), 
                       " avg (", data$count[i], " reviews)"))
             })
    )
  })
  
  # Critical Issues
  critical_data <- reactive({
    reviews %>% 
      filter(!is.na(RefundRequested) | Overall <= 2)
  })
  
  output$critical_alert <- renderUI({
    critical <- critical_data()
    if (nrow(critical) == 0) return(NULL)
    
    div(class = "critical-alert",
        h4(style = "color: #856404;", 
           paste("Alert:", nrow(critical), "Critical Issue(s) Found")),
        p("Issues resulting in low ratings or refund requests require immediate attention")
    )
  })
  
  output$critical_issues <- renderUI({
    critical <- critical_data()
    if (nrow(critical) == 0) {
      return(div(class = "property-card", 
                 p("No critical issues found. Excellent job!")))
    }
    
    critical_cards <- lapply(1:nrow(critical), function(i) {
      issue <- critical[i, ]
      
      div(class = "property-card", 
          style = "border-left: 4px solid #dc3545;",
          fluidRow(
            column(8,
                   h4(issue$Listing),
                   p(paste(issue$PropertyType, "|", issue$Region))
            ),
            column(4, style = "text-align: right;",
                   div(style = "font-size: 24px; color: #dc3545; font-weight: bold;",
                       paste(issue$Overall, "★")),
                   if (!is.na(issue$RefundRequested) && issue$RefundRequested) {
                     tags$span(class = "badge badge-danger", "REFUND REQUESTED")
                   }
            )
          ),
          hr(),
          h5("Issue Description:"),
          div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px;",
              p(issue$Public.Review)
          ),
          br(),
          fluidRow(
            column(6,
                   h5("Identified Problems:"),
                   tags$ul(
                     if (issue$Cleanliness <= 2) tags$li("Severe cleanliness issues"),
                     if (!is.na(issue$Communication) && issue$Communication <= 2) 
                       tags$li("Poor communication/No response"),
                     if (str_detect(tolower(issue$Public.Review), "refund")) 
                       tags$li("Guest requested refund"),
                     if (str_detect(tolower(issue$Public.Review), "early")) 
                       tags$li("Guest left early")
                   )
            ),
            column(6,
                   h5("Action Required:"),
                   tags$ul(style = "color: #007bff;",
                           tags$li("Immediate deep cleaning and inspection"),
                           tags$li("Follow up with guest for resolution"),
                           tags$li("Review cleaning protocols"),
                           tags$li("Implement quality control checks")
                   )
            )
          )
      )
    })
    
    do.call(tagList, critical_cards)
  })
  
  output$resolution_summary <- renderUI({
    critical <- critical_data()
    unresolved <- sum(!is.na(critical$Communication) & critical$Communication <= 2)
    refund_requests <- sum(!is.na(critical$RefundRequested) & critical$RefundRequested)
    
    tagList(
      div(style = "background-color: #f8f9fa; padding: 10px; margin: 5px 0; border-radius: 4px;",
          fluidRow(
            column(9, "Total Critical Issues"),
            column(3, style = "text-align: right; font-weight: bold;", 
                   nrow(critical))
          )
      ),
      div(style = "background-color: #fff5f5; padding: 10px; margin: 5px 0; border-radius: 4px;",
          fluidRow(
            column(9, "Unresolved (No Host Response)"),
            column(3, style = "text-align: right; font-weight: bold; color: #dc3545;", 
                   unresolved)
          )
      ),
      div(style = "background-color: #fff3e0; padding: 10px; margin: 5px 0; border-radius: 4px;",
          fluidRow(
            column(9, "Refund Requests"),
            column(3, style = "text-align: right; font-weight: bold; color: #ff8c00;", 
                   refund_requests)
          )
      )
    )
  })
}

# ==============================================================================
# RUN THE APP
# ==============================================================================

shinyApp(ui = ui, server = server)

# To run with your own data:
# 1. Load your data: reviews <- read.csv("PropertyReviews.csv")
# 2. Or from JSON: reviews <- fromJSON("PropertyReviews.json")
# 3. Make sure your data has these columns:
#    - Listing, Overall, Cleanliness, Communication (optional)
#    - Public.Review, PropertyType, Region, BEDROOMS
#    - RefundRequested (optional, TRUE/FALSE)
# 4. Run: shinyApp(ui = ui, server = server)