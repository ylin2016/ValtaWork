# COMPREHENSIVE ISSUE DETECTION SYSTEM IN R
# This code shows the detailed logic for identifying common issues from reviews

library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# ==============================================================================
# STEP 1: DEFINE ISSUE CATEGORIES AND KEYWORDS
# ==============================================================================

issue_categories <- list(
  cleanliness = list(
    keywords = c('dirty', 'dust', 'dusty', 'filth', 'filthy', 'unclean',
                 'musty', 'smell', 'odor', 'stain', 'stains', 'mold', 'moldy',
                 'hair', 'hairs', 'crumb', 'crumbs', 'grime', 'grimy',
                 'not clean', 'needs cleaning', 'poorly cleaned'),
    severity = 5,
    category = 'Critical',
    recommended_action = 'Immediate deep cleaning required, review cleaning protocols'
  ),
  
  lockbox_checkin = list(
    keywords = c('lockbox', 'lock box', 'key', 'keys', 'code', 'access',
                 'check-in', 'check in', 'checkin', 'entry', 'door lock',
                 "couldn't get in", 'trouble accessing', 'locked out',
                 'wrong code', "didn't work"),
    severity = 4,
    category = 'High Priority',
    recommended_action = 'Test all lockbox codes before guest arrival, improve instructions'
  ),
  
  broken_fixtures = list(
    keywords = c('broken', 'broke', 'not working', "doesn't work", 'malfunction',
                 'toilet', 'sink', 'faucet', 'shower', 'drain', 'leak', 'leaking',
                 'fixture', 'plumbing', 'clog', 'clogged', 'stopped up'),
    severity = 4,
    category = 'High Priority',
    recommended_action = 'Pre-arrival maintenance check, establish repair response protocol'
  ),
  
  smoke_alarm = list(
    keywords = c('smoke alarm', 'smoke detector', 'fire alarm', 'carbon monoxide',
                 'co detector', 'beeping', 'chirping', 'alarm went off',
                 'false alarm', 'detector beeping'),
    severity = 5,
    category = 'Critical',
    recommended_action = 'Replace batteries monthly, test all alarms before each guest'
  ),
  temperature_ac_related = list(
    keywords = c(
      # AC related
      'air conditioning', 'air conditioner', 'a/c', 'ac unit', 'central ac',
      'no ac', 'no air conditioning', 'without ac', 'lacks ac',
      'ac not working', 'ac broken', 'ac issue' ),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Install central AC if feasible, ensure thermostats are functional, provide adequate portable units and fans'
  ),
  temperature_complaints = list(
    keywords = c(    
      # Temperature complaints
      'too hot', 'too warm', 'very hot', 'extremely hot', 'sweltering',
      'too cold', 'too cool', 'freezing', 'chilly',
      'uncomfortable temperature', 'temperature control'),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Install central AC if feasible, ensure thermostats are functional, provide adequate portable units and fans'
  ),
  temperature_heating_related = list(
    keywords = c(     
      # Heating related
      'heater', 'heating', 'heat not working', 'no heat',
      'heater broken', 'furnace'),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Install central AC if feasible, ensure thermostats are functional, provide adequate portable units and fans'
  ),
  temperature_Thermostat = list(
    keywords = c(     
      # Thermostat
      'thermostat', 'thermostat not working', 'can\'t adjust temperature',
      'thermostat locked', 'can\'t change temperature'),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Install central AC if feasible, ensure thermostats are functional, provide adequate portable units and fans'
  ),
      
  temperature_ventilation_AirQuality = list(
        keywords = c(   # Ventilation/Air quality
      'stuffy', 'no ventilation', 'poor ventilation', 'no airflow',
      'humid', 'humidity', 'muggy', 'stifling'),
      severity = 3,
      category = 'Medium Priority',
      recommended_action = 'Install central AC if feasible, ensure thermostats are functional, provide adequate portable units and fans'
      ),
      
  temperature_portable_units = list(
        keywords = c(    # Portable units
      'portable ac', 'window unit', 'space heater',
      'fan', 'fans', 'no fan', 'need fan'
    ),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Install central AC if feasible, ensure thermostats are functional, provide adequate portable units and fans'
  ),
  
  furniture_comfort = list(
    keywords = c('couch', 'sofa', 'chair', 'furniture', 'uncomfortable',
                 'no seating', 'need a couch', 'uncomfortable bed',
                 'mattress', 'pillows', 'blanket'),
    severity = 2,
    category = 'Low Priority',
    recommended_action = 'Add comfortable seating, upgrade mattresses and bedding'
  ),
  
  tv_wifi_tech = list(
    keywords = c('tv', 'television', 'smart tv', 'wifi', 'wi-fi', 'internet',
                 'streaming', 'netflix', 'remote', 'cable', 'channels',
                 'not working', "couldn't use", 'no signal'),
    severity = 2,
    category = 'Low Priority',
    recommended_action = 'Provide clear instructions, test all devices before arrival'
  ),
  
  communication_response = list(
    keywords = c('no response', "didn't respond", 'no reply', 'ignored',
                 'never answered', 'poor communication', 'unresponsive',
                 "didn't get back", 'never heard back'),
    severity = 5,
    category = 'Critical',
    recommended_action = 'Improve response time SLA, set up automated responses'
  ),
  
  parking = list(
    keywords = c('parking', 'park', 'no parking', 'difficult to park',
                 'street parking', 'parking space', 'garage', 'driveway'),
    severity = 2,
    category = 'Low Priority',
    recommended_action = 'Provide clear parking instructions, consider reserved spots'
  ),
  
  noise = list(
    keywords = c('noise', 'noisy', 'loud', 'sound', 'neighbor', 'neighbors',
                 'party', 'music', 'quiet', 'sleep', "couldn't sleep"),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Set house rules, add soundproofing, address neighbor issues'
  ),
  
  decor_ambiance = list(
    keywords = c('basic', 'plain', 'no decor', 'bland', 'stark', 'empty',
                 'no character', 'uninviting', 'not homey', 'no curtains',
                 'no pictures', 'sterile'),
    severity = 1,
    category = 'Enhancement',
    recommended_action = 'Add decorative touches, curtains, artwork, plants'
  ),
  
  privacy = list(
    keywords = c('privacy', 'private', 'blinds', 'curtains', 'window',
                 'can see in', 'exposed', 'shared', 'sharing'),
    severity = 3,
    category = 'Medium Priority',
    recommended_action = 'Install proper window coverings, review shared space policies'
  ),
  
  pests = list(
    keywords = c('ant', 'ants', 'bug', 'bugs', 'insect', 'insects', 'pest',
                 'cockroach', 'spider', 'mouse', 'mice', 'rat', 'infestation'),
    severity = 5,
    category = 'Critical',
    recommended_action = 'Immediate pest control service, preventive treatments'
  ),
  
  appliances = list(
    keywords = c('fridge', 'refrigerator', 'dishwasher', 'washer', 'dryer',
                 'stove', 'oven', 'microwave', 'not working', 'broken',
                 'not cold', "won't start"),
    severity = 4,
    category = 'High Priority',
    recommended_action = 'Test all appliances weekly, maintain service contracts'
  )
)

# ==============================================================================
# STEP 2: EXTRACT RELEVANT TEXT SNIPPET
# ==============================================================================

extract_snippet <- function(text, keyword, context_length = 100) {
  if (is.na(text) || text == "") return("")
  
  text_lower <- tolower(text)
  keyword_lower <- tolower(keyword)
  
  # Find position of keyword
  pos <- str_locate(text_lower, fixed(keyword_lower))
  
  if (is.na(pos[1])) return("")
  
  # Calculate start and end positions
  start_pos <- max(1, pos[1] - context_length)
  end_pos <- min(nchar(text), pos[2] + context_length)
  
  # Extract snippet
  snippet <- substr(text, start_pos, end_pos)
  
  # Add ellipsis
  if (start_pos > 1) snippet <- paste0("...", snippet)
  if (end_pos < nchar(text)) snippet <- paste0(snippet, "...")
  
  return(snippet)
}

# ==============================================================================
# STEP 3: DETECT ISSUES IN A SINGLE REVIEW
# ==============================================================================

detect_issues <- function(review_row, issue_categories) {
  review_text <- review_row$Public.Review
  if (is.na(review_text)) review_text <- ""
  
  review_lower <- tolower(review_text)
  detected_issues <- list()
  
  # Check each issue category
  for (issue_name in names(issue_categories)) {
    issue_data <- issue_categories[[issue_name]]
    matched_keywords <- c()
    
    # Check for keyword matches
    for (keyword in issue_data$keywords) {
      if (str_detect(review_lower, fixed(tolower(keyword)))) {
        matched_keywords <- c(matched_keywords, keyword)
      }
    }
    
    # If keywords found, record the issue
    if (length(matched_keywords) > 0) {
      detected_issues[[length(detected_issues) + 1]] <- list(
        issue_type = issue_name,
        severity = issue_data$severity,
        category = issue_data$category,
        match_count = length(matched_keywords),
        keywords = matched_keywords,
        recommended_action = issue_data$recommended_action,
        review_snippet = extract_snippet(review_text, matched_keywords[1])
      )
    }
  }
  
  # Check numeric ratings for issues
  if (!is.na(review_row$Cleanliness) && review_row$Cleanliness < 3) {
    detected_issues[[length(detected_issues) + 1]] <- list(
      issue_type = 'cleanliness',
      severity = 5,
      category = 'Critical',
      source = 'Low rating score',
      recommended_action = issue_categories$cleanliness$recommended_action
    )
  }
  
  if (!is.na(review_row$Communication) && review_row$Communication < 3) {
    detected_issues[[length(detected_issues) + 1]] <- list(
      issue_type = 'communication_response',
      severity = 5,
      category = 'Critical',
      source = 'Low rating score',
      recommended_action = issue_categories$communication_response$recommended_action
    )
  }
  
  return(detected_issues)
}

# ==============================================================================
# STEP 4: AGGREGATE ISSUES ACROSS ALL REVIEWS
# ==============================================================================

aggregate_issues <- function(reviews_df, issue_categories) {
  all_issues <- list()
  issue_stats <- list()
  issues_by_property <- list()
  critical_issues <- list()
  
  # Process each review
  for (i in 1:nrow(reviews_df)) {
    review <- reviews_df[i, ]
    issues <- detect_issues(review, issue_categories)
    property_name <- review$Listing
    
    # Initialize property tracking
    if (is.null(issues_by_property[[property_name]])) {
      issues_by_property[[property_name]] <- list()
    }
    
    # Process each detected issue
    for (issue in issues) {
      issue_type <- issue$issue_type
      
      # Aggregate overall stats
      if (is.null(issue_stats[[issue_type]])) {
        issue_stats[[issue_type]] <- list(
          count = 0,
          severity = issue$severity,
          category = issue$category,
          properties = c(),
          examples = list(),
          recommended_action = issue$recommended_action
        )
      }
      
      issue_stats[[issue_type]]$count <- issue_stats[[issue_type]]$count + 1
      issue_stats[[issue_type]]$properties <- 
        unique(c(issue_stats[[issue_type]]$properties, property_name))
      
      # Add example if snippet available
      if (!is.null(issue$review_snippet) && issue$review_snippet != "") {
        issue_stats[[issue_type]]$examples[[length(issue_stats[[issue_type]]$examples) + 1]] <- 
          list(
            property = property_name,
            snippet = issue$review_snippet,
            rating = review$Overall
          )
      }
      
      # Track by property
      if (is.null(issues_by_property[[property_name]][[issue_type]])) {
        issues_by_property[[property_name]][[issue_type]] <- 0
      }
      issues_by_property[[property_name]][[issue_type]] <- 
        issues_by_property[[property_name]][[issue_type]] + 1
      
      # Flag critical issues
      if (issue$severity >= 4) {
        critical_issues[[length(critical_issues) + 1]] <- list(
          property = property_name,
          issue = issue,
          review = review,
          review_index = i
        )
      }
    }
  }
  
  return(list(
    issue_stats = issue_stats,
    issues_by_property = issues_by_property,
    critical_issues = critical_issues
  ))
}

# ==============================================================================
# STEP 5: GENERATE DETAILED REPORT
# ==============================================================================

generate_issue_report <- function(reviews_df, issue_categories) {
  results <- aggregate_issues(reviews_df, issue_categories)
  
  cat('\n═══════════════════════════════════════════════════════\n')
  cat('           PROPERTY REVIEW ISSUE ANALYSIS              \n')
  cat('═══════════════════════════════════════════════════════\n\n')
  
  # Sort issues by severity and count
  issue_names <- names(results$issue_stats)
  if (length(issue_names) > 0) {
    sorted_indices <- order(
      sapply(results$issue_stats, function(x) x$severity),
      sapply(results$issue_stats, function(x) x$count),
      decreasing = TRUE
    )
    sorted_issues <- issue_names[sorted_indices]
    
    cat('ISSUE SUMMARY (sorted by severity and frequency):\n\n')
    
    for (issue_type in sorted_issues) {
      data <- results$issue_stats[[issue_type]]
      
      cat(sprintf('\n┌─ %s\n', toupper(gsub('_', ' ', issue_type))))
      cat(sprintf('│  Category: %s\n', data$category))
      cat(sprintf('│  Severity: %s (%d/5)\n', 
                  paste(rep('★', data$severity), collapse=''), data$severity))
      cat(sprintf('│  Occurrences: %d\n', data$count))
      cat(sprintf('│  Properties affected: %d\n', length(data$properties)))
      cat(sprintf('│  Recommended Action: %s\n', data$recommended_action))
      
      if (length(data$examples) > 0) {
        cat('│\n│  Example mentions:\n')
        for (i in 1:min(2, length(data$examples))) {
          ex <- data$examples[[i]]
          cat(sprintf('│  %d. [%s] (%.1f★): "%s"\n', 
                     i, ex$property, ex$rating, ex$snippet))
        }
      }
      cat('└─────────────────────────────────────────────────────\n')
    }
  }
  
  # Critical Issues Section
  cat('\n\n═══════════════════════════════════════════════════════\n')
  cat('           CRITICAL ISSUES REQUIRING IMMEDIATE ACTION  \n')
  cat('═══════════════════════════════════════════════════════\n\n')
  
  if (length(results$critical_issues) > 0) {
    for (i in 1:length(results$critical_issues)) {
      critical <- results$critical_issues[[i]]
      
      cat(sprintf('\n%d. %s\n', i, critical$property))
      cat(sprintf('   Issue: %s\n', gsub('_', ' ', critical$issue$issue_type)))
      cat(sprintf('   Severity: %s\n', 
                  paste(rep('★', critical$issue$severity), collapse='')))
      cat(sprintf('   Guest Rating: %.1f/5\n', critical$review$Overall))
      
      if (!is.null(critical$issue$review_snippet) && 
          critical$issue$review_snippet != "") {
        cat(sprintf('   Context: "%s"\n', critical$issue$review_snippet))
      }
      cat(sprintf('   Action: %s\n', critical$issue$recommended_action))
    }
  } else {
    cat('\nNo critical issues found.\n')
  }
  
  # Issues by Property Section
  cat('\n\n═══════════════════════════════════════════════════════\n')
  cat('           ISSUES BY PROPERTY                          \n')
  cat('═══════════════════════════════════════════════════════\n\n')
  
  for (property in names(results$issues_by_property)) {
    issues <- results$issues_by_property[[property]]
    
    cat(sprintf('\n%s:\n', property))
    
    # Sort by count
    issue_names <- names(issues)
    counts <- unlist(issues)
    sorted_idx <- order(counts, decreasing = TRUE)
    
    for (idx in sorted_idx) {
      issue_name <- issue_names[idx]
      count <- counts[idx]
      severity <- issue_categories[[issue_name]]$severity
      
      cat(sprintf('  • %s: %d mention(s) [%s]\n', 
                  gsub('_', ' ', issue_name), 
                  count,
                  paste(rep('★', severity), collapse='')))
    }
  }
  
  # Summary statistics
  total_mentions <- sum(sapply(results$issue_stats, function(x) x$count))
  
  cat('\n\nSUMMARY STATISTICS:\n')
  cat(sprintf('Total unique issues identified: %d\n', length(results$issue_stats)))
  cat(sprintf('Total issue mentions: %d\n', total_mentions))
  cat(sprintf('Critical issues: %d\n', length(results$critical_issues)))
  cat(sprintf('Properties with issues: %d\n', length(results$issues_by_property)))
  
  return(invisible(results))
}

# ==============================================================================
# STEP 6: CREATE SUMMARY DATA FRAME FOR ANALYSIS
# ==============================================================================

create_issue_summary_df <- function(reviews_df, issue_categories) {
  results <- aggregate_issues(reviews_df, issue_categories)
  
  # Convert to data frame
  issue_df <- data.frame(
    issue_type = character(),
    count = numeric(),
    severity = numeric(),
    category = character(),
    num_properties = numeric(),
    recommended_action = character(),
    stringsAsFactors = FALSE
  )
  
  for (issue_name in names(results$issue_stats)) {
    data <- results$issue_stats[[issue_name]]
    
    issue_df <- rbind(issue_df, data.frame(
      issue_type = gsub('_', ' ', issue_name),
      count = data$count,
      severity = data$severity,
      category = data$category,
      num_properties = length(data$properties),
      recommended_action = data$recommended_action,
      stringsAsFactors = FALSE
    ))
  }
  
  return(issue_df %>% arrange(desc(severity), desc(count)))
}