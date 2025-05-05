library(shiny)
library(tidyverse)
library(keyring)
library(httr2)
library(jsonlite)
library(plotly)
library(DT)

# ONE THING YOU HAVE TO DO TO MAKE THIS RUN - get an FEC api key and save it by un-noting this out and adding it
# you can sign up for a key here - https://api.open.fec.gov/developers/
# key_set("fec_api_key")

################## FUNCTION 1 ################

fetch_candidate_ids <- function(state_abbreviations) {
  # Set the base URL for the FEC API
  base_url_for_candidate_name <- "https://api.open.fec.gov/v1/candidates/totals/"
  
  # Initialize an empty list to store results
  committee_id_data <- list()
  
  # Iterate through each state in the list of state abbreviations
  for (each_state in state_abbreviations) { # Define query parameters for the current state
    params_states_in_question <- list(
      election_year = 2026,
      state = each_state,
      per_page = 100,
      office = "S", # Senate candidates
      election_full = TRUE,
      is_active_candidate = TRUE,
      api_key = keyring::key_get("fec_api_key")
    )
    
    # Try to fetch data for the current state
    tryCatch(
      {
        fundraising <- (request(base_url_for_candidate_name) |>
                          req_url_query(!!!params_states_in_question) |>
                          req_perform() |>
                          resp_body_json(resp = _))$results |>
          tibble(data = _) |>
          unnest_wider(data)
        
        # Append to the results list using the state abbreviation as a key
        committee_id_data[[each_state]] <- fundraising
      },
      error = function(e) {
        message(paste("Error fetching data for state:", each_state))
      }
    )
  }
  
  # Combine all data into a single dataframe
  candidates_per_state <- bind_rows(committee_id_data, .id = "state")
  
  # Generate a list of candidate IDs
  list_candidates <- candidates_per_state |>
    select(name, candidate_id, receipts, other_political_committee_contributions, individual_itemized_contributions, party)
  
  return(list_candidates) ## Corrected: Moved this line to ensure saving and loading happens first
}

########## END OF FUNCTION ############


################## FUNCTION 2 ################

fetch_candidate_fundraising <- function(list_candidates) {
  # Set the base URL for the FEC API
  base_url_for_candidate_fundraising <- "https://api.open.fec.gov/v1/schedules/schedule_a/by_state/by_candidate/"
  # Initialize an empty list to store results
  all_data <- list()
  # Iterate through each candidate ID in the dataframe
  for (candidate_id in list_candidates$candidate_id) { # Use the candidate_id column from list_candidates
    # Define query parameters for the current candidate
    params_candidate_fundraising <- list(
      per_page = 100,
      candidate_id = candidate_id,
      cycle = 2026,
      election_full = TRUE,
      sort = "state",
      api_key = keyring::key_get("fec_api_key") # Securely retrieve API key
    )
    # Try to fetch data for the current candidate
    tryCatch(
      {
        state_locations <- (request(base_url_for_candidate_fundraising) |>
                              req_url_query(!!!params_candidate_fundraising) |>
                              req_perform() |>
                              resp_body_json(resp = _))$results |>
          tibble(data = _) |>
          unnest_wider(data)
        # Add a column identifying the candidate
        state_locations$candidate_id <- candidate_id
        # Append to the results list
        all_data[[candidate_id]] <- state_locations
      },
      error = function(e) {
        message(paste("Error fetching data for candidate:", candidate_id))
      }
    )
  }
  # Combine all data into a single dataframe
  final_data <- bind_rows(all_data)
  # make sure the candidate name, party, etc is associated with each line
  final_data <- list_candidates |>
    select(name, candidate_id, party) |>
    right_join(final_data, by = "candidate_id")
  return(final_data)
}

########## END OF FUNCTION ############


########## LIST OF STATE ABBREVIATIONS  ############

# state_abbreviations - Creates the list of state names for use in the query - this can eventually likely get migrated to a separate file to improve readability, but for now it's going to be here.
state_abbreviations <- c(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arkansas" = "AR",
  "Colorado" = "CO",
  "Delaware" = "DE",
  "Georgia" = "GA",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Mississippi" = "MS",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "North Carolina" = "NC",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Virginia" = "VA",
  "West Virginia" = "WV",
  "Wyoming" = "WY"
)

########## END OF LIST  ############


########## BEGIN SHINY UI  ############

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly", primary = "#0055A4", base_font = "Georgia"),
  titlePanel("2026 Senate Race Donations"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "state_select",
        label = "Choose State(s):",
        choices = state_abbreviations,
        selected = "AL"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Candidate Summary",  br(), DTOutput("candidate_id_table")),
        tabPanel("Fundraising Summary", br(), DTOutput("candidate_fundraising_table")),
        tabPanel("Bar Plot",  br(), plotlyOutput("itemized_plot", height = "100%")),
        tabPanel("Donation Map",  br(), plotlyOutput("fundraising_map", height = "100%")),
        tabPanel("Summary Stats", 
                 br(), 
                 verbatimTextOutput("summary_stats"), 
                 br(), 
                 verbatimTextOutput("t_test_explanation") # New output for explanation
        ),
        tabPanel("Regression Analysis", 
                 br(), 
                 verbatimTextOutput("model_summary"),   # Model summary output
                 br(),
                 plotOutput("regression_plot")          # Regression plot output
        )
      )
    )
  )
)

########## END SHINY UI  ############

########## BEGIN SHINY SERVER SECTION  ############

server <- function(input, output, session) {
  
  list_candidates <- reactive({
    req(input$state_select)
    fetch_candidate_ids(input$state_select)
  })
  
  output$candidate_id_table <- renderDT({
    req(list_candidates())
    datatable(list_candidates())
  })
  
  candidate_fundraising_df <- reactive({
    req(list_candidates())
    fetch_candidate_fundraising(list_candidates())
  })
  
  output$candidate_fundraising_table <- renderDT({
    req(candidate_fundraising_df())
    datatable(candidate_fundraising_df())
  })
  
  # Plot itemized vs PACs
  output$itemized_plot <- renderPlotly({
    df <- list_candidates()
    req(nrow(df) > 0)
    
    df_long <- df |>
      filter(receipts > 1) |>
      mutate(receipts = (receipts - (individual_itemized_contributions + other_political_committee_contributions))) |>
      pivot_longer(
        cols = c(receipts, individual_itemized_contributions, other_political_committee_contributions),
        names_to = "Type", values_to = "Amount"
      ) |>
      mutate(
        Type = recode(Type,
                      receipts = "Small-dollar donations, transfers, other donations",
                      individual_itemized_contributions = "Donations over $200",
                      other_political_committee_contributions = "PACs"
        ),
        name_party = paste0(name, " (", party, ")")
      )
    
    plot_ly(df_long, x = ~name_party, y = ~Amount, color = ~Type, type = "bar") |>
      layout(
        title = "Contributions to candidates by type",
        barmode = "stack",
        yaxis = list(
          title = "Amount",
          tickprefix = "$",
          tickformat = ",.0f" # comma thousands, no decimals
        ),
        xaxis = list(
          title = "Candidate (Party)"
        ),
        legend = list(
          orientation = "h", # horizontal
          xanchor = "center", # anchor legend to the center
          x = 0.5, # center of the plot
          y = -0.2 # below the plot
        )
      )
  })
  
  # Map
  output$fundraising_map <- renderPlotly({
    df <- candidate_fundraising_df()
    req(nrow(df) > 0)
    
    df_map <- df |>
      group_by(state) |>
      summarise(total = sum(total, na.rm = TRUE))
    
    plot_geo(df_map, locationmode = "USA-states") |>
      add_trace(
        z = ~total,
        locations = ~state,
        color = ~total,
        colors = "Blues",
        text = ~ paste(state, "<br>$", round(total, 2)),
        hoverinfo = "text"
      ) |>
      layout(geo = list(scope = "usa"), title = "Donation Origins by State for All Candidates")
  })
  
  output$model_summary <- renderPrint({
    df <- candidate_fundraising_df()
    req(nrow(df) > 0)
    
    # Create the home_state variable (in_state or out_of_state)
    df <- df |> mutate(candidate_state = substr(candidate_id, 3, 4))  # extract state code from candidate_id
    df <- df |> mutate(home_state = ifelse(state == candidate_state, "in_state", "out_of_state"))
    
    # Ensure 'home_state' and 'party' are factors for regression
    df <- df |> mutate(
      home_state = factor(home_state, levels = c("in_state", "out_of_state")),
      party = factor(party)
    )
    
    # Fit the linear regression model
    model <- lm(total ~ home_state + party, data = df)
    
    # Display the summary of the model (coefficients, p-values, etc.)
    summary(model)
  })
  
  output$regression_plot <- renderPlot({
    df <- candidate_fundraising_df()
    req(nrow(df) > 0)
    
    # Create the home_state variable (in_state or out_of_state)
    df <- df |> mutate(candidate_state = substr(candidate_id, 3, 4))  # extract state code from candidate_id
    df <- df |> mutate(home_state = ifelse(state == candidate_state, "in_state", "out_of_state"))
    
    # Ensure 'home_state' and 'party' are factors for regression
    df <- df |> mutate(
      home_state = factor(home_state, levels = c("in_state", "out_of_state")),
      party = factor(party)
    )
    
    # Plot donations vs. home_state with colors by party
    ggplot(df, aes(x = home_state, y = total, color = party)) +
      geom_boxplot() +
      labs(title = "Donation Distribution by Home State and Party",
           x = "Home State (In-State vs. Out-of-State)",
           y = "Total Donations") +
      theme_minimal()
  })
  
  
  output$summary_stats <- renderPrint({
    df <- candidate_fundraising_df()
    req(nrow(df) > 0)
    
    # Classify donations as in-state vs out-of-state
    df <- df |> mutate(home_state = ifelse(state == substr(candidate_id, 3, 4), "in_state", "out_of_state"))
    
    # Calculate average contributions per group
    avg_contribs <- df |>
      group_by(home_state) |>
      summarise(
        avg_total = mean(total, na.rm = TRUE),
        median_total = median(total, na.rm = TRUE),
        sd_total = sd(total, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Perform Welch's t-test
    t_test_result <- t.test(total ~ home_state, data = df)
    
    # Output the average contribution statistics and the t-test results together
    cat("Average Contributions by Home State:\n")
    print(avg_contribs)
    cat("\nWelch's T-Test Result:\n")
    print(t_test_result)
  })
  
  
  output$t_test_explanation <- renderText({
    df <- candidate_fundraising_df()
    req(nrow(df) > 0)
    
    # Ensure home_state is classified correctly
    df <- df |> mutate(candidate_state = substr(candidate_id, 3, 4))
    df <- df |> mutate(home_state = ifelse(state == candidate_state, "in_state", "out_of_state"))
    
    # Perform Welch's t-test
    t_test_result <- t.test(total ~ home_state, data = df)
    
    # Explanation text based on p-value
    p_value <- t_test_result$p.value
    if (p_value < 0.05) {
      result_text <- "The p-value is less than 0.05, so we reject the null hypothesis. This means that there is a statistically significant difference between in-state and out-of-state donations. In other words, the location of the donor (whether they are from the candidate's home state or not) has an effect on the total donation amount."
    } else {
      result_text <- "The p-value is greater than 0.05, so we fail to reject the null hypothesis. This means that there is no statistically significant difference between in-state and out-of-state donations. The location of the donor does not appear to have a major effect on the total donation amount."
    }
    
    # Add more context and interpretation
    interpretation <- paste(
      result_text,
      "\n\nThe t-test compares the average donation amounts between two groups: in-state donors and out-of-state donors. The null hypothesis assumes that there is no difference between these groups. The t-statistic is a measure of how much the means of the two groups differ, while the p-value tells us whether this difference is statistically significant.",
      "\n\nIf the p-value is low (typically below 0.05), it suggests that the observed difference is unlikely to have occurred by chance, and we conclude that the difference is statistically significant."
    )
    
    interpretation
  })
  
}

########## END SHINY SERVER SECTION  ############

shinyApp(ui, server)
