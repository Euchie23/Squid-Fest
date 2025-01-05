library(dplyr)
library(tidyr)
library(DescTools)

temporal_analysis_part1 <- function(data, remove_zeroes = FALSE) {
  # Determine the range of columns to process
  if (grepl("Fe|Ag", colnames(data)[16])) {
    pollutants <- colnames(data)[16:25]
  } else {
    pollutants <- colnames(data)[16:19]
  }
  
  # Prepare the long-format data
  long_data <- data %>%
    select(c(16:ncol(data), Tissue = 6, Year = 3, ID_num = 4)) %>%
    pivot_longer(cols = all_of(pollutants), names_to = "Pollutant", values_to = "Concentration") %>%
    mutate(
      Concentration = as.numeric(Concentration),
      Year = factor(Year),
      Tissue = factor(Tissue)
    )
  
  if (remove_zeroes == TRUE) {
    long_data <- long_data %>% filter(Concentration != 0)
  }
  
  # Initialize results containers
  shapiro_results <- data.frame()
  summary_statistics <- list()
  
  for (pollutant in pollutants) {
    pollutant_data <- long_data %>% filter(Pollutant == pollutant)
    tissues <- unique(pollutant_data$Tissue)
    years <- unique(pollutant_data$Year)
    
    for (tissue in tissues) {
      for (year in years) {
        subset_data <- pollutant_data %>%
          filter(Tissue == tissue, Year == year)
        
        
        if (nrow(subset_data) < 3 | all(subset_data$Concentration[-1] == subset_data$Concentration[1])) {
          shapiro_results <- rbind(
            shapiro_results,
            data.frame(
              Pollutant = pollutant,
              Tissue = tissue,
              Year = year,
              ShapiroPValue = "<3/No observations",
              TestResult = "<3/No observations"
            )
          )
          next
        }
        
        if (nrow(subset_data) > 3 | all(subset_data$Concentration[-1] != subset_data$Concentration[1])){
          # Perform Shapiro-Wilk test
          shapiro_test <- shapiro.test(subset_data$Concentration)
          test_result <- ifelse(shapiro_test$p.value > 0.05, "Pass", "Fail")
          # Store Shapiro-Wilk results
          shapiro_results <- rbind(
            shapiro_results,
            data.frame(
              Pollutant = pollutant,
              Tissue = tissue,
              Year = year,
              ShapiroPValue = shapiro_test$p.value,
              TestResult = test_result
            )
          )
        }
      }
    }
    
            # Define all tissues and years you want in the output
            all_tissues <- c("liver", "muscle", "inksac", "stomach")
            all_years <- c(2019, 2020, 2021)
            
            
            # Generate a complete set of combinations
            all_combinations <- expand.grid(
              Pollutant = unique(pollutant_data$Pollutant),
              Tissue = all_tissues,
              Year = all_years
            )
            
            # Summarize with explicit handling for zero observations
            summary_stats <- pollutant_data %>%
              group_by(Pollutant, Tissue, Year) %>%
              summarize(
                N = n(),
                Mean = ifelse(N > 0, mean(Concentration, na.rm = TRUE), 0),
                Median = ifelse(N > 0, median(Concentration, na.rm = TRUE), 0),
                Max = ifelse(N > 0, max(Concentration, na.rm = TRUE), 0),
                Min = ifelse(N > 0, min(Concentration, na.rm = TRUE), 0),
                SD = ifelse(N > 0, sd(Concentration, na.rm = TRUE), 0),
                SE = ifelse(N > 0, SD / sqrt(N), 0),
              .groups = "drop"
            ) 
            
            # Ensure 'Year' is the same type in both datasets
            all_combinations <- all_combinations %>%
              mutate(Year = as.character(Year))
            
            summary_stats <- summary_stats %>%
              mutate(Year = as.character(Year))
            
            # Join the summary back to the complete set of combinations
            final_stats <- all_combinations %>%
              left_join(summary_stats, by = c("Pollutant", "Tissue", "Year")) %>%
              replace_na(list(
                N = 0, Mean = 0, Median = 0, Max = 0, Min = 0, SD = 0, SE = 0
              ))
          
            #print(final_stats)
          summary_statistics[[pollutant]] <- final_stats

      }
  # Return results
  list(
    ShapiroResults = shapiro_results,
    SummaryStatistics = summary_statistics,
    LongData = long_data
  )
}

temporal_analysis_part <-temporal_analysis_part1(processed_sm_data)
