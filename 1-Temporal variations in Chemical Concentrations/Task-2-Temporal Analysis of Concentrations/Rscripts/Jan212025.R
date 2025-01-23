
library(dplyr)
library(tidyr)
library(DescTools)

process_tissue_analysis <- function(data, remove_zeroes = FALSE) {
  # Determine the range of columns to process
  if (stringr::str_detect(colnames(data)[16], 'Ag')) {
    metals <- colnames(data)[16:25]
  } else {
    metals <- colnames(data)[16:19]
  }
  
  # Prepare the long-format data
  long_data <- data %>%
    select(c(16:ncol(data), Tissue = 6, Year = 3, ID_num = 4)) %>%
    pivot_longer(cols = all_of(metals), names_to = "Metal", values_to = "Concentration") %>%
    mutate(
      Concentration = as.numeric(Concentration),
      Year = factor(Year),
      Tissue = factor(Tissue)
    )
  
  if (remove_zeroes) {
    long_data <- long_data %>% filter(Concentration != 0)
  }
  
  # Initialize results containers
  shapiro_results <- data.frame()
  summary_statistics <- list()
  kruskal_wallis_results <- list()
  anova_results <- list()
  
  for (metal in metals) {
    metal_data <- long_data %>% filter(Metal == metal)
    tissues <- unique(metal_data$Tissue)
    years <- unique(metal_data$Year)
    
    for (tissue in tissues) {
      for (year in years) {
        
        print(year)
        print (tissue)
        
        subset_data <- metal_data %>%
          filter(Tissue == tissue, Year == year)
        
        if (nrow(subset_data) < 3| all(subset_data$Concentration == subset_data$Concentration[1])) {
          next
        }
        
        # Perform Shapiro-Wilk test
        shapiro_test <- shapiro.test(subset_data$Concentration)
        test_result <- ifelse(shapiro_test$p.value > 0.05, "Pass", "Fail")
        
        # Store Shapiro-Wilk results
        shapiro_results <- rbind(
          shapiro_results,
          data.frame(
            Metal = metal,
            Tissue = tissue,
            Year = year,
            ShapiroPValue = shapiro_test$p.value,
            TestResult = test_result,
            FurtherTest = ifelse(test_result == "Fail", "Kruskal-Wallis", "ANOVA")
          )
        )
        
        # Perform Kruskal-Wallis or ANOVA
        # if (test_result == "Fail") {
        #   kw_test <- kruskal.test(Concentration ~ Year, data = subset_data)
        #   kruskal_wallis_results[[paste(metal, tissue, year, sep = "_")]] <- kw_test
        # } else {
        #   aov_test <- aov(Concentration ~ Year, data = subset_data)
        #   anova_results[[paste(metal, tissue, year, sep = "_")]] <- aov_test
        # }
      }
    }
    
    # Calculate summary statistics
    summary_stats <- metal_data %>%
      group_by(Tissue, Year) %>%
      summarise(
        N = n(),
        Mean = mean(Concentration, na.rm = TRUE),
        Median = median(Concentration, na.rm = TRUE),
        Max = max(Concentration, na.rm = TRUE),
        Min = min(Concentration, na.rm = TRUE),
        SD = sd(Concentration, na.rm = TRUE),
        SE = SD / sqrt(N)
      )
    summary_statistics[[metal]] <- summary_stats
  }
  
  # Return results
  list(
    ShapiroResults = shapiro_results,
    SummaryStatistics = summary_statistics,
    KruskalWallisResults = kruskal_wallis_results,
    ANOVAResults = anova_results,
    LongData = long_data
  )
}

process_tissue_analysis(processed_hm_data)
