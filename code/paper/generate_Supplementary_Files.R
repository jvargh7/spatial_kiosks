library(officer)
library(ggplot2)
library(here)

# Variables
STAGES      <- c("stage1", "stage2")
OUTCOMES    <- c("prevalence", "awareness-conditional", "controlled", "awareness-marginal")
YEAR_RANGES <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024")

source(here("R/figures.R"))

# Supplementary File 2: State and County Estimates ------------------------
message("Pursuant Maps of Estimates")
plots <- vector(mode = "list", length = length(STAGES) * length(OUTCOMES) * 2)
i <- 1
pdf(file = here("results", "figures", "supplement", "Supplementary File 2.pdf"), width = 11, height = 8.5)
for(stage in STAGES){
  for(outcome in OUTCOMES){
    message(paste(stage, outcome))
    name <- paste0(YEAR_RANGES, "_", outcome, "_", stage, "_national.csv")
    filepaths <- here("results", "estimates", name)
    dt <- rbindlist(lapply(filepaths, function(filepath) fread(filepath)))
    dt[, mean := ifelse(mean <1, mean*100, mean)]
    dt[, se := (upper - mean) / 1.96]
    
    MID <- median(dt$mean) |> ceiling()
    MID.se <- median(dt$se) 
    
    name <- paste0(YEAR_RANGES, "_", outcome, "_", stage, "_county.csv")
    filepaths <- here("results", "estimates", name)
    dt <- rbindlist(lapply(filepaths, function(filepath) fread(filepath)))
    dt[, mean := ifelse(mean <1, mean*100, mean)]
    dt[, se := (upper - mean) / 1.96]
    
    RAD <- abs(dt$mean - MID) |> max() |> ceiling()
    RAD.se <- abs(dt$se - MID.se) |> max() 
    
    title <- paste(stage, outcome)
    plt.county <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MID - RAD, mid = MID, max = MID + RAD, level = "county", var = "mean") +
      ggtitle(paste(title, "county"))
    plt.state <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MID - RAD, mid = MID, max = MID + RAD, level = "state", var = "mean") +
      ggtitle(paste(title, "state"))
      
    plots[[i]] <- plt.county
    i <- i+1
    plots[[i]] <- plt.state
    i <- i+1
    
    print(plt.state)
    print(plt.county)
  }
}
dev.off()

# Initialize a Word document
doc <- read_docx()

# Define landscape section properties for full-page orientation
landscape_section <- prop_section(
  type = "continuous",
  page_size = page_size(orient = "landscape")
)

# Add each plot to the document, one plot per page
for (i in seq_along(plots)) {
  # Save the plot temporarily as an image
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plots[[i]], width = 9, height = 6, dpi = 125) # Fit to landscape page
  
  # Add the plot to the Word document
  doc <- doc %>%
    body_add_img(src = temp_file, width = 9, height = 6) 
  
  # Delete the temporary file
  unlink(temp_file)
}

# Apply landscape orientation to the entire document
doc <- body_set_default_section(doc, landscape_section)

# Save the Word document
print(doc, target = "Supplementary File 2.docx")

# Supplementary File 3: Standard Errors -----------------------------------
pdf(file = here("results", "figures", "supplement", "Supplementary File 3.pdf"), width = 11, height = 8.5)
plots.se <- vector(mode = "list", length = length(STAGES) * length(OUTCOMES) * 2)
i <- 1
for(stage in STAGES){
  for(outcome in OUTCOMES){
    message(paste(stage, outcome))
    name <- paste0(YEAR_RANGES, "_", outcome, "_", stage, "_state.csv")
    filepaths <- here("results", "estimates", name)
    dt.state <- rbindlist(lapply(filepaths, function(filepath) fread(filepath)))
    dt.state[, mean := ifelse(mean <1, mean*100, mean)]
    dt.state[, lower := ifelse(lower <1, lower*100, lower)]
    dt.state[, upper := ifelse(upper <1, upper*100,upper)]
    dt.state[, se := (upper - mean) / 1.96]
    
    name <- paste0(YEAR_RANGES, "_", outcome, "_", stage, "_county.csv")
    filepaths <- here("results", "estimates", name)
    dt.county <- rbindlist(lapply(filepaths, function(filepath) fread(filepath)))
    dt.county[, mean := ifelse(mean <1, mean*100, mean)]
    dt.county[, lower := ifelse(lower <1, lower*100, lower)]
    dt.county[, upper := ifelse(upper <1, upper*100,upper)]
    dt.county[, se := (upper - mean) / 1.96]
    
    MIN.se <- min(dt.state$se, dt.county$se) |> floor()
    MAX.se <- max(dt.state$se, dt.county$se) |> ceiling()
    
    title <- paste(stage, outcome)
    plt.county <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MIN.se, mid = MID.se, max = MAX.se, level = "county", var = "se") +
      ggtitle(paste(title, "county"))
    plt.state <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MIN.se, mid = MID.se, max = MAX.se, level = "state", var = "se") +
      ggtitle(paste(title, "state"))
      
    plots.se[[i]] <- plt.county
    i <- i+1
    plots.se[[i]] <- plt.state
    i <- i+1
    
    print(plt.state)
    print(plt.county)
  }
}
dev.off()

# Initialize a Word document
doc <- read_docx()

# Define landscape section properties for full-page orientation
landscape_section <- prop_section(
  type = "continuous",
  page_size = page_size(orient = "landscape")
)

# Add each plot to the document, one plot per page
for (i in seq_along(plots.se)) {
  # Save the plot temporarily as an image
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plots.se[[i]], width = 9, height = 6, dpi = 125) # Fit to landscape page
  
  # Add the plot to the Word document
  doc <- doc %>%
    body_add_img(src = temp_file, width = 9, height = 6) 
  
  # Delete the temporary file
  unlink(temp_file)
}

# Apply landscape orientation to the entire document
doc <- body_set_default_section(doc, landscape_section)

# Save the Word document
print(doc, target = "Supplementary File 3.docx")

# Supplementary File 4: Pursuant National Estimate Barplots ----------------------------------------------------------------
message("Pursuant Barplots by Gender")
pdf(file = "results/figures/supplement/Supplementary File 4 - Pursuant National Estimate Barplots by Age, Sex, and Race Ethnicity.pdf", 
    width = 11, height = 8.5)
for(demo_var in c("age_group", "ethnicity")){
  for(stage in STAGES){
    for(outcome in OUTCOMES){
        plt <- plot_barplot_by_gender(stage = stage, outcome = outcome, demo_var = demo_var, year_range = "All")
        print(plt)
    }
  }
}
dev.off()

# Supplementary File 5: State Unadjust vs. Model-Based Diagnostics ------------------------------
message("Pursuant State Estimates - Direct vs. Model-based")
pdf(file = "results/figures/supplement/Supplementary File 5 - Pursuant Direct vs. Model-Based Estimates at State and National Level.pdf", 
    width = 11, height = 8.5)
for(stage in STAGES){
  for(outcome in OUTCOMES){
     for(year_range in YEAR_RANGES){
       plt <- plot_pursuant_unadjust_vs_adjust_state(stage = stage, outcome = outcome, year_range_arg = year_range)
       print(plt)
     }
  }
}
dev.off()