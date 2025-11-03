# Emerging_Methods_GRIL

#### Part 1 -- Shiny App Questionnaire 


<img width="1736" height="1156" alt="Image" src="https://github.com/user-attachments/assets/b61272dc-a578-4263-b479-80e9742652f0" />

```
#By Jack A. Greenhalgh, Sept 2025.
#Department of Biology, McGill University, 1205 Dr Penfield Ave, Montreal, Quebec, H3A 1B1, Canada

# ===================================================================
# R Shiny Survey for Freshwater Invasive Species Detection Methods
# V4 - With Fully Labeled Scales and Comprehensive Data Capture
# ===================================================================

# -- Step 1: Install and load necessary packages --
library(shiny)
library(sortable)
library(shinyjs)

# -- Step 2: Define the questions and choices --
taxa_list <- c(
  "Bryozoan", "Crustacean - small", "Crustacean - large", "Cnidarian", 
  "Mollusk - Gastropod", "Mollusk - Bivalves", "Fish", "Reptiles", 
  "Aquatic Plants", "Wetland Plants", "Amphibians", "Mammals", 
  "Viruses", "Parasites", "Aquatic insects"
)
method_list <- c("Remote sensing", "Drones", "ROVs", "Visual survey", "eDNA", "Acoustics")

# Trait and attribute lists for weighting questions
taxon_traits <- c("Cryptic appearance", "Acoustic signature", "Mobility", "Body size", "Number of broods per year")
method_attributes <- c("Sensitivity", "Developmental stage ID", "Spatial scalability", "Ease of analysis", "Temporal scalability")


# ===================================================================
# UI (User Interface) - The front-end of the survey
# ===================================================================
ui <- fluidPage(
  useShinyjs(), 
  titlePanel("Survey: Freshwater Invasive Species Detection Methods"),
  
  p("Please answer the following questions based on your expert opinion. Your responses will help build a model to select the best detection methods for various invasive species."),
  textInput("expert_name", "Please enter your name or a unique identifier:", placeholder = "e.g., Expert A"),
  hr(),
  
  tabsetPanel(
    id = "survey_tabs",
    
    # ----------- PART 1: SPECIES TRAITS (UPDATED WITH FULL LABELS) -----------
    tabPanel("Part 1: Species Trait Assessment",
             h3("Instructions:"),
             p("For each species group below, please select the category that best describes its biological traits."),
             
             lapply(taxa_list, function(taxon) {
               fluidRow(
                 column(12, h4(paste("Assessing Taxon:", taxon))),
                 
                 # UPDATED: All choices now have descriptive labels
                 column(12, radioButtons(paste0("q4_body_size_", gsub(" - ", "_", gsub(" ", "_", taxon))), 
                                         "4. Adult Body Size:", inline = TRUE,
                                         choices = c("0 (<1mm)"=0, "1 (1mm-1cm)"=1, "2 (1-10cm)"=2, "3 (10-50cm)"=3, "4 (50cm-2m)"=4, "5 (>2m)"=5))),
                 column(12, radioButtons(paste0("q1_cryptic_", gsub(" - ", "_", gsub(" ", "_", taxon))), 
                                         "1. Cryptic Appearance:", inline = TRUE,
                                         choices = c("0 (Highly conspicuous)"=0, "1 (Mostly visible)"=1, "2 (Slightly cryptic)"=2, "3 (Moderately cryptic)"=3, "4 (Highly cryptic)"=4, "5 (Extremely cryptic/hidden)"=5))),
                 column(12, radioButtons(paste0("q3_mobility_", gsub(" - ", "_", gsub(" ", "_", taxon))), 
                                         "3. Mobility:", inline = TRUE,
                                         choices = c("0 (Sessile)"=0, "1 (Very slow)"=1, "2 (Slow)"=2, "3 (Moderate)"=3, "4 (Fast)"=4, "5 (Very fast/highly mobile)"=5))),
                 hr()
               )
             })
    ),
    
    # ----------- PART 2: METHOD ATTRIBUTES (UPDATED WITH FULL LABELS & NEW QUESTION) -----------
    tabPanel("Part 2: Method Attribute Assessment",
             h3("Instructions:"),
             p("For each detection method, please select the category that best describes its attributes."),
             
             lapply(method_list, function(method) {
               fluidRow(
                 column(12, h4(paste("Assessing Method:", method))),
                 
                 # UPDATED: All choices have labels and Temporal Scalability is added
                 column(12, radioButtons(paste0("q15_", tolower(gsub(" ", "_", method))), "15. Sensitivity:", inline = TRUE,
                                         choices = c("1 (Detects only high densities)"=1, "2 (Detects mod-high densities)"=2, "3 (Detects moderate densities)"=3, "4 (Detects low-mod densities)"=4, "5 (Detects very low densities)"=5))),
                 column(12, radioButtons(paste0("q17_", tolower(gsub(" ", "_", method))), "17. Spatial Scalability:", inline = TRUE,
                                         choices = c("1 (Point sample, <1ha)"=1, "2 (Small scale, <10ha)"=2, "3 (Moderate, <1km²)"=3, "4 (Large scale, <100km²)"=4, "5 (Very large scale, >100km²)"=5))),
                 column(12, radioButtons(paste0("q19_", tolower(gsub(" ", "_", method))), "19. Temporal Scalability:", inline = TRUE,
                                         choices = c("1 (One-time use only)"=1, "2 (Annually)"=2, "3 (Seasonally)"=3, "4 (Monthly)"=4, "5 (Weekly/Continuously)"=5))),
                 hr()
               )
             })
    ),
    
    # ----------- PART 3: WEIGHTING (Unchanged) -----------
    tabPanel("Part 3: Weighting Elicitation",
             h3("Instructions:"),
             p("For each question below, please distribute 100 points among the options based on their importance for the given scenario."),
             
             h4("23. For identifying a typical invasive species, how important is each trait?"),
             lapply(taxon_traits, function(trait) {
               numericInput(paste0("q23_", tolower(gsub(" ", "_", trait))), trait, value = 20, min = 0, max = 100)
             }),
             
             h4("27. For a successful early detection program, how important is each attribute?"),
             lapply(method_attributes, function(attr) {
               numericInput(paste0("q27_", tolower(gsub(" ", "_", attr))), attr, value = 20, min = 0, max = 100)
             })
    ),
    
    # ----------- PART 4: OVERALL RANKING (Unchanged) -----------
    tabPanel("Part 4: Overall Ranking & Submission",
             h3("Instructions:"),
             p("Please answer the final questions and rank the methods."),
             
             radioButtons("q36_best_method", "36. Which method is the single best for early detection?", choices = method_list),
             
             h4("40. Please rank the following methods from most useful (top) to least useful (bottom)."),
             rank_list(
               text = "Drag to reorder:",
               labels = method_list,
               input_id = "q40_rank"
             ),
             
             hr(),
             actionButton("submit", "Submit Your Responses", class = "btn-primary"),
             uiOutput("thank_you_message")
    )
  )
)


# ===================================================================
# SERVER - The back-end logic of the survey
# ===================================================================
server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    
    # --- UPDATED: Comprehensive data gathering ---
    
    # First, gather all the general data (Parts 2, 3, 4) that is the same for every taxon
    general_data <- list()
    general_data$expert_name <- input$expert_name
    general_data$submission_time <- as.character(Sys.time())
    
    # Part 2 Data
    for (method in method_list) {
      method_id <- tolower(gsub(" ", "_", method))
      general_data[[paste0("q15_sensitivity_", method_id)]] <- as.numeric(input[[paste0("q15_", method_id)]])
      general_data[[paste0("q17_spatial_scalability_", method_id)]] <- as.numeric(input[[paste0("q17_", method_id)]])
      general_data[[paste0("q19_temporal_scalability_", method_id)]] <- as.numeric(input[[paste0("q19_", method_id)]])
    }
    
    # Part 3 Data
    for (trait in taxon_traits) {
      general_data[[paste0("q23_weight_", tolower(gsub(" ", "_", trait)))]] <- input[[paste0("q23_", tolower(gsub(" ", "_", trait)))]]
    }
    for (attr in method_attributes) {
      general_data[[paste0("q27_weight_", tolower(gsub(" ", "_", attr)))]] <- input[[paste0("q27_", tolower(gsub(" ", "_", attr)))]]
    }
    
    # Part 4 Data
    general_data$q36_best_method <- input$q36_best_method
    ranks <- input$q40_rank
    for (i in 1:length(ranks)) {
      general_data[[paste0("q40_rank_", i)]] <- ranks[i]
    }
    
    # Now, create a data frame for each taxon, combining general data with taxon-specific data
    all_responses <- list()
    for (taxon in taxa_list) {
      taxon_id <- gsub(" - ", "_", gsub(" ", "_", taxon))
      
      taxon_specific_data <- list(
        taxon = taxon,
        q4_body_size = as.numeric(input[[paste0("q4_body_size_", taxon_id)]]),
        q1_cryptic = as.numeric(input[[paste0("q1_cryptic_", taxon_id)]]),
        q3_mobility = as.numeric(input[[paste0("q3_mobility_", taxon_id)]])
      )
      
      # Combine general and taxon-specific data for this row
      all_responses[[taxon]] <- as.data.frame(c(general_data, taxon_specific_data))
    }
    
    # Combine the list of data frames into a single, multi-row data frame
    final_data <- do.call(rbind, all_responses)
    
    # --- Save the data to a CSV file ---
    file_path <- "C:/Users/Administrador/OneDrive - McGill University/Emerging methods/survey_responses.csv"
    
    write.table(
      final_data, 
      file = file_path, 
      append = TRUE, 
      sep = ",", 
      row.names = FALSE, 
      col.names = !file.exists(file_path)
    )
    
    # --- Show a "Thank You" message to the user ---
    output$thank_you_message <- renderUI({
      h3("Thank you for your submission! Your responses have been saved.", style = "color: green;")
    })
    
    shinyjs::disable("submit")
  })
}

# -- Run the application --
shinyApp(ui = ui, server = server)
```

#### Part 2 -- Pairwise Weighted Scoring Model 

<img width="1250" height="1150" alt="Image" src="https://github.com/user-attachments/assets/1b32c6f7-f0ad-4fff-835e-b805877277bf" />

The values in this plot don't make sense, but hopefully this should give you an idea of what I have in mind... 

```
#By Jack A. Greenhalgh, Nov 2025.
#Department of Biology, McGill University, 1205 Dr Penfield Ave, Montreal, Quebec, H3A 1B1, Canada.

# =============================================
# Pairwise Weighted Scoring: Taxa vs Methods
# Final Corrected and Fully Functional Script
# =============================================

#### Load packages ####
library(pheatmap)
library(ComplexHeatmap)

#### Step 0: Define the importance of each taxa trait and methods attribute ####
Taxa <- data.frame(
  Taxon = c("Amphibians", "Arthropods", "Fishes", "Macrophytes"),
  Body.size = c(
    mean(c(2)),             # Amphibians
    mean(c(3, 3, 2)),       # Arthropods
    mean(c(2)),             # Fishes
    mean(c(5, 5))           # Macrophytes
  ),
  Mobility = c(
    mean(c(3)),             # Amphibians
    mean(c(3, 2, 3)),       # Arthropods
    mean(c(4)),             # Fishes
    mean(c(0, 0))           # Macrophytes
  ),
  Sound.production = c(
    mean(c(4)),             # Amphibians
    mean(c(3, 4, 5)),       # Arthropods
    mean(c(5)),             # Fishes
    mean(c(0, 0))           # Macrophytes
  ),
  Activity = c(
    mean(c(2)),             # Amphibians
    mean(c(1, 4, 1)),       # Arthropods
    mean(c(3)),             # Fishes
    mean(c(3, 3))           # Macrophytes
  ),
  Growing.season = c(
    mean(c(3)),             # Amphibians
    mean(c(4, 2, 4)),       # Arthropods
    mean(c(2)),             # Fishes
    mean(c(1, 1))           # Macrophytes
  ),
  Reproduction = c(
    mean(c(3)),             # Amphibians
    mean(c(4, 2, 4)),       # Arthropods
    mean(c(2)),             # Fishes
    mean(c(1, 1))           # Macrophytes
  )
)


Methods <- data.frame(
  Method = c("eDNA", "Acoustics", "Underwater camera", "Remote sensing"),
  Spatial.coverage = c(3, 4, 4, 5),
  Temporal.coverage = c(2, 3, 4, 5),
  Accuracy = c(5, 4, 3, 2),
  Logistics = c(5, 4, 3, 4),
  Presence = c(5, 4, 3, 2),
  Abundance = c(5, 4, 3, 2)
)

#### Step 1 & 2: Define Weighting of each taxon trait and method attribute ####

# --- Taxon trait weights (relative importance of traits per taxon) ---
taxon_trait_weights <- list(
  "Amphibians" = c(Body.size=0.15, Mobility=0.25, Sound.production=0.25, Activity=0.15, Growing.season=0.1, Reproduction=0.1),
  "Arthropods" = c(Body.size=0.2, Mobility=0.2, Sound.production=0.15, Activity=0.15, Growing.season=0.15, Reproduction=0.15),
  "Fishes" = c(Body.size=0.25, Mobility=0.25, Sound.production=0.2, Activity=0.1, Growing.season=0.1, Reproduction=0.1),
  "Macrophytes" = c(Body.size=0.3, Mobility=0, Sound.production=0, Activity=0.1, Growing.season=0.3, Reproduction=0.3)
)

# --- Method attribute weights (importance of each attribute per method) ---
method_attribute_weights <- list(
  "eDNA" = c(Spatial.coverage=0.15, Temporal.coverage=0.2, Accuracy=0.35, Logistics=0.1, Presence=0.1, Abundance=0.1),
  "Acoustics" = c(Spatial.coverage=0.25, Temporal.coverage=0.25, Accuracy=0.25, Logistics=0.1, Presence=0.1, Abundance=0.05),
  "Underwater camera" = c(Spatial.coverage=0.2, Temporal.coverage=0.15, Accuracy=0.25, Logistics=0.2, Presence=0.1, Abundance=0.1),
  "Remote sensing" = c(Spatial.coverage=0.35, Temporal.coverage=0.25, Accuracy=0.15, Logistics=0.1, Presence=0.1, Abundance=0.05)
)

# --- Overall weighting (or cost adjustment) per method ---
method_weights <- c(
  "eDNA" = 1,
  "Acoustics" = 1,
  "Underwater camera" = 1,
  "Remote sensing" = 1
)

#### Step 3: Define Trait–Attribute Interaction Matrix ####

traits <- c("Body.size", "Mobility", "Sound.production", "Activity", "Growing.season", "Reproduction")
attributes <- c("Spatial.coverage", "Temporal.coverage", "Accuracy", "Logistics", "Presence", "Abundance")

interaction_matrix <- matrix(
  c(
    # Spatial, Temporal, Accuracy, Logistics, Presence, Abundance
    0.8, 0.5, 1.0, 0.6, 0.8, 0.7,   # Body.size
    0.7, 0.8, 0.6, 0.5, 0.9, 0.8,   # Mobility
    0.5, 0.6, 0.8, 0.4, 1.0, 0.7,   # Sound.production
    0.4, 0.7, 0.5, 0.4, 0.8, 0.6,   # Activity
    0.6, 1.0, 0.4, 0.3, 0.6, 0.5,   # Growing.season
    0.5, 0.8, 0.5, 0.4, 0.9, 0.9    # Reproduction
  ),
  nrow = length(traits),
  byrow = TRUE,
  dimnames = list(traits, attributes)
)

#### Step 4: Compute Pairwise Score Function ####

compute_pairwise_score <- function(taxon_row, method_row, interactions,
                                   taxon_weights, method_attr_weights, method_weight = 1) {
  
  traits <- intersect(names(taxon_weights), rownames(interactions))
  attributes <- intersect(names(method_attr_weights), colnames(interactions))
  
  taxon_vals <- unlist(taxon_row[traits])
  method_vals <- unlist(method_row[attributes])
  taxon_w <- unlist(taxon_weights[traits])
  method_w <- unlist(method_attr_weights[attributes])
  
  interactions_sub <- interactions[traits, attributes, drop = FALSE]
  
  # Vectorized calculation using outer product
  score_matrix <- outer(taxon_vals * taxon_w,
                        method_vals * method_w,
                        FUN = `*`)
  
  # Element-wise multiply with interaction strengths
  score_matrix <- score_matrix * interactions_sub
  
  total_score <- sum(score_matrix, na.rm = TRUE) * method_weight
  return(total_score)
}

  #### Step 5: Compute Scores Matrix ####

scores_matrix <- matrix(0, nrow = nrow(Taxa), ncol = nrow(Methods))
rownames(scores_matrix) <- Taxa$Taxon
colnames(scores_matrix) <- Methods$Method

for (i in seq_len(nrow(Taxa))) {
  for (j in seq_len(nrow(Methods))) {
    taxon_name <- Taxa$Taxon[i]
    method_name <- Methods$Method[j]
    
    scores_matrix[i, j] <- compute_pairwise_score(
      Taxa[i, ],
      Methods[j, ],
      interaction_matrix,
      taxon_trait_weights[[taxon_name]],
      method_attribute_weights[[method_name]],
      method_weights[[method_name]]
    )
  }
}

#### Step 6: Analysis ####

best_methods <- apply(scores_matrix, 1, function(x) names(which.max(x)))
ranked_methods <- apply(scores_matrix, 1, function(x) names(sort(x, decreasing = TRUE)))

best_method_table <- data.frame(
  Taxon = rownames(scores_matrix),
  Best_Method = best_methods,
  Best_Score = apply(scores_matrix, 1, max)
)

print("=== Best Method per Taxon (Pairwise) ===")
print(best_method_table)

print("=== Ranked Methods per Taxon (Pairwise) ===")
for (taxon in rownames(scores_matrix)) {
  cat("\n", taxon, ":\n")
  cat(paste(ranked_methods[, taxon], collapse = " > "), "\n")
}
  
  #### Step 7: Visualization ####
library(ComplexHeatmap)
library(grid)

Heatmap(
  scores_matrix / max(scores_matrix),
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  heatmap_legend_param = list(title = "Detection probability"),
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.2f", scores_matrix[i, j]), x, y, gp = gpar(fontsize = 12))
  },
  col = colorRampPalette(c("white", "purple"))(50),
  column_names_rot = 45
)

```
