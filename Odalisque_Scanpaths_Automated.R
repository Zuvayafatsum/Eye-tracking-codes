# Load necessary libraries
# install.packages(c("tidyverse", "patchwork")) # Run if you haven't installed patchwork
library(tidyverse)
library(patchwork)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================
# Define the target area dimensions in pixels (Landscape)
target_width <- 2200
target_height <- 1225

# ==============================================================================
# 2. DATA LOADING AND CLEANING
# ==============================================================================
fixations_raw <- read_csv("/Users/mustafacvbe/Downloads/img_data 2/Odalisque_1/fixations.csv", show_col_types = FALSE)

fixations_clean <- fixations_raw %>%
  rename(
    section_id = `section id`,
    recording_id = `recording id`,
    fixation_id = `fixation id`,
    start_time_ns = `start timestamp [ns]`,
    end_time_ns = `end timestamp [ns]`,
    duration_ms = `duration [ms]`,
    is_on_target = `fixation detected in reference image`,
    x_px = `fixation x [px]`,
    y_px = `fixation y [px]`
  )

# ==============================================================================
# 3. FILTERING & PREPARATION
# ==============================================================================
target_fixations <- fixations_clean %>%
  filter(is_on_target == TRUE) %>%
  filter(!is.na(x_px) & !is.na(y_px)) %>%
  arrange(recording_id, start_time_ns)

# ==============================================================================
# 4. PLOTTING FUNCTION
# ==============================================================================
generate_scanpath <- function(data_subset, rec_id, width, height) {
  
  # Truncate recording ID for a cleaner, readable title on each subplot
  short_id <- substr(rec_id, 1, 8)
  
  plot <- ggplot(data_subset, aes(x = x_px, y = y_px)) +
    geom_path(color = "gray40", alpha = 0.6, linewidth = 0.8) +
    geom_point(aes(size = duration_ms), color = "steelblue", alpha = 0.8) +
    geom_text(aes(label = fixation_id), vjust = -1.2, size = 3, color = "black") +
    
    # Set axis limits
    scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
    scale_y_reverse(limits = c(height, 0), expand = c(0, 0)) +
    
    # CRITICAL FIX: Lock the aspect ratio to 1:1 to strictly enforce the landscape dimensions
    coord_fixed(ratio = 1) + 
    
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Recording ID:", short_id),
      x = "X [px]",
      y = "Y [px]",
      size = "Duration [ms]"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.grid.minor = element_blank()
    )
  
  return(plot)
}

unique_recordings <- unique(target_fixations$recording_id)

# ==============================================================================
# 5. AUTOMATED BACKGROUND OVERLAY (FOOLPROOF GRID METHOD)
# ==============================================================================
# We will use the 'jpeg' or 'png' package along with 'grid' to perfectly map 
# the image strictly to the plot panel, bypassing axis reversal issues entirely.
# install.packages(c("jpeg", "png")) # Run if you don't have these installed
library(grid)
library(magick)
library(ggprism)
# 1. Load the background image
# Replace with your actual file name. If using a PNG, use png::readPNG("file.png")
target_image_raw <- image_read("/Users/mustafacvbe/Downloads/img_data 2/Odalisque_1/reference_image.jpeg")


# 2. Convert to a Graphical Object (Grob) that explicitly fills 100% of the panel area
# unit(1, "npc") stands for "Normalized Parent Coordinates", meaning it scales 
# exactly to the borders of the plot regardless of the axis values.
target_grob <- rasterGrob(
  target_image_raw, 
  width = unit(1, "npc"), 
  height = unit(1, "npc"), 
  interpolate = TRUE
)

# 3. Modified plotting function using annotation_custom
generate_automated_scanpath <- function(data_subset, width, height, background_grob) {
  
  plot <- ggplot(data_subset, aes(x = x_px, y = y_px)) +
    
    # CRITICAL: This places the image perfectly in the background panel.
    # -Inf to Inf ensures it stretches exactly to the coordinate limits defined below
    annotation_custom(background_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    
    # Scanpath layers (Using the turquoise aesthetic from Section 8)
    geom_path(color = "turquoise", alpha = 0.6, linewidth = 0.8) +
    geom_point(color = "turquoise", size = 3, alpha = 0.8) +
    
    # Enforce precise dimensions and locked aspect ratio
    scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
    scale_y_reverse(limits = c(height, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 1) + 
    
    # Theme styling (Prism)
    theme_prism(base_size = 12) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
      
      # Ensure the panel itself is transparent so the grob beneath is visible
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(plot)
}

# 4. Initialize a new list for the automated plots
automated_scanpath_plots <- list()

# Iterate through the unique recordings already in memory
for (rec_id in unique_recordings) {
  current_data <- target_fixations %>% filter(recording_id == rec_id)
  
  # Generate the visualization, passing the Grob into the function
  p <- generate_automated_scanpath(current_data, target_width, target_height, target_grob)
  automated_scanpath_plots[[rec_id]] <- p
}

# 5. Combine all plots side-by-side using patchwork
automated_combined_plot <- wrap_plots(automated_scanpath_plots, ncol = 2)

# Render the final embedded plot in RStudio
print(automated_combined_plot)

# 6. Export the final, presentation-ready image
ggsave(
  filename = "automated_scanpaths_with_background.png", 
  plot = automated_combined_plot, 
  width = 16, 
  height = 9, 
  dpi = 300,
  bg = "white" # Background can be white now since the target image is embedded
)


# 6 ALTERNATIVE TRANSPERANT BACKGROUND -----------------------------------------------------------------------


# 3. Modified plotting function using annotation_custom and a fade layer
generate_automated_scanpath_transperant <- function(data_subset, width, height, background_grob) {
  
  plot <- ggplot(data_subset, aes(x = x_px, y = y_px)) +
    
    # LAYER 1: The full-resolution background image
    annotation_custom(background_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    
    # LAYER 2: The "Tracing Paper" Fade Layer
    # TWEAK THIS PARAMETER: Change alpha (e.g., 0.3 for a light fade, 0.8 for a heavy wash)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
             fill = "white", alpha = 0.1) +
    
    # LAYER 3: The Scanpath Data
    geom_path(color = "turquoise", alpha = 0.6, linewidth = 0.8) +
    geom_point(color = "turquoise", size = 3, alpha = 0.8) +
    
    # Enforce precise dimensions and locked aspect ratio
    scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
    scale_y_reverse(limits = c(height, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 1) + 
    
    # Theme styling (Prism)
    theme_prism(base_size = 12) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(plot)
}


automated_scanpath_plots <- list()

# Iterate through the unique recordings already in memory
for (rec_id in unique_recordings) {
  current_data <- target_fixations %>% filter(recording_id == rec_id)
  
  # Generate the visualization, passing the Grob into the function
  p <- generate_automated_scanpath_transperant(current_data, target_width, target_height, target_grob)
  automated_scanpath_plots[[rec_id]] <- p
}

# 5. Combine all plots side-by-side using patchwork
automated_combined_plot <- wrap_plots(automated_scanpath_plots, ncol = 2)

# Render the final embedded plot in RStudio
print(automated_combined_plot)
