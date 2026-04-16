
## =============================================================================
##  DESCRIPTIVE RESULTS -- PLOTS -- Functions_descriptive_plot.R
## =============================================================================
# -   **Function descriptive plot for participant-level and group-level visualization:** These plotting functions are designed to visualize environmental,physiological, perceptuals and behavioral data across periods and heatwave phases.
#
# -   `boxplot_numeric`:Generates boxplots with overlaid jittered points for each numeric outcome, grouped by period and/or heatwave status. Effective for group-level comparisons of numeric responses across time and conditions.
#
# -   `time_series_combine_plot_two_way`:Creates a multi-panel time series plot for outcomes measured three times per day, grouped by heatwave phase and period of day.It visualizes group-level trends with uncertainty bands, but without displaying individual trajectories.
#
# -   `time_series_combine_plot_one_way:` Generates faceted time series plots for outcomes measured once per day, across different periods of the day and heatwave phases. It visualizes group-level trends with uncertainty bands, but without displaying individual trajectories.
#
# -   `time_series_combine_plot_environment:` Creates a faceted time series plot for environmental variables measured once per day, separately for indoor and outdoor conditions. The plots show group-level trends across the heatwave phases.
#
# -   `time_series_combine_plot_two_way_individual`: Create a visual summary combining plots of physiological response according to heatwave OR/AND period (variables with three daily measurement). Individual participant trajectories are shown as thin grey lines behind the mean ± CI.
#
# -   `time_series_combine_plot_one_way_individual` Create a visual summary combining plots according to heatwave status (variables with only single daily measurement). Individual participant trajectories are shown as thin grey lines behind the mean ± CI.
#
# -   `time_series_combine_plot_environment_individual:`Create a visual summary of indoor environmental conditions over time, grouped by heatwave status and period of day. Individual apartment/sensor trajectories are shown as thin grey lines behind the mean ± CI.
# 
# -   `time_series_environment_by_phase`: Create a visual summary of environmental conditions (temperature, humidity) across heatwave phases. Individual trajectories are shown as thin grey lines behind the mean ± CI, with vertical lines marking heatwave start/end.
#
# -   `time_series_heatwave_AC`: Generates a time series plot for outcomes measured once per day, grouped by heatwave phase and air conditioning status. It visualizes group-level trends with uncertainty bands, but without displaying individual trajectories.
#
# -   `violin_plot_ordinal:` Create violin plots for ordinal outcome variables grouped by either "period" or "heatwave_status"
#
# -   `chart_categorical:` Generate proportional stacked bar charts for categorical or ordinal variables by "period" or "heatwave_status".
#
# -   `bar_chart_plot:` Create proportional stacked bar charts for categorical data with faceting by either period or heatwave phase.
#
# -   `heatmap_plot:` Generate heatmaps for numeric outcome variables, with x and y axes based on period and heatwave status.
#
# -   `heatmap_plot_long`: Create long-format daily heatmaps showing temporal trends of numeric outcomes with annotations marking heatwave phases.
#
# -   `heatmap_plot_facets_fixed`: Generate faceted heatmaps for numeric outcomes, with fixed scales across facets to allow direct comparison of temporal trends across periods and heatwave phases.
#
# -   `heatmap_plot_long_AC`: Create long-format daily heatmaps for outcomes measured once per day, grouped by heatwave phase and air conditioning status.
#
# -   `heatmap_plot_facets_fixed_AC`: Generate faceted heatmaps for outcomes measured once per day, grouped by heatwave phase and air conditioning status, with fixed scales across facets.


## =============================================================================
# Function 1: Create a visual summary box plot according to period and heatwave
boxplot_numeric <- function(data_list,
                            outcome_variables,
                            plot_type = "period",
                            output_dir = path_res) {
  # Define how variables will be mapped to x-axis and fill color based on the selected plot_type
  plot_types <- list(
    period = list(x = "period", fill = "period"),
    heatwave = list(x = "heatwave_status", fill = "heatwave_status"),
    heatwave_period = list(x = "heatwave_status", fill = "period"),
    period_heatwave = list(x = "period", fill = "heatwave_status")
  )
  
  # Check that the selected plot_type is valid
  if (!(plot_type %in% names(plot_types)))
    stop(
      "Invalid plot_type. Choose from: period, heatwave, heatwave_period, period_heatwave"
    )
  
  # Apply the plotting process to each outcome variable
  results_list <- lapply(names(outcome_variables), function(outcome_name) {
    # Retrieve the appropriate dataset and outcome variable name
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    
    # Skip if outcome variable is not in the dataset
    if (!outcome_var %in% names(data))
      return(NULL)
    # Filter for relevant heatwave periods and factorize the levels for plotting consistency
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        period = factor(period, levels = c("AM", "PM", "NI")),
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
        )
      )
    
    # Build the ggplot object with boxplot and jittered individual points
    
    plot <- ggplot(data,
                   aes(
                     x = .data[[plot_types[[plot_type]]$x]],
                     y = .data[[outcome_var]],
                     fill = .data[[plot_types[[plot_type]]$fill]]
                   )
                   ) +
      geom_boxplot(
        outlier.colour = "red",
        outlier.shape = 1,
        outlier.size = 2,
        width = 0.5
      ) +
      geom_jitter(color = "gray",
                  width = 0.2,
                  alpha = 0.6) + # add raw data points
      guides(color = "none")
    # Add labels, themes, and clean up the aesthetics
    plot <- plot +
      scale_fill_viridis_d(option = "D") +
      labs(x = NULL, y = outcome_name, fill = "") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        axis.text.x = element_blank(),
        # Hide x-axis labels
        axis.ticks.x = element_blank(),
        # Hide x-axis ticks
        axis.text.y = element_text(size = 11),
        # Larger y-axis text
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white")
      )
    
    # Optionally add special color settings for certain plot types
    if (plot_type %in% c("heatwave_period", "period_heatwave")) {
      plot <- plot + #geom_jitter(aes(color = "participant"), alpha = 0.6, position = position_dodge(width = 0.75)) +
        scale_color_manual(values = c("gray" = "gray"))
    }
    
    # Save individual plot to file
    ggsave(
      filename = file.path(
        output_dir,
        paste0("boxplot_", outcome_name, "_", plot_type, ".png")
      ),
      plot = plot,
      bg = "white",
      width = 6,
      height = 5,
      dpi = 300
    )
    return(plot)
  })
  
}



## =============================================================================
# Function 2: Create a visual summary combining plots of physiological response according to heatwave (variables with only single day measurement)

time_series_combine_plot_two_way <- function(data_list, 
                                             outcome_variables, 
                                             output_dir) {
  full_data <- data.frame() # Initialize empty data frame to store all summarised data
  
  for (outcome_name in names(outcome_variables)) {
    # Extract dataset and relevant variable names for the current outcome
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var <- outcome_variables[[outcome_name]]$date
    
    # Skip if the outcome variable is not in the dataset
    if (!outcome_var %in% names(data))
      next
    
    # Filter only relevant heatwave periods and format variables
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        period = factor(period, 
                        levels = c("AM", "PM", "NI"),
                        labels = c("Morning", "Afternoon", "Night")),# <- Renommer les périodes pour une meilleure lisibilité
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
        )
      )
    data$outcome_name <- outcome_name # Add outcome label
    
    
    
    # Summarize data: compute mean, SD, CI per date, period, and heatwave_status
    summarised_data <- data %>%
      group_by(!!sym(time_var), period, heatwave_status) %>%
      summarise(
        mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
        sd_value = sd(!!sym(outcome_var), na.rm = TRUE),
        min_value = min(!!sym(outcome_var), na.rm = TRUE),
        max_value = max(!!sym(outcome_var), na.rm = TRUE),
        n = n(),
        lower_CI = mean_value - 1.96 * (sd_value / sqrt(n)),
        upper_CI = mean_value + 1.96 * (sd_value / sqrt(n)),
        .groups = "drop"
      ) %>%
      mutate(outcome_name = outcome_name, date = !!sym(time_var))
    
    
    # Set outcome label and order
    outcome_order_raw <- c(
      "Systolic Blood Pressure (mmHg)",
      "Diastolic Blood Pressure (mmHg)",
      "Heart Rate (bpm)",
      "Daily change\nOral Temperature (deg C)"
    )
    outcome_order_wrapped <- str_wrap(outcome_order_raw, width = 20)
    
    # Apply label in the right order
    wrapped_outcome_name <- str_wrap(outcome_name, width = 20)
    summarised_data$outcome_name <- factor(wrapped_outcome_name, levels = outcome_order_wrapped)
    
    # ← NOUVEAU : Renommer les niveaux de heatwave_status
    summarised_data <- summarised_data %>%
      mutate(
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
          labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave")  # ← Nouveaux noms
        )
      )
    
    full_data <- bind_rows(full_data, summarised_data)
    
  }
  
  # Dates heatwave
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end <- as.Date("2024-06-20")
  
  # Main plot with heatwave_status + daily trend
  p_main <- ggplot(
    full_data,
    aes(
      x = date,
      y = mean_value,
      fill = heatwave_status,
      color = heatwave_status
    )
  ) +
    
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                alpha = 0.3,
                color = NA) + #draw 95% CI
    geom_ribbon(
      aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
      alpha = 0.2,
      fill = "grey80",
      color = NA
    ) +
    
    geom_line(linewidth = 1.5, aes(color = heatwave_status), linetype = "solid", alpha = 1) + # line within 95%CI which represent mean per period per heatwave
    
    #geom_line(data = full_daily, aes(x = date, y = daily_mean, group = outcome_name), inherit.aes = FALSE, color = "grey40", size = 0.2) + #mean per day across all period
    scale_fill_viridis_d(name = NULL) +
    scale_color_viridis_d(name = NULL) +
    facet_grid(
      rows = vars(outcome_name),
      cols = vars(period),
      scales = "free_y",
      switch="y" #label to the left
    ) +
    theme_minimal(base_size = 18) +
    theme(
      # Axe X
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      
      # Axe Y - TOUT À GAUCHE
      axis.text.y.left = element_text(size = 24),  # ← Valeurs Y à GAUCHE
      axis.text.y.right = element_blank(),  # ← Rien à droite
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      
      # Labels des facettes (noms des outcomes) à GAUCHE
      strip.text.y.left = element_text(
        size = 24, 
        angle = 90,
        vjust= 0.5,
        hjust = 0.5),  # ← Augmenté et aligné
      strip.text.x.top = element_text(size = 24),  # ← Augmenté et en gras
      strip.placement = "outside",
      
      # Légende à GAUCHE
      legend.position = "top",
      legend.direction= "horizontal",
      legend.text = element_text(size = 26),  # ← Augmenté
      legend.title = element_text(size = 22, face="bold"),  # ← Augmenté
      legend.key.size = unit(1.5, "cm"),  # ← Plus grande légende
      
      
      # ← NOUVEAU : Rectangle autour de la légende
      legend.background = element_rect(
        fill = "white",      # Fond blanc
        color = "grey50",     # Bordure noire
        size = 0.5,          # Épaisseur de la bordure
        linetype = "solid"   # Ligne continue
      ),
      legend.margin = margin(10, 15, 10, 15), 
      # Espacement
      panel.spacing.x = unit(1.2, "lines"),  # ← Augmenté
      panel.spacing.y = unit(2, "lines"),  # ← Augmenté
      
      # Grille
      panel.grid.major = element_line(color = "grey65", size = 0.15),  # ← Plus épais
      panel.grid.minor = element_line(color = "grey85", size = 0.10)
    ) +
    # Positionner l'axe Y à DROITE
    scale_y_continuous(position = "left")  # ← NOUVEAU : axe Y à droite
  
  # X-axis at bottom only
  x_axis <- ggplot(full_data, aes(x = date)) +
    facet_grid(cols = vars(period)) +
    scale_x_date(
      date_labels = "%b %d",
      breaks = seq(min(full_data$date), max(full_data$date), by = "3 days"),
      expand = c(0, 2) # remove blank space to the graph
    ) +
    theme_minimal(base_size=18) +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        size = 18
      ),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.line = element_blank()
    )
  
  # Apply margin theme to each plot BEFORE combining
  p_main <- p_main + theme(plot.margin = margin(5, 5, 0, 5))
  x_axis <- x_axis + theme(plot.margin = margin(0, 5, 5, 5))
  
  # Combine main and axis plots
  final_plot <- p_main / x_axis +
    plot_layout(heights = c(25, 0))
  
  # Save plot
  output_file <- file.path(output_dir,
                           paste0("combined_timeseries_plot_two_way.png"))
  
  # Avant de sauvegarder, fermez tous les devices graphiques
  
  ggsave(
    filename=output_file,
    plot = final_plot,
    width = 22,
    height = 16,
    dpi=300,
    device = "png"
  )
}

## =============================================================================
# Function 3: Create a visual summary combining plots of response according to heatwave (variables with only single day measurement)

time_series_combine_plot_one_way <- function(data_list, 
                                             outcome_variables, 
                                             output_dir) {
  full_data <- data.frame() # Initialize empty data frame to store all summarised data
  
  
  for (outcome_name in names(outcome_variables)) {
    # Extract relevant dataset and variables
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var <- outcome_variables[[outcome_name]]$date
    
    # Skip if outcome_var is not present in the dataset
    if (!outcome_var %in% names(data))
      next
    
    # Keep relevant periods and set proper formats
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        period = factor(period, levels = c("AM", "PM", "NI")), # Rename periods for better readability
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
          labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave")  # New labels
        )
      )
    data$outcome_name <- outcome_name # Add outcome label
    
    
    
    # Summarize data: compute mean, SD, CI per date, period, and heatwave_status
    summarised_data <- data %>%
      group_by(!!sym(time_var), period, heatwave_status) %>%
      summarise(
        mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
        sd_value = sd(!!sym(outcome_var), na.rm = TRUE),
        min_value = min(!!sym(outcome_var), na.rm = TRUE),
        max_value = max(!!sym(outcome_var), na.rm = TRUE),
        n = n(),
        lower_CI = mean_value - 1.96 * (sd_value / sqrt(n)),
        upper_CI = mean_value + 1.96 * (sd_value / sqrt(n)),
        .groups = "drop"
      ) %>%
      mutate(outcome_name = outcome_name, date = !!sym(time_var))
    
    
    
    # Set outcome label and order
    outcome_order_raw <- c(
      #"Body Mass (kg)",
      "Daily change\nBody Mass (g)",
      "Urine Frequency per Day",
      "Mean Daily Acceleration (mg)"
    )
    outcome_order_wrapped <- str_wrap(outcome_order_raw, width = 20)
    
    
    wrapped_outcome_name <- str_wrap(outcome_name, width = 20)
    summarised_data$outcome_name <- factor(wrapped_outcome_name, levels = outcome_order_wrapped)
    
    
    # Combine data
    full_data <- bind_rows(full_data, summarised_data)
    
  }
  
  
  # Optional: Heatwave start/end (for reference lines if needed)
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end <- as.Date("2024-06-20")
  
  # Main plot
  p_main <- ggplot(
    full_data,
    aes(
      x = date,
      y = mean_value,
      fill = heatwave_status,
      color = heatwave_status
    )
  ) +
    
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                alpha = 0.3,
                color = NA) + #draw 95% CI
    geom_ribbon(
      aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
      alpha = 0.2,
      fill = "grey80",
      color = NA
    ) +
    
    geom_line(linewidth = 1.5, aes(color = heatwave_status), linetype = "solid", alpha = 1) + # line within 95%CI which represent mean per period per heatwave
    
    scale_fill_viridis_d(name = NULL) +
    scale_color_viridis_d(name = NULL) +
    facet_grid(
      rows = vars(outcome_name),
      cols = vars(period),
      scales = "free_y",
      switch="y" #label to the left
    ) +
    theme_minimal(base_size = 18) +
    theme(
      # Axe X
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      
      # Axe Y - TOUT À GAUCHE
      axis.text.y.left = element_text(size = 26),  # ← Valeurs Y à GAUCHE
      axis.text.y.right = element_blank(),  # ← Rien à droite
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      
      # Labels des facettes (noms des outcomes) à GAUCHE
      strip.text.y.left = element_text(
        size = 26, 
        angle = 90,
        vjust= 0.5,
        hjust = 0.5),  # ← Augmenté et aligné
      strip.text.x.top = element_text(size = 24),  # ← Augmenté 
      strip.placement = "outside",
      
      # Légende à GAUCHE
      legend.position = "top",
      legend.direction= "horizontal",
      legend.text = element_text(size = 28),  # ← Augmenté
      legend.key.size = unit(1.5, "cm"),  # ← Plus grande légende
      
      
      # ← NOUVEAU : Rectangle autour de la légende
      legend.background = element_rect(
        fill = "white",      # Fond blanc
        color = "grey50",     # Bordure noire
        size = 0.5,          # Épaisseur de la bordure
        linetype = "solid"   # Ligne continue
      ),
      legend.margin = margin(10, 15, 10, 15), 
      # Espacement
      panel.spacing.x = unit(1.2, "lines"),  # ← Augmenté
      panel.spacing.y = unit(2, "lines"),  # ← Augmenté
      
      # Grille
      panel.grid.major = element_line(color = "grey65", size = 0.15),  # ← Plus épais
      panel.grid.minor = element_line(color = "grey85", size = 0.10)
    ) +
    # Positionner l'axe Y à DROITE
    scale_y_continuous(position = "left")  # ← NOUVEAU : axe Y à droite
  
  # X-axis only panel
  x_axis <- ggplot(full_data, aes(x = date)) +
    facet_grid(cols = vars(period)) +
    scale_x_date(
      date_labels = "%b %d",
      breaks = seq(min(full_data$date), max(full_data$date), by = "3 days"),
      expand = c(0, 1) # Eviter l'espace blanc à gauche et à droite de l'axe
    ) +
    theme_minimal(base_size=18) +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        size = 24
      ),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.line = element_blank()
    )
  
  # Combine main and axis plots
  p_main <- p_main + theme(plot.margin = margin(5, 5, 0, 5))
  x_axis <- x_axis + theme(plot.margin = margin(0, 5, 5, 5))
  
  final_plot <- (p_main / x_axis) + plot_layout(heights = c(25, 0))
  
  print(final_plot)
  
  output_file<- file.path(output_dir, "combined_timeseries_plot_one_way.png")
  
  ggsave(
    filename=output_file,
    plot = final_plot,
    width = 22,
    height = 16,
    device = "png"
  )
}


## =============================================================================
# Function 4: Create a visual summary combining plots of environmental response according to heatwave (variables with only single day measurement)

time_series_combine_plot_environment <- function(data_list, 
                                                 outcome_variables, 
                                                 output_dir) {
  
  full_data <- data.frame() # Initialize empty data frame to store all summarised data
  
  
  for (outcome_name in names(outcome_variables)) {
    # Extract relevant dataset and variables
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var <- outcome_variables[[outcome_name]]$date
    
    # Skip if outcome_var is not present in the dataset
    if (!outcome_var %in% names(data))
      next
    
    # Keep relevant periods and set proper formats
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        period = factor(period, levels = c("AM", "PM", "NI")), # Rename periods for better readability
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
          labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave")  # New labels
        )
      )
    data$outcome_name <- outcome_name # Add outcome label
    
    # Summarize data: compute mean, SD, CI per date, period, and heatwave_status 
    summarised_data <- data %>%
      group_by(!!sym(time_var), period, heatwave_status) %>%
      summarise(
        mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
        sd_value = sd(!!sym(outcome_var), na.rm = TRUE),
        min_value = min(!!sym(outcome_var), na.rm = TRUE),
        max_value = max(!!sym(outcome_var), na.rm = TRUE),
        n = n(),
        lower_CI = mean_value - 1.96 * (sd_value / sqrt(n)),
        upper_CI = mean_value + 1.96 * (sd_value / sqrt(n)),
        .groups = "drop"
      ) %>%
      mutate(outcome_name = outcome_name, date = !!sym(time_var))
    
    # Set outcome label and order
    outcome_order_raw <- c(
      "Indoor Average Temperature (°C)",
      "Indoor Minimal Temperature (°C)",
      "Indoor Maximal Temperature (°C)",
      "Indoor Average Relative Humidity (%)"
    )
    outcome_order_wrapped <- str_wrap(outcome_order_raw, width = 20)
    
    wrapped_outcome_name <- str_wrap(outcome_name, width = 20)
    summarised_data$outcome_name <- factor(wrapped_outcome_name, levels = outcome_order_wrapped)
    
    full_data <- bind_rows(full_data, summarised_data)
  }
  
  #
  # Optional: draw vertical lines for heatwave start/end
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end <- as.Date("2024-06-20")
  
  # Appliquer l'ordre souhaité sur environment
  #full_data$environment <- factor(full_data$environment, levels = environment_order)
  
  # ---- Create main plot with CI ribbons and mean lines ----
  p_main <- ggplot(
    full_data,
    aes(
      x = date,
      y = mean_value,
      fill = heatwave_status,
      color = heatwave_status
    )
  ) +
    
    # 95% Confidence Interval Ribbon
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                alpha = 0.3,
                color = NA) + 
    
    # Standard Deviation Ribbon
    geom_ribbon(
      aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
      alpha = 0.2,
      fill = "grey80",
      color = NA
    ) +
    
    # Line for the mean value
    geom_line(linewidth = 1.5, aes(color = heatwave_status), linetype = "solid", alpha = 1.2) + # line within 95%CI which represent mean per period per heatwave
    
    #geom_line(data = full_daily, aes(x = date, y = daily_mean, group = outcome_name), inherit.aes = FALSE, color = "grey40", size = 0.2) + #mean per day across all period
    scale_fill_viridis_d(name = NULL) +
    scale_color_viridis_d(name = NULL) +
    facet_grid(
      rows = vars(outcome_name),
      cols = vars(period),
      scales = "free_y",
      switch="y" #label to the left
    ) +
    theme_minimal(base_size = 18) +
    theme(
      # Axe X
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      
      # Axe Y - TOUT À GAUCHE
      axis.text.y.left = element_text(size = 26),  # ← Valeurs Y à GAUCHE
      axis.text.y.right = element_blank(),  # ← Rien à droite
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      
      # Labels des facettes (noms des outcomes) à GAUCHE
      strip.text.y.left = element_text(
        size = 26, 
        angle = 90,
        vjust= 0.5,
        hjust = 0.5),  # ← Augmenté et aligné
      strip.text.x.top = element_text(size = 24),  # ← Augmenté 
      strip.placement = "outside",
      
      # Légende à GAUCHE
      legend.position = "top",
      legend.direction= "horizontal",
      legend.text = element_text(size = 28),  # ← Augmenté
      legend.key.size = unit(1.5, "cm"),  # ← Plus grande légende
      
      
      # ← NOUVEAU : Rectangle autour de la légende
      legend.background = element_rect(
        fill = "white",      # Fond blanc
        color = "grey50",     # Bordure noire
        linewidth = 0.5,          # Épaisseur de la bordure
        linetype = "solid"   # Ligne continue
      ),
      legend.margin = margin(10, 15, 10, 15), 
      # Espacement
      panel.spacing.x = unit(1.2, "lines"),  # ← Augmenté
      panel.spacing.y = unit(2, "lines"),  # ← Augmenté
      
      # Grille
      panel.grid.major = element_line(color = "grey65", linewidth = 0.15),  # ← Plus épais
      panel.grid.minor = element_line(color = "grey85", linewidth = 0.10)
    ) +
    # Positionner l'axe Y à DROITE
    scale_y_continuous(position = "left",labels = scales::number_format(accuracy = 1))  # ← NOUVEAU : axe Y à droite
  
  
  # ---- Add bottom blank panel for spacing (x-axis position control) ----
  x_axis <- ggplot(full_data, aes(x = date)) +
    facet_grid(cols = vars(period)) +
    scale_x_date(
      date_labels = "%b %d",
      breaks = seq(min(full_data$date), max(full_data$date), by = "3 days"),
      expand = c(0, 1) # Eviter l'espace blanc à gauche et à droite de l'axe
    ) +
    theme_minimal(base_size=18) +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        size = 24
      ),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.line = element_blank()
    )
  
  # Combine main plot and x-axis dummy space
  final_plot <- p_main / x_axis
  final_plot <- final_plot + plot_layout(heights = c(25, 1))
  print(final_plot)
  
  # Display and save the plot
  print(final_plot)
  output_file <- file.path(output_dir,
                           paste0("combined_timeseries_plot.png"))
  
  ggsave(
    filename=output_file,
    plot = final_plot,
    width = 22,
    height = 16,
    dpi=300,
    device = "png"
  )
}









## =============================================================================
# Function 5:Create a visual summary combining plots of responses according to heatwave OR/AND period (variables with three daily measurement)
# Individual participant trajectories are shown as thin grey lines behind the mean ± CI.

time_series_combine_plot_two_way_individual <- function(data_list,
                                                        outcome_variables,
                                                        output_dir) {
  
  full_data          <- data.frame() # Will store summarised data for all outcomes
  full_individual    <- data.frame() # Will store individual-level data for all outcomes
  
  for (outcome_name in names(outcome_variables)) {
    
    # Extract dataset and variable names for this outcome
    data        <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var    <- outcome_variables[[outcome_name]]$date
    
    # Skip outcome if variable is absent from the dataset
    if (!outcome_var %in% names(data)) next
    
    # Filter and format 
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        period = factor(period,
                        levels = c("AM", "PM", "NI"),
                        labels = c("Morning", "Afternoon", "Night")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
                                 labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave"))
      )
    
    # Wrap outcome name for facet label 
    outcome_order_raw     <- c("Systolic Blood Pressure (mmHg)",
                               "Diastolic Blood Pressure (mmHg)",
                               "Heart Rate (bpm)",
                               "Daily change\nOral Temperature (deg C)")
    outcome_order_wrapped <- str_wrap(outcome_order_raw, width = 20)
    wrapped_outcome_name  <- str_wrap(outcome_name, width = 20)
    
    # Summarise: mean, SD, CI per date × period × heatwave_status 
    summarised_data <- data %>%
      group_by(!!sym(time_var), period, heatwave_status) %>%
      summarise(
        mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
        sd_value   = sd(!!sym(outcome_var),   na.rm = TRUE),
        n          = n(),
        lower_CI   = mean_value - 1.96 * (sd_value / sqrt(n)),
        upper_CI   = mean_value + 1.96 * (sd_value / sqrt(n)),
        .groups    = "drop"
      ) %>%
      mutate(
        outcome_name = factor(wrapped_outcome_name, levels = outcome_order_wrapped),
        date         = !!sym(time_var)
      )
    
    # Individual-level data (one row per participant × date × period) 
    individual_data <- data %>%
      filter(!is.na(!!sym(outcome_var))) %>%
      mutate(
        outcome_name = factor(wrapped_outcome_name, levels = outcome_order_wrapped),
        date         = !!sym(time_var),
        raw_value    = !!sym(outcome_var)
      ) %>%
      select(participant_id, date, period, heatwave_status, outcome_name, raw_value)
    
    # Append to full datasets 
    full_data       <- bind_rows(full_data,       summarised_data)
    full_individual <- bind_rows(full_individual, individual_data)
  }
  
  # Heatwave boundary dates 
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end   <- as.Date("2024-06-20")
  
  # Main plot 
  p_main <- ggplot(full_data,
                   aes(x = date, y = mean_value,
                       fill = heatwave_status, color = heatwave_status)) +
    
    # 1) Individual participant trajectories — thin grey lines per participant × period × phase
    geom_line(
      data = full_individual,
      aes(x     = date,
          y     = raw_value,
          group = interaction(participant_id, period, heatwave_status)),
      color       = "grey50",
      alpha       = 0.3,
      size        = 0.5,
      inherit.aes = FALSE
    ) +
    
    # 2) 95% confidence interval ribbon
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                alpha = 0.4, color = NA) +
    
    # 3) ±1 SD ribbon (grey, behind the coloured CI)
    geom_ribbon(aes(ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value),
                alpha = 0.25, fill = "grey80", color = NA) +
    
    # 4) Mean line per heatwave phase
    geom_line(size = 1.5, linetype = "solid", alpha = 1) +
    
    
    scale_fill_viridis_d(name = NULL) +
    scale_color_viridis_d(name = NULL) +
    
    # Facet: outcomes in rows, periods of day in columns
    facet_grid(rows   = vars(outcome_name),
               cols   = vars(period),
               scales = "free_y",
               switch = "y") +
    
    scale_y_continuous(position = "left") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 18) +
    theme(
      # X-axis hidden (shown via separate x_axis plot below)
      axis.text.x  = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      # Y-axis
      axis.text.y.left  = element_text(size = 24),
      axis.text.y.right = element_blank(),
      axis.title.y      = element_blank(),
      
      # Facet strip labels
      strip.text.y.left = element_text(size = 24, angle = 90, vjust = 0.5, hjust = 0.5),
      strip.text.x.top  = element_text(size = 24),
      strip.placement   = "outside",
      
      # Legend
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.text       = element_text(size = 26),
      legend.title      = element_text(size = 22, face = "bold"),
      legend.key.size   = unit(1.5, "cm"),
      legend.background = element_rect(fill = "white", color = "grey50",
                                       size = 0.5, linetype = "solid"),
      legend.margin     = margin(10, 15, 10, 15),
      
      # Panel spacing and grid
      panel.spacing.x   = unit(1.2, "lines"),
      panel.spacing.y   = unit(2,   "lines"),
      panel.grid.major  = element_line(color = "grey65", size = 0.15),
      panel.grid.minor  = element_line(color = "grey85", size = 0.10)
    )
  
  #  X-axis strip (shared, shown only at the bottom)
  x_axis <- ggplot(full_data, aes(x = date)) +
    facet_grid(cols = vars(period)) +
    scale_x_date(
      date_labels = "%b %d",
      breaks      = seq(min(full_data$date), max(full_data$date), by = "3 days"),
      expand      = c(0, 2)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x        = element_text(angle = 0, hjust = 0.5, size = 18),
      axis.title         = element_blank(),
      panel.grid         = element_blank(),
      panel.background   = element_blank(),
      strip.text         = element_blank(),
      axis.ticks.length  = unit(0, "pt"),
      axis.line          = element_blank()
    )
  
  # Combine main plot and x-axis using patchwork
  p_main <- p_main + theme(plot.margin = margin(5, 5, 0, 5))
  x_axis <- x_axis + theme(plot.margin = margin(0, 5, 5, 5))
  
  final_plot <- (p_main / x_axis) + plot_layout(heights = c(25, 0))
  
  print(final_plot)
  
  # Save to output directory 
  output_file <- file.path(output_dir, "combined_timeseries_plot_two_way_individual.png")
  ggsave(
    filename = output_file,
    plot     = final_plot,
    width    = 22,
    height   = 16,
    dpi      = 300,
    device   = "png"
  )
}


## =============================================================================
# Function 6: Create a visual summary combining plots according to heatwave status (variables with only single daily measurement).
# Individual participant trajectories are shown as thin grey lines behind the mean ± CI.
time_series_combine_plot_one_way_individual <- function(data_list,
                                             outcome_variables,
                                             output_dir) {
  
  full_data       <- data.frame() # Will store summarised data for all outcomes
  full_individual <- data.frame() # Will store individual-level data for all outcomes
  
  for (outcome_name in names(outcome_variables)) {
    
    # Extract dataset and variable names for this outcome 
    data        <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var    <- outcome_variables[[outcome_name]]$date
    
    # Skip outcome if variable is absent from the dataset
    if (!outcome_var %in% names(data)) next
    
    # Filter and format 
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        period = factor(period,
                        levels = c("AM", "PM", "NI"),
                        labels = c("", "Afternoon", "Night")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
                                 labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave"))
      )
   
    
    # Wrap outcome name for facet label 
    outcome_order_raw     <- c("Daily change\nBody Mass (g)",
                               "Urine Frequency per Day",
                               "Mean Daily Acceleration (mg)")
    outcome_order_wrapped <- str_wrap(outcome_order_raw, width = 20)
    wrapped_outcome_name  <- str_wrap(outcome_name, width = 20)
    
    # Summarise: mean, SD, CI per date × period × heatwave_status
    summarised_data <- data %>%
      group_by(!!sym(time_var), period, heatwave_status) %>%
      summarise(
        mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
        sd_value   = sd(!!sym(outcome_var),   na.rm = TRUE),
        min_value  = min(!!sym(outcome_var),   na.rm = TRUE),
        max_value  = max(!!sym(outcome_var),   na.rm = TRUE),
        n          = n(),
        lower_CI   = mean_value - 1.96 * (sd_value / sqrt(n)),
        upper_CI   = mean_value + 1.96 * (sd_value / sqrt(n)),
        .groups    = "drop"
      ) %>%
      mutate(
        outcome_name = factor(wrapped_outcome_name, levels = outcome_order_wrapped),
        date         = !!sym(time_var)
      )
    
    # Individual-level data (one row per participant × date × period)
    individual_data <- data %>%
      filter(!is.na(!!sym(outcome_var))) %>%
      mutate(
        outcome_name = factor(wrapped_outcome_name, levels = outcome_order_wrapped),
        date         = !!sym(time_var),
        raw_value    = !!sym(outcome_var)
      ) %>%
      select(participant_id, date, period, heatwave_status, outcome_name, raw_value)
    
    # Append to full datasets
    full_data       <- bind_rows(full_data,       summarised_data)
    full_individual <- bind_rows(full_individual, individual_data)
  }
  
  # Heatwave boundary dates 
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end   <- as.Date("2024-06-20")
  
  # Main plot 
  p_main <- ggplot(full_data,
                   aes(x = date, y = mean_value,
                       fill = heatwave_status, color = heatwave_status)) +
    
    # 1) Individual participant trajectories thin grey lines per participant × period × phase
    geom_line(
      data        = full_individual,
      aes(x     = date,
          y     = raw_value,
          group = interaction(participant_id, period, heatwave_status)),
      color       = "grey50",
      alpha       = 0.3,
      size        = 0.4,
      inherit.aes = FALSE
    ) +
    
    # 2) 95% confidence interval ribbon
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                alpha = 0.4, color = NA) +
    
    # 3) ±1 SD ribbon (grey, behind the coloured CI)
    geom_ribbon(aes(ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value),
                alpha = 0.25, fill = "grey80", color = NA) +
    
    # 4) Mean line per heatwave phase
    geom_line(size = 1.5, linetype = "solid", alpha = 1) +
    
    
    scale_fill_viridis_d(name = NULL) +
    scale_color_viridis_d(name = NULL) +
    
    # Facet: outcomes in rows, periods of day in columns
    facet_grid(rows   = vars(outcome_name),
               cols   = vars(period),
               scales = "free_y",
               switch = "y") +
    
    scale_y_continuous(position = "left") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 18) +
    theme(
      # X-axis hidden (shown via separate x_axis plot below)
      axis.text.x  = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      # Y-axis
      axis.text.y.left  = element_text(size = 26),
      axis.text.y.right = element_blank(),
      axis.title.y      = element_blank(),
      
      # Facet strip labels
      strip.text.y.left = element_text(size = 26, angle = 90, vjust = 0.5, hjust = 0.5),
      strip.text.x.top  = element_text(size = 24),
      strip.placement   = "outside",
      
      # Legend
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.text       = element_text(size = 28),
      legend.key.size   = unit(1.5, "cm"),
      legend.background = element_rect(fill = "white", color = "grey50",
                                       size = 0.5, linetype = "solid"),
      legend.margin     = margin(10, 15, 10, 15),
      
      # Panel spacing and grid
      panel.spacing.x  = unit(1.2, "lines"),
      panel.spacing.y  = unit(2,   "lines"),
      panel.grid.major = element_line(color = "grey65", size = 0.15),
      panel.grid.minor = element_line(color = "grey85", size = 0.10)
    )
  
  # X-axis strip (shared, shown only at the bottom)
  x_axis <- ggplot(full_data, aes(x = date)) +
    facet_grid(cols = vars(period)) +
    scale_x_date(
      date_labels = "%b %d",
      breaks      = seq(min(full_data$date), max(full_data$date), by = "3 days"),
      expand      = c(0, 1)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 24),
      axis.title        = element_blank(),
      panel.grid        = element_blank(),
      panel.background  = element_blank(),
      strip.text        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.line         = element_blank()
    )
  
  # Combine main plot and x-axis using patchwork 
  p_main <- p_main + theme(plot.margin = margin(5, 5, 0, 5))
  x_axis <- x_axis + theme(plot.margin = margin(0, 5, 5, 5))
  
  final_plot <- (p_main / x_axis) +
    plot_layout(heights = c(25, 0))
  
  print(final_plot)
  
  #  Save to output directory
  output_file <- file.path(output_dir, "combined_timeseries_plot_one_way_individual.png")
  ggsave(
    filename = output_file,
    plot     = final_plot,
    width    = 22,
    height   = 16,
    dpi      = 300,
    device   = "png"
  )
}







## =============================================================================
# Function 7: Create a visual summary of indoor environmental conditions over time, grouped by heatwave status and period of day.
# Individual apartment/sensor trajectories are shown as thin grey lines behind the mean ± CI.
time_series_combine_plot_environment_individual <- function(data_list,
                                                 outcome_variables,
                                                 output_dir) {
  
  full_data       <- data.frame() # Will store summarised data for all outcomes
  full_individual <- data.frame() # Will store individual-level data for all outcomes
  
  for (outcome_name in names(outcome_variables)) {
    
    # Extract dataset and variable names for this outcome 
    data        <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var    <- outcome_variables[[outcome_name]]$date
    
    # Skip outcome if variable is absent from the dataset
    if (!outcome_var %in% names(data)) next
    
    # Filter and format 
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        period = factor(period,
                        levels = c("AM", "PM", "NI"),
                        labels = c("", "Afternoon", "Night")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
                                 labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave"))
      )
    
    # Wrap outcome name for facet label 
    outcome_order_raw <- c(
      "Indoor Average Temperature (°C)",
      "Indoor Minimal Temperature (°C)",
      "Indoor Maximal Temperature (°C)",
      "Indoor Average Relative Humidity (%)"
    )
    outcome_order_wrapped <- str_wrap(outcome_order_raw, width = 20)
    wrapped_outcome_name  <- str_wrap(outcome_name, width = 20)
    
    # Summarise: mean, SD, CI per date × period × heatwave_status
    summarised_data <- data %>%
      group_by(!!sym(time_var), period, heatwave_status) %>%
      summarise(
        mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
        sd_value   = sd(!!sym(outcome_var),   na.rm = TRUE),
        min_value  = min(!!sym(outcome_var),   na.rm = TRUE),
        max_value  = max(!!sym(outcome_var),   na.rm = TRUE),
        n          = n(),
        lower_CI   = mean_value - 1.96 * (sd_value / sqrt(n)),
        upper_CI   = mean_value + 1.96 * (sd_value / sqrt(n)),
        .groups    = "drop"
      ) %>%
      mutate(
        outcome_name = factor(wrapped_outcome_name, levels = outcome_order_wrapped),
        date         = !!sym(time_var)
      )
    
    # Individual-level data (one row per sensor/apartment × date × period) 
    individual_data <- data %>%
      filter(!is.na(!!sym(outcome_var))) %>%
      mutate(
        outcome_name = factor(wrapped_outcome_name, levels = outcome_order_wrapped),
        date         = !!sym(time_var),
        raw_value    = !!sym(outcome_var)
      ) %>%
      select(participant_id, date, period, heatwave_status, outcome_name, raw_value)
    
    #  Append to full datasets
    full_data       <- bind_rows(full_data,       summarised_data)
    full_individual <- bind_rows(full_individual, individual_data)
  }
  
  # Heatwave boundary dates 
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end   <- as.Date("2024-06-20")
  
  # Main plot 
  p_main <- ggplot(full_data,
                   aes(x = date, y = mean_value,
                       fill = heatwave_status, color = heatwave_status)) +
    
    # 1) Individual sensor/apartment trajectories — thin grey lines per unit × period × phase
    geom_line(
      data        = full_individual,
      aes(x     = date,
          y     = raw_value,
          group = interaction(participant_id, period, heatwave_status)),
      color       = "grey50",
      alpha       = 0.4,
      linewidth   = 0.5,
      inherit.aes = FALSE
    ) +
    # 2) 95% confidence interval ribbon
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                alpha = 0.4, color = NA) +
    
    # 3) ±1 SD ribbon (grey, behind the coloured CI)
    geom_ribbon(aes(ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value),
                alpha = 0.25, fill = "grey80", color = NA) +
    
    # 4) Mean line per heatwave phase
    geom_line(linewidth = 1.5, linetype = "solid", alpha = 1) +
    
    scale_fill_viridis_d(name = NULL) +
    scale_color_viridis_d(name = NULL) +
    
    # Facet: outcomes in rows, periods of day in columns
    facet_grid(rows   = vars(outcome_name),
               cols   = vars(period),
               scales = "free_y",
               switch = "y") +
    
    scale_y_continuous(position = "left",
                       labels = scales::number_format(accuracy = 1)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 18) +
    theme(
      # X-axis hidden (shown via separate x_axis plot below)
      axis.text.x  = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      # Y-axis
      axis.text.y.left  = element_text(size = 26),
      axis.text.y.right = element_blank(),
      axis.title.y      = element_blank(),
      
      # Facet strip labels
      strip.text.y.left = element_text(size = 26, angle = 90, vjust = 0.5, hjust = 0.5),
      strip.text.x.top  = element_text(size = 24),
      strip.placement   = "outside",
      
      # Legend
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.text       = element_text(size = 28),
      legend.key.size   = unit(1.5, "cm"),
      legend.background = element_rect(fill = "white", color = "grey50",
                                       linewidth = 0.5, linetype = "solid"),
      legend.margin     = margin(10, 15, 10, 15),
      
      # Panel spacing and grid
      panel.spacing.x  = unit(1.2, "lines"),
      panel.spacing.y  = unit(2,   "lines"),
      panel.grid.major = element_line(color = "grey65", linewidth = 0.15),
      panel.grid.minor = element_line(color = "grey85", linewidth = 0.10)
    )
  
  # X-axis strip (shared, shown only at the bottom)
  x_axis <- ggplot(full_data, aes(x = date)) +
    facet_grid(cols = vars(period)) +
    scale_x_date(
      date_labels = "%b %d",
      breaks      = seq(min(full_data$date), max(full_data$date), by = "3 days"),
      expand      = c(0, 1)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 24),
      axis.title        = element_blank(),
      panel.grid        = element_blank(),
      panel.background  = element_blank(),
      strip.text        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.line         = element_blank()
    )
  
  # Combine main plot and x-axis using patchwork 
  final_plot <- (p_main / x_axis) + plot_layout(heights = c(25, 1))
  print(final_plot)
  
  # Save to output directory
  # Note: filename uses "environment" as a fixed label since all outcomes
  # belong to the same environmental
  output_file <- file.path(output_dir, "combined_timeseries_plot_environment_individual.png")
  ggsave(
    filename = output_file,
    plot     = final_plot,
    width    = 22,
    height   = 16,
    dpi      = 300,
    device   = "png"
  )
}


## =============================================================================
# Function 8:Create 

time_series_environment_by_phase <- function(data_list,
                                             outcome_variables,
                                             output_dir) {
  
  # ── Configuration des phases ─────────────────────────────────
  phase_config <- list(
    list(hw_code = "BEF_HW_June", label = "Pre-Heatwave",    color = "#440154FF",
         start = as.POSIXct("2024-06-11 00:00:00", tz = "UTC"),
         end   = as.POSIXct("2024-06-17 23:59:59", tz = "UTC"),
         break_hrs = c(0, 6, 12, 18), prefix = "pre"),
    list(hw_code = "HW_June",     label = "During Heatwave", color = "#2BB07FFF",
         start = as.POSIXct("2024-06-18 00:00:00", tz = "UTC"), end = as.POSIXct("2024-06-20 23:59:59", tz = "UTC"),
         break_hrs = seq(0, 23, by = 3), prefix = "during"),
    list(hw_code = "AFT_HW_June", label = "Post-Heatwave",   color = "#CBDF25",
         start = as.POSIXct("2024-06-21 00:00:00", tz = "UTC"), end = as.POSIXct("2024-06-27 23:59:59", tz = "UTC"),
         break_hrs = c(0, 6, 12, 18), prefix = "post")
  )
  
  
  for (phase in phase_config) {
    for (outcome_name in names(outcome_variables)) {
      
      data        <- data_list[[outcome_variables[[outcome_name]]$data]]
      outcome_var <- outcome_variables[[outcome_name]]$var
      time_var    <- outcome_variables[[outcome_name]]$date
      
      if (!outcome_var %in% names(data)) next
      
      # ── Filtre et format ───────────────────────────────────────
      data <- data %>%
        mutate(
          heatwave_status = case_when(
            as.character(heatwave_status) %in% c("BEF_HW_June", "Pre-Heatwave")  ~ "BEF_HW_June",
            as.character(heatwave_status) %in% c("HW_June", "During Heatwave")   ~ "HW_June",
            as.character(heatwave_status) %in% c("AFT_HW_June", "Post-Heatwave") ~ "AFT_HW_June",
            TRUE ~ as.character(heatwave_status)
          )
        ) %>%
        filter(heatwave_status == phase$hw_code) %>%
        mutate(
          !!sym(time_var) := as.POSIXct(!!sym(time_var), tz="UTC") 
        )%>%
        filter(!!sym(time_var) >= phase$start & 
                 !!sym(time_var) <= phase$end)  # ← filtre strict sur les dates
      
      if (nrow(data) == 0) next
      # ── Individual data ────────────────────────────────────────
      # ← MODIFICATION : vérifier si participant_id existe
      
      # ── Hours elapsed ──────────────────────────────────────────
      origin_date <- as.POSIXct(phase$start, tz = "UTC")
      
      
      has_individuals <- "participant_id" %in% names(data) & 
        n_distinct(data$participant_id) > 1
      
      if (has_individuals) {
        individual_data <- data %>%
          mutate(
            date          = !!sym(time_var),
            raw_value     = !!sym(outcome_var),
            hours_elapsed = as.numeric(difftime(!!sym(time_var), origin_date, units = "hours"))
          ) %>%
          select(participant_id, date, raw_value, hours_elapsed) %>%
          arrange(participant_id, hours_elapsed) %>%
          group_by(participant_id) %>%
          mutate(
            gap        = hours_elapsed - lag(hours_elapsed, default = first(hours_elapsed)),
            # Nouveau segment à chaque fois que le gap est > 1h OU que la valeur est NA
            segment_id = cumsum(gap > 5 | is.na(raw_value)),
            group_id   = paste(participant_id, segment_id, sep = "_")
          ) %>%
          ungroup() %>%
          filter(!is.na(raw_value))  # ← retire les NA après avoir créé les segments
      }
      # ── Summarise ──────────────────────────────────────────────
      summarised_data <- data %>%
        group_by(!!sym(time_var)) %>%
        summarise(
          mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
          sd_value   = sd(!!sym(outcome_var),   na.rm = TRUE),
          n          = sum(!is.na(!!sym(outcome_var))),
          lower_CI   = mean_value - 1.96 * (sd_value / sqrt(n)),
          upper_CI   = mean_value + 1.96 * (sd_value / sqrt(n)),
          .groups    = "drop"
        ) %>%
        mutate(date = !!sym(time_var),
               # ← Masquer les heures avec moins de 5 participants
               mean_value = ifelse(has_individuals & n < 5, NA_real_, mean_value),
               lower_CI   = ifelse(has_individuals & n < 5, NA_real_, lower_CI),
               upper_CI   = ifelse(has_individuals & n < 5, NA_real_, upper_CI),
               sd_value   = ifelse(has_individuals & n < 5, NA_real_, sd_value)
        )
      

      # ── Hours elapsed ──────────────────────────────────────────
      origin_date <- as.POSIXct(phase$start, tz = "UTC")
      
      summarised_data <- summarised_data %>%
        mutate(hours_elapsed = as.numeric(difftime(date, origin_date, units = "hours")))
      
      # ── Interpolation NA — APRÈS hours_elapsed ────────────────
 
      
      
      # ── X breaks ──────────────────────────────────────────────
      total_hours <- as.numeric(difftime(phase$end, phase$start, units = "hours"))
      n_days      <- ceiling(total_hours / 24)
      x_breaks    <- unlist(lapply(0:(n_days - 1), function(d) d * 24 + phase$break_hrs))
      x_breaks    <- x_breaks[x_breaks <= total_hours]
      x_labels    <- format(origin_date + lubridate::hours(x_breaks), "%b %d\n%H:%M")
      
      
      
      # ── Anchor points : moyenne globale des phases précédentes ────
      anchor_colors <- list(
        BEF_HW_June = "#440154FF",
        HW_June     = "#2BB07FFF",
        AFT_HW_June = "#CBDF25"
      )
      
      anchor_data <- data.frame()
      
      if (phase$hw_code == "HW_June") {
        # Pour During HW → un point = moyenne Pre-HW
        pre_data <- data_list[[outcome_variables[[outcome_name]]$data]] %>%
          filter(as.character(heatwave_status) %in% c("BEF_HW_June", "Pre-Heatwave")) %>%
          summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
          mutate(hours_elapsed = -1,   # ← juste avant le début
                 color         = anchor_colors$BEF_HW_June)
        anchor_data <- bind_rows(anchor_data, pre_data)
        
      } else if (phase$hw_code == "AFT_HW_June") {
        # Pour Post HW → point Pre-HW + point During HW
        pre_data <- data_list[[outcome_variables[[outcome_name]]$data]] %>%
          filter(as.character(heatwave_status) %in% c("BEF_HW_June", "Pre-Heatwave")) %>%
          summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
          mutate(hours_elapsed = -2,
                 color         = anchor_colors$BEF_HW_June)
        
        hw_data <- data_list[[outcome_variables[[outcome_name]]$data]] %>%
          filter(as.character(heatwave_status) %in% c("HW_June", "During Heatwave")) %>%
          summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
          mutate(hours_elapsed = -1,
                 color         = anchor_colors$HW_June)
        
        anchor_data <- bind_rows(anchor_data, pre_data, hw_data)
      }

      # ── Plot ───────────────────────────────────────────────────────
      p <- ggplot(summarised_data, aes(x = hours_elapsed, y = mean_value)) +
        
        geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                    alpha = 0.5, fill = phase$color, color = NA) +
        
        { if (has_individuals)
          geom_line(
            data        = individual_data,
            aes(x = hours_elapsed, y = raw_value, group = group_id),  # ← group_id
            color       = "grey40", alpha = 0.3, linewidth = 0.6,
            inherit.aes = FALSE
          )
        } +
        geom_line(linewidth = 2, color = phase$color) +
        
        # ← Anchor points
        { if (nrow(anchor_data) > 0)
          geom_point(
            data        = anchor_data,
            aes(x = hours_elapsed, y = mean_value),
            color       = anchor_data$color,
            size        = 4,
            shape       = 18,   # ← losange
            inherit.aes = FALSE
          )
        } +
        
        # Ligne pointillée reliant anchor au début de la phase
        { if (nrow(anchor_data) > 0)
          geom_segment(
            data = anchor_data,
            aes(x    = hours_elapsed,
                xend = 0,
                y    = mean_value,
                yend = summarised_data$mean_value[which.min(summarised_data$hours_elapsed)]),
            color    = anchor_data$color,
            linetype = "dashed",
            linewidth = 0.8,
            inherit.aes = FALSE
          )
        } +
        
        scale_x_continuous(
          breaks = x_breaks,
          labels = x_labels,
          expand = c(0.05, 0.05)  # ← espace supplémentaire à gauche pour les anchors
        ) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
        labs(x = NULL, y = outcome_name,
             title = paste(phase$label, "—", outcome_name)) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 10),
          axis.ticks.x     = element_line(color = "grey60"),
          axis.text.y      = element_text(size = 11),
          axis.title.y     = element_text(size = 11, angle = 90),
          plot.title       = element_text(size = 13, face = "bold", color = phase$color),
          panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
          panel.grid.minor = element_blank(),
          plot.margin      = margin(5, 10, 5, 10)
        )
      
      print(p)
      
      clean_name  <- gsub("[^A-Za-z0-9]", "_", outcome_name)
      output_file <- file.path(output_dir,
                               paste0(phase$prefix, "_", clean_name, ".png"))
      ggsave(filename = output_file, plot = p,
             width = 22, height = 4, dpi = 300, device = "png")
      cat("✅ Saved:", basename(output_file), "\n")
    }
  }
}

## =============================================================================
# Function 9 :Create a visual summary combining plots of responses according to A/C Using
time_series_heatwave_AC <- function(data_list,
                                    outcome_variables,
                                    group_by = c("heatwave_status"),
                                    output_dir) {
  
  group_by <- match.arg(group_by) 
  plot_list<-list()
  full_data <- data.frame()  
  
  for (outcome_name in names(outcome_variables)) {
    # Extracting column and data
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    time_var <- outcome_variables[[outcome_name]]$date
    
    if (!outcome_var %in% names(data)) next
    
    
    # Prepare dataset for plotting
    data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        !!sym(time_var) := as.Date(!!sym(time_var)),
        
        heatwave_status = factor(heatwave_status, 
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"),
                                 labels = c("Pre-Heatwave", "During Heatwave", "Post-Heatwave")),
        AC = as.factor(AC),  # convert AC as factor for labeling
        AC_label = ifelse(AC == "1", "AC", "No AC")  # Create AC label for plotting
      )
    
    
    # Create combined factor for heatwave status and AC for plotting
    data$heatwave_AC <- factor(
      paste(data$heatwave_status, data$AC_label, sep = " - "),
      levels = c(
        "Pre-Heatwave - AC", "Pre-Heatwave - No AC", "During Heatwave - AC", "During Heatwave - No AC","Post-Heatwave - AC","Post-Heatwave - No AC"
      )
    )
    # Summary statistics: mean, SD, CI per date × heatwave_status × AC_label
    if (group_by == "heatwave_status") {
      summarised_data <- data %>%
        group_by(!!sym(time_var), heatwave_status, AC_label) %>%
        summarise(
          mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
          sd_value = sd(!!sym(outcome_var), na.rm = TRUE),
          n = n(),
          lower_CI = mean_value - 1.96 * (sd_value / sqrt(n)),
          upper_CI = mean_value + 1.96 * (sd_value / sqrt(n)),
          .groups = "drop"
        )
    } else { # group_by period
      summarised_data <- data %>%
        group_by(!!sym(time_var), period, AC_label) %>%
        summarise(
          mean_value = mean(!!sym(outcome_var), na.rm = TRUE),
          sd_value = sd(!!sym(outcome_var), na.rm = TRUE),
          n = n(),
          lower_CI = mean_value - 1.96 * (sd_value / sqrt(n)),
          upper_CI = mean_value + 1.96 * (sd_value / sqrt(n)),
          .groups = "drop"
        )
    }
    
    
    summarised_data <- summarised_data %>%
      mutate(date = !!sym(time_var),
             heatwave_AC = factor(
               paste(heatwave_status, AC_label, sep = " - "),
               levels = c(
                 "Pre-Heatwave - AC", "Pre-Heatwave - No AC", "During Heatwave - AC", "During Heatwave - No AC","Post-Heatwave - AC","Post-Heatwave - No AC"
               )
             )
      )
    summarised_data$outcome_name <- factor(
      str_wrap(outcome_name, width = 20),
      levels = str_wrap(names(outcome_variables), width = 20)
    )
    full_data <- bind_rows(full_data, summarised_data)
    
  } 
  
  # Heatwave boundary dates (for potential annotation in plots)
  heatwave_start <- as.Date("2024-06-18")
  heatwave_end <- as.Date("2024-06-20")
  
  
  if (group_by == "heatwave_status") {
    p_main <- ggplot(full_data,
                     aes(x     = date,
                         y     = mean_value,
                         color = heatwave_AC,
                         fill  = heatwave_AC)) +
      
      geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                  alpha = 0.2, color = NA) +
      geom_line(linewidth = 1.5) +
      
      scale_fill_viridis_d(name = NULL) +
      scale_color_viridis_d(name = NULL) +
      
      facet_grid(
        rows   = vars(outcome_name),
        scales = "free_y",
        switch = "y"
      ) +
      
      scale_y_continuous(position = "left",
                         labels = scales::number_format(accuracy = 1)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 18) +
      theme(
        axis.text.x        = element_blank(),
        axis.title.x       = element_blank(),
        axis.ticks.x       = element_blank(),
        axis.text.y.left   = element_text(size = 26),
        axis.text.y.right  = element_blank(),
        axis.title.y.left  = element_blank(),
        axis.title.y.right = element_blank(),
        strip.text.y.left  = element_text(size = 26, angle = 90,
                                          vjust = 0.5, hjust = 0.5),
        strip.placement    = "outside",
        legend.position    = "top",
        legend.direction   = "horizontal",
        legend.text        = element_text(size = 28),
        legend.key.size    = unit(1.5, "cm"),
        legend.background  = element_rect(fill = "white", color = "grey50",
                                          linewidth = 0.5, linetype = "solid"),
        legend.margin      = margin(10, 15, 10, 15),
        panel.spacing.y    = unit(2, "lines"),
        panel.grid.major   = element_line(color = "grey65", linewidth = 0.15),
        panel.grid.minor   = element_line(color = "grey85", linewidth = 0.10)
      )
    
    # Shared X-axis strip (shown only at the bottom)
    x_axis <- ggplot(full_data, aes(x = date)) +
      scale_x_date(
        date_labels = "%b %d",
        breaks      = seq(min(full_data$date), max(full_data$date), by = "3 days"),
        expand      = c(0, 1)
      ) +
      theme_minimal(base_size = 18) +
      theme(
        axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.title        = element_blank(),
        panel.grid        = element_blank(),
        panel.background  = element_blank(),
        strip.text        = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.line         = element_blank()
      )
    
    # Combine plots using patchwork
    final_plot <- p_main / x_axis
    final_plot <- final_plot + plot_layout(heights = c(25, 1))
    print(final_plot)
    
    
    # Save to output directory
    output_file <- file.path(output_dir, "combined_plot_AC.png")
    ggsave(
      filename = output_file,
      plot     = final_plot,
      width    = 22,
      height   = 16,
      dpi      = 300,
      device   = "png"
    )
  }
}


## =============================================================================
# Function 10: Create a violon plot for ordinal variables, grouped by heatwave status and/or period of day. 

violin_plot_ordinal <- function(data_list, 
                                outcome_variables, 
                                plot_type = "period") #either "period" or "heatwave", specifying the x-axis grouping. 
                                {
  
  # Define how to map x and fill aesthetics for different plot types                              
  plot_types <- list(
    period = list(x = "period", fill = "period"),
    heatwave = list(x = "heatwave_status", fill = "heatwave_status")
  )
  
  # Check for valid plot_type input
  if (!(plot_type %in% names(plot_types)))
    stop("Invalid plot_type. Choose from: period, heatwave")
  
  # Create a list of plots, one for each outcome variable
  results_list <- lapply(names(outcome_variables), function(outcome_name) {
    
    # Retrieve the corresponding data frame and variables 
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    resp_var <- outcome_variables[[outcome_name]]$resp
    
    # Skip if the outcome variable is not in the dataset
    if (!outcome_var %in% names(data))
      return(NULL)
    
    # Filter and prepare data
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>% # Focus on relevant heatwave categories
      mutate(
        period = factor(period, levels = c("AM", "PM", "NI")),# Ensure consistent order
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
        ),
        
        # Convert response variable to a factor with unique levels (preserves ordering)
        resp_factor = factor(.data[[resp_var]], levels = unique(.data[[resp_var]]))  
      )
    # Create the violin plot
    plot <- ggplot(data,
                   aes(
                     x = .data[[plot_types[[plot_type]]$x]], # dynamic x-axis
                     y = .data[[outcome_var]],# outcome variable (y-axis)
                     fill = .data[[plot_types[[plot_type]]$fill]] # fill based on grouping
                   )
                   ) +
      geom_violin(alpha = 0.7, trim = FALSE) + # Show full distribution
      geom_jitter(
        aes(color = as.factor(participant_id)),# Show individual participant IDs
        width = 0.2,
        size = 1,
        alpha = 0.6
      ) +
      scale_fill_viridis_d(option = "D") + # Use viridis color scale for better readability
      labs(
        title = paste("Violin plot Summary of", outcome_name),
        x = plot_types[[plot_type]]$x,
        y = resp_var,
        color = "participant_id"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
            legend.position = "none") # Hide legend for participant IDs
    
    return(plot)
  })
  # Combine plots into a 2-column layout
  wrap_plots(results_list, ncol = 2)
} 
  



## =============================================================================
# Function 11:  Generate proportional bar charts for categorical/ordinal variables by time period or heatwave status

chart_categorical <- function(data_list,
                              outcome_variables,
                              chart_type = "period") {
  
  # Set the grouping variable for x and fill based on the chart_type
  chart_settings <- list(
    period = list(x = "period", fill = "period"),
    heatwave = list(x = "heatwave_status", fill = "heatwave_status")
  )
  
  # Validate the input chart_type
  if (!(chart_type %in% names(chart_settings)))
    stop("Invalid chart_type. Choose from: period, heatwave")
  
  # Create a list of plots for each outcome variable
  results_list <- lapply(names(outcome_variables), function(outcome_name) {
    
    # Extract dataset and variable names from outcome_variables
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    resp_var <- outcome_variables[[outcome_name]]$resp
    
    # Skip if outcome variable is not in the dataset
    if (!outcome_var %in% names(data))
      return(NULL)
    
    
    # Filter dataset to only include relevant heatwave periods
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        period = factor(period, levels = c("AM", "PM", "NI")),# Order time of day
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")# Order heatwave categories
        )
      )
    
    # Build the proportional stacked bar plot
    plot <- ggplot(data, aes(x = .data[[chart_settings[[chart_type]]$x]], # X-axis grouping (period or heatwave_status)
                             fill = .data[[resp_var]])) + # Fill bars by response variable (categorical)
      geom_bar(position = "fill", width = 0.7) + # Use "fill" to show proportions (0–1)
      scale_fill_viridis_d(option = "D") + # Use color-blind friendly palette
      labs(
        title = paste("Bar Chart Summary of", outcome_name),
        x = chart_settings[[chart_type]]$x, # Dynamic x-axis label
        y = outcome_name, # y-axis label from outcome definition
        fill = "Response" # Legend title
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1), # Tilt x-axis labels for readability
        legend.position = "bottom",# Move legend below the chart
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    return(plot)
  })
  
  # Combine all plots into a 2-column layout using patchwork
  combined_plot <- wrap_plots(results_list, ncol = 2) 
  
  return(combined_plot)
}


  

## =============================================================================
# Function 12:  Bar chart for categorical data with faceting

bar_chart_plot <- function(data_list, outcome_variables, plot_type = "heatwave_period")
{
  # Define which variable is on x-axis and which to facet by based on plot_type
  plot_settings <- list(
    heatwave_period = list(x = "heatwave_status", y = "period"),
    period_heatwave = list(x = "period", y = "heatwave_status")
  )
  
  # Validate the plot_type input
  if (!(plot_type %in% names(plot_settings)))
    stop("Invalid plot_type. Choose from: heatwave_period, period_heatwave")
 
  # Generate a plot for each outcome variable
  results_list <- lapply(names(outcome_variables), function(outcome_name) {
    
    # Extract relevant dataset and variables from input
    data <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    resp_var <- outcome_variables[[outcome_name]]$resp
    
    # Skip if the expected outcome variable doesn't exist in the dataset
    if (!outcome_var %in% names(data))
      return(NULL)
    
    # Filter and prepare the data
    data <- data %>%
      filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
      mutate(
        period = factor(period, levels = c("AM", "PM", "NI")),
        heatwave_status = factor(
          heatwave_status,
          levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
        )
      )
    
    
    # Build a proportional bar chart using ggplot2
    ggplot(data, aes(
      x = !!sym(plot_settings[[plot_type]]$x),
      fill = !!sym(resp_var)
    )) +
      geom_bar(position = "fill", width = 0.7) +  # Proportional stacked bars
      facet_wrap( ~ get(plot_settings[[plot_type]]$y)) +  # Facet by the complementary variable
      scale_fill_viridis_d(option = "D") + # Color-blind friendly palette
      labs(
        title = paste(
          "Distribution of Responses by",
          outcome_name,
          "according to",
          plot_settings[[plot_type]]$x,
          "and",
          plot_settings[[plot_type]]$y
        ),
        x = plot_settings[[plot_type]]$x,
        y = "Response Proportion",
        fill = "Response"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # Combine all generated plots into a grid using patchwork
  wrap_plots(results_list, ncol = 2)
}



## =============================================================================
# Function 13: Heatmap for numeric outcome variables 

 heatmap_plot<- function(data_list, 
                         outcome_variables, 
                         plot_type = "heatwave_period") 
                        {
   
   # Define x and y-axis mappings based on chosen plot type
   plot_settings <- list(
     heatwave_period = list(x = "heatwave_status", y = "period"),
     period_heatwave = list(x = "period", y = "heatwave_status")
   )
   
   # Validate the input plot type
   if (!(plot_type %in% names(plot_settings))) stop("Invalid plot_type. Choose from: heatwave_period, period_heatwave")
   
   # Loop over all outcome variables to create individual heatmaps
   results_list <- lapply(names(outcome_variables), function(outcome_name) {
     # Extract the dataset and variable name for this outcome
     data <- data_list[[outcome_variables[[outcome_name]]$data]]
     outcome_var <- outcome_variables[[outcome_name]]$var
     
     # Skip if the outcome variable is not present in the dataset
     if (!outcome_var %in% names(data)) return(NULL)
     
     
     # Preprocess the data:
     # - Filter to only relevant heatwave statuses
     # - Ensure period and heatwave_status are factors with correct order
     # - Convert date for daily counts
     data <- data %>%
       filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
       mutate(
         period = factor(period, levels = c("AM", "PM", "NI")),
         heatwave_status = factor(heatwave_status, levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
         date = as.Date(date_converted)  # Convert to Date for daily analysis
       )
     
     # Group data by the selected x and y variables
     # Calculate summary statistics per group
     data_grouped <- data %>%
       group_by(!!sym(plot_settings[[plot_type]]$x), !!sym(plot_settings[[plot_type]]$y)) %>%
       summarise(
         mean_value = mean(!!sym(outcome_var), na.rm = TRUE),# Mean of the outcome
         median_value = median(!!sym(outcome_var), na.rm = TRUE),# Median (not used in plot)
         n = n(),# Total number of observations
         n_days = n_distinct(date),# Number of unique days
         n_per_day = round(n / n_days, 1),# Observations per day
         .groups = "drop")
     
     
     # Create the heatmap using ggplot2
     ggplot(data_grouped, aes(x = .data[[plot_settings[[plot_type]]$x]], 
                                     y = .data[[plot_settings[[plot_type]]$y]], 
                                     fill = .data[["mean_value"]])) +
       geom_tile(color = "white") + # Color tile per mean value
       geom_text(aes(label = n_per_day), color = "white", size = 4) + # Add text: average obs per day
       scale_fill_viridis_c(option = "D", name = "Mean") +  # Continuous Viridis color scale
       labs(
         title = paste("Heatmap of", outcome_name),
         x = plot_settings[[plot_type]]$x,
         y = plot_settings[[plot_type]]$y
       ) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels
   })
   # Combine all individual heatmaps into a single grid
   wrap_plots(results_list, ncol = 2)
 }
 
 
## =============================================================================
# Function 14: Heatmap Long-format heatmap for daily trends
 
 heatmap_plot_long <- function(data_list,
                               outcome_variables,
                               output_dir,
                               plot_type = "heatwave_period") {
   
   # Validate plot_type
   plot_settings <- list(
     heatwave_period = list(x = "heatwave_status", y = "period"),
     period_heatwave = list(x = "period", y = "heatwave_status")
   )
   
   if (!(plot_type %in% names(plot_settings)))
     stop("Invalid plot_type. Choose from: heatwave_period, period_heatwave")
   
   # Loop through each outcome variable
   for (outcome_name in names(outcome_variables)) {
     
     cat("\n", strrep("=", 60), "\n")
     cat("🔹 Creating heatmap for:", outcome_name, "\n")
     cat(strrep("=", 60), "\n\n")
     
     # Retrieve dataset and variable names
     data <- data_list[[outcome_variables[[outcome_name]]$data]]
     outcome_var <- outcome_variables[[outcome_name]]$var
     resp_var <- outcome_variables[[outcome_name]]$resp
     
     # Skip if the outcome variable doesn't exist in the dataset
     if (!outcome_var %in% names(data)) {
       cat("Variable not found. Skipping.\n")
       next
     }
     
     # Preprocess data
     data <- data %>%
       filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
       mutate(
         period = factor(period, levels = c("AM", "PM", "NI"),
                         labels = c("Morning", "Afternoon", "Night")),
         heatwave_status = factor(
           heatwave_status,
           levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
         ),
         date = as.Date(date_converted)
       )
     
     # Summarize data by day and period
     data_grouped <- data %>%
       group_by(date, period) %>%
       summarise(
         mean_value = mean(!!sym(outcome_var) == 1, na.rm = TRUE), # proportion of "Yes"
         total      = sum(!is.na(!!sym(outcome_var))),  
         n_yes      = sum(!!sym(outcome_var) == 1, na.rm = TRUE),  # n respondents
         .groups    = "drop"
       )
     
     # Define the key heatwave window
     heatwave_start <- as.Date("2024-06-18")
     heatwave_end <- as.Date("2024-06-20")
     
     # Create the heatmap plot
     heatmap <- ggplot(data_grouped, aes(x = date, y = period, fill = mean_value)) +
       
       # Heatmap rendering
       geom_tile(color = "white") +
       geom_text(aes(label = n_yes), color = "white", size = 3.5)+
       
       # Delimitation of the heatwave period with vertical dashed lines
       geom_vline(
         xintercept = as.numeric(heatwave_start - 0.5),
         linetype = "dashed",
         color = "grey20",
         linewidth = 0.5
       ) +
       geom_vline(
         xintercept = as.numeric(heatwave_end + 0.5),
         linetype = "dashed",
         color = "grey20",
         linewidth = 0.5
       ) +
       
       # Annotate the heatwave phases
       annotate(
         "text",
         x = as.Date("2024-06-14"),
         y = 4,
         label = "Pre-Heatwave",
         size = 7,
         fontface = "plain",
         color = "grey20"
       ) +
       annotate(
         "text",
         x = as.Date("2024-06-19"),
         y = 4.25,
         label = "During\nHeatwave",
         size = 7,
         fontface = "plain",
         color = "grey20"  
       ) +
       annotate(
         "text",
         x = as.Date("2024-06-24"),
         y = 4,
         label = "Post-Heatwave",
         size = 7,
         fontface = "plain",
         color = "grey20"  
       ) +
       
       scale_fill_viridis_c(
         name = "",  # Empty legend title
         option = "D",
         limits = c(0, 1),
         breaks = seq(0, 1, 0.25),
         labels = scales::percent_format()
       ) +
       
       # Format x-axis
       scale_x_date(
         date_labels = "%b %d",
         breaks = seq(min(data$date), max(data$date), by = "3 days"),
         expand = c(0.01, 0.01)
       ) +
       # Elimine space between tiles and add extra space at the top for annotations
       scale_y_discrete(
         expand = expansion(add = c(0, 2))  
       ) +
       
       # Titles and themes
       labs(
         title = NULL,
         x = NULL,
         y = NULL
       ) +
       theme_minimal(base_size = 12) +
       # Forced aspect ratio to ensure squares and prevent distortion
       coord_fixed(
         ratio = 1,  #  day = 1:1 for square tiles
         clip = 'off'
       ) +
       
       theme(
         axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16,color = "grey20"),  # ← hjust = 0.5
         axis.text.y = element_text(size = 16,color = "grey20"),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         plot.title = element_blank(),
         legend.position = "right", 
         
         # Control of the legend appearance
         legend.title = element_text(size = 18,  color = "grey20"),  
         legend.text = element_text(size = 18, color = "grey20"), 
         legend.key.size = unit(0.75, "cm"),  
         legend.key.height = unit(1.5, "cm"),  
         legend.key.width = unit(1, "cm"),  
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         plot.margin = margin(
           t = 5,
           r = 5,
           b = 5,
           l = 5
         )
       )
     
     # Print the plot
     print(heatmap)
     
     # Save as PNG
     filename_png <- file.path(
       output_dir,
       paste0("heatmap_", gsub(" ", "_", outcome_name), ".png")
     )
     
     ggsave(
       file = filename_png,
       plot = heatmap,
       width = 12,
       height = 6,
       dpi = 600,
       bg = "white"
     )
     
     
     cat("Saved:\n")
     cat("  PNG:", filename_png, "\n")
     
   }
   
   cat("\n All heatmaps created\n")
   invisible(NULL)
   
 }  



## =============================================================================
# Function 15: Generate faceted heatmaps for numeric outcomes, with fixed scales across facets to allow direct comparison of temporal trends across periods and heatwave phases.

heatmap_plot_facets_fixed <- function(data_list, 
                                       outcome_variables,
                                       output_dir) {
   
   # Loop through each outcome variable
   for (outcome_name in names(outcome_variables)) {
     
     cat("\n", strrep("=", 60), "\n")
     cat("Creating faceted heatmap for:", outcome_name, "\n")
     cat(strrep("=", 60), "\n\n")
     
     # Retrieve data
     data <- data_list[[outcome_variables[[outcome_name]]$data]]
     outcome_var <- outcome_variables[[outcome_name]]$var
     levels_var <- outcome_variables[[outcome_name]]$levels
     
     if (!outcome_var %in% names(data)) {
       cat("Variable not found. Skipping.\n")
       next
     }
     
     # Pre-treatment of the data: filtering, factor conversion, date conversion
     data <- data %>%
       filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
       mutate(
         period = factor(period, levels = c("AM", "PM", "NI"),
                         labels = c("Morning", "Afternoon", "Night")),
         date = as.Date(date_converted),
         modality = factor(!!sym(outcome_var), levels = levels_var),
         heatwave_status = factor(
           heatwave_status,
           levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
         )
       )
     
     # Calculate totals per date and period for proportion calculations
     totals <- data %>%
       group_by(date, period) %>%
       summarise(total = n(), .groups = "drop")
     
     # Count per modality per date and period
     counts <- data %>%
       group_by(date, period, modality) %>%
       summarise(n_mod = n(), .groups = "drop")
     
     # Merging counts with totals to calculate proportions
     data_grouped <- counts %>%
       left_join(totals, by = c("date", "period")) %>%
       mutate(prop = n_mod / total)
     
     # Define heatwave dates
     heatwave_start <- as.Date("2024-06-18")
     heatwave_end <- as.Date("2024-06-20")
     
     # Create the faceted heatmap
     heatmap <- ggplot(data_grouped, aes(x = date, y = period, fill = prop)) +
       
       # Heatmap rendering
       geom_tile(color = "white") +
       
       # vertical lines to delimit the heatwave period
       geom_vline(
         xintercept = as.numeric(heatwave_start - 0.5),
         linetype = "dashed",
         color = "grey20",
         linewidth = 0.5
       ) +
       geom_vline(
         xintercept = as.numeric(heatwave_end + 0.5),
         linetype = "dashed",
         color = "grey20",
         linewidth = 0.5
       ) +
       
       # Faceting by modality with fixed y-axis scales for direct comparison
       facet_wrap(~modality, ncol = 1) +
       
       # Annotate the heatwave phases
       annotate(
         "text",
         x = as.Date("2024-06-14"),
         y = 4,
         label = "Pre-Heatwave",
         size = 8,
         fontface = "plain",
         color = "grey20"
       ) +
       annotate(
         "text",
         x = as.Date("2024-06-19"),
         y = 4.25,
         label = "During\nHeatwave",
         size = 8,
         fontface = "plain",
         color = "grey20"
       ) +
       annotate(
         "text",
         x = as.Date("2024-06-24"),
         y = 4,
         label = "Post-Heatwave",
         size = 8,
         fontface = "plain",
         color = "grey20"
       ) +
       
       scale_fill_viridis_c(
         name = "",
         option = "D",
         limits = c(0, 1),
         breaks = seq(0, 1, 0.25),
         labels = scales::percent_format()
       ) +
       
       # Format x-axis
       scale_x_date(
         date_labels = "%b %d",
         breaks = seq(min(data$date), max(data$date), by = "3 days"),
         expand = c(0.01, 0.01)
       ) +
       
       # Scaling y axis to ensure all facets have the same range, allowing direct comparison of temporal trends across modalities
       scale_y_discrete(
         expand = expansion(add = c(0, 2))
       ) +
       
       # Titles and themes
       labs(
         title = NULL,
         x = NULL,
         y = NULL
       ) +
       theme_minimal(base_size = 12) +
       
       # Forcing aspect ratio to maintain square tiles and prevent distortion across facets, now compatible with fixed scales
       coord_fixed(
         ratio = 1, 
         clip = 'off'
       ) +
       
       theme(
         axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "grey20"),
         axis.text.y = element_text(size = 18, color = "grey20"),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         plot.title = element_blank(),
         
         # Strips (Facet title)
         strip.text = element_text(size = 16, face = "bold", color = "grey20"),
         strip.background = element_blank(),
         
         # Legend
         legend.position = "left",
         legend.title = element_text(size = 18, color = "grey20"),
         legend.text = element_text(size = 18, color = "grey20"),
         legend.key.size = unit(0.75, "cm"),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(1, "cm"),
         
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.y = unit(1.5, "lines"),  # Espace entre facettes
         plot.margin = margin(
           t = 50,
           r = 5,
           b = 5,
           l = 5
         )
       )
     
     # Print the plot
     print(heatmap)
     
     # Save as PNG
     filename_png <- file.path(
       output_dir,
       paste0("heatmap_faceted_", gsub(" ", "_", outcome_name), ".png")
     )
     
     # adjust height based on the number of modalities to ensure all facets are visible without excessive white space
     n_modalities <- length(levels_var)
     plot_height <- 4 + (n_modalities * 3)  # 3 pouces par modalité + 4 de marge
     
     ggsave(
       file = filename_png,
       plot = heatmap,
       width = 12,
       height = plot_height,
       dpi = 600,
       bg = "white"
     )
     
     cat(" Saved:\n")
     cat("  PNG:", filename_png, "\n")
     cat("  Modalities:", n_modalities, "\n")
     cat("  Height:", plot_height, "inches\n")
   }
   
   cat("\n All faceted heatmaps created\n")
   invisible(NULL)
 }
 
## =============================================================================
# Function 16: Generate a long-format heatmap for the binary AC variable, showing daily trends across the heatwave period and AC access, with annotations for heatwave phases and counts of "Yes" responses in each cell.

 heatmap_plot_long_AC <- function(data_list,
                                  outcome_variables,
                                  output_dir) {
   
   # Loop through each outcome variable
   for (outcome_name in names(outcome_variables)) {
     
     cat("\n", strrep("=", 60), "\n")
     cat("🔹 Creating AC heatmap for:", outcome_name, "\n")
     cat(strrep("=", 60), "\n\n")
     
     # Retrieve dataset and variable names
     data        <- data_list[[outcome_variables[[outcome_name]]$data]]
     outcome_var <- outcome_variables[[outcome_name]]$var
     
     # Skip if the outcome variable doesn't exist in the dataset
     if (!outcome_var %in% names(data)) {
       cat("Variable not found. Skipping.\n")
       next
     }
     
     # Preprocess data
     data <- data %>%
       filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
       mutate(
         AC_label = factor(
           ifelse(AC == "1", "AC", "No AC"),
           levels = c("AC", "No AC")
         ),
         heatwave_status = factor(
           heatwave_status,
           levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
         ),
         date = as.Date(date_converted)
       )
     
     # Summarize data by day and AC status - binary variable (proportion of "Yes")
     data_grouped <- data %>%
       group_by(date, AC_label) %>%
       summarise(
         mean_value = mean(!!sym(outcome_var) == 1, na.rm = TRUE), # proportion of "Yes"
         total      = sum(!is.na(!!sym(outcome_var))),              # n respondents
         n_yes      = sum(!!sym(outcome_var) == 1, na.rm = TRUE),  # number of "1"
         .groups    = "drop"
       )
     
     # Define heatwave boundaries
     heatwave_start <- as.Date("2024-06-18")
     heatwave_end   <- as.Date("2024-06-20")
     
     # Create the heatmap
     heatmap <- ggplot(data_grouped, aes(x = date, y = AC_label, fill = mean_value)) +
       
       # Heatmap tiles
       geom_tile(color = "white") +
       
       # Number of "Yes" displayed in each cell
       geom_text(aes(label = n_yes), color = "white", size = 3.5) +
       
       # Vertical lines marking heatwave start and end
       geom_vline(xintercept = as.numeric(heatwave_start - 0.5),
                  linetype = "dashed", color = "grey20", linewidth = 0.5) +
       geom_vline(xintercept = as.numeric(heatwave_end + 0.5),
                  linetype = "dashed", color = "grey20", linewidth = 0.5) +
       
       # Annotate heatwave phases
       annotate("text", x = as.Date("2024-06-14"), y = 3,
                label = "Pre-Heatwave", size = 7,
                fontface = "plain", color = "grey20") +
       annotate("text", x = as.Date("2024-06-19"), y = 3.25,
                label = "During\nHeatwave", size = 7,
                fontface = "plain", color = "grey20") +
       annotate("text", x = as.Date("2024-06-24"), y = 3,
                label = "Post-Heatwave", size = 7,
                fontface = "plain", color = "grey20") +
       
       scale_fill_viridis_c(
         name   = "",
         option = "D",
         limits = c(0, 1),
         breaks = seq(0, 1, 0.25),
         labels = scales::percent_format()
       ) +
       
       # Format x-axis
       scale_x_date(
         date_labels = "%b %d",
         breaks      = seq(min(data$date), max(data$date), by = "3 days"),
         expand      = c(0.01, 0.01)
       ) +
       
       # Y-axis — AC vs No AC with space for annotations
       scale_y_discrete(
         expand = expansion(add = c(0, 2))
       ) +
       
       labs(title = NULL, x = NULL, y = NULL) +
       theme_minimal(base_size = 12) +
       
       # Square tiles
       coord_fixed(ratio = 1, clip = "off") +
       
       theme(
         axis.text.x       = element_text(angle = 0, hjust = 0.5,
                                          size = 16, color = "grey20"),
         axis.text.y       = element_text(size = 16, color = "grey20"),
         axis.title.x      = element_blank(),
         axis.title.y      = element_blank(),
         plot.title        = element_blank(),
         legend.position   = "right",
         legend.title      = element_text(size = 18, color = "grey20"),
         legend.text       = element_text(size = 18, color = "grey20"),
         legend.key.size   = unit(0.75, "cm"),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width  = unit(1, "cm"),
         panel.grid.major  = element_blank(),
         panel.grid.minor  = element_blank(),
         plot.margin       = margin(t = 5, r = 5, b = 5, l = 5)
       )
     
     # Print the plot
     print(heatmap)
     
     # Save as PNG
     filename_png <- file.path(
       output_dir,
       paste0("heatmap_AC_", gsub(" ", "_", outcome_name), ".png")
     )
     
     ggsave(
       file   = filename_png,
       plot   = heatmap,
       width  = 12,
       height = 4,      # ← moins haut car seulement 2 niveaux (AC vs No AC)
       dpi    = 600,
       bg     = "white"
     )
     
     cat(" Saved:\n")
     cat("  PNG:", filename_png, "\n")
     
   }  # end for loop
   
   cat("\n All AC heatmaps created\n")
   invisible(NULL)
 } 
 
## =============================================================================
# Function 17: Generate faceted heatmaps for the binary AC variable, showing daily trends across the heatwave period and AC access, with annotations for heatwave phases and counts of "Yes" responses in each cell, with fixed scales across facets to allow direct comparison of temporal trends across AC access categories.
 
 heatmap_plot_facets_fixed_AC <- function(data_list,
                                          outcome_variables,
                                          output_dir) {
   
   # Loop through each outcome variable
   for (outcome_name in names(outcome_variables)) {
     
     cat("\n", strrep("=", 60), "\n")
     cat("🔹 Creating faceted AC heatmap for:", outcome_name, "\n")
     cat(strrep("=", 60), "\n\n")
     
     # Retrieve data
     data        <- data_list[[outcome_variables[[outcome_name]]$data]]
     outcome_var <- outcome_variables[[outcome_name]]$var
     levels_var  <- outcome_variables[[outcome_name]]$levels
     
     if (!outcome_var %in% names(data)) {
       cat(" Variable not found. Skipping.\n")
       next
     }
     
     # Preprocess data — replace period by AC_label
     data <- data %>%
       filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
       mutate(
         AC_label = factor(ifelse(AC == "1", "AC", "No AC"),
                           levels = c("AC", "No AC")),
         date     = as.Date(date_converted),
         modality = factor(!!sym(outcome_var), levels = levels_var),
         heatwave_status = factor(
           heatwave_status,
           levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")
         )
       )
     
     # Compute total per date × AC_label
     totals <- data %>%
       group_by(date, AC_label) %>%
       summarise(total = n(), .groups = "drop")
     
     # Count per modality
     counts <- data %>%
       group_by(date, AC_label, modality) %>%
       summarise(n_mod = n(), .groups = "drop")
     
     # Merge to get proportions
     data_grouped <- counts %>%
       left_join(totals, by = c("date", "AC_label")) %>%
       mutate(prop = n_mod / total)
     
     # Define heatwave boundaries
     heatwave_start <- as.Date("2024-06-18")
     heatwave_end   <- as.Date("2024-06-20")
     
     # Create the faceted heatmap
     heatmap <- ggplot(data_grouped,
                       aes(x = date, y = AC_label, fill = prop)) +
       
       # Heatmap tiles
       geom_tile(color = "white") +
       # Number of "Yes" displayed in each cell
       geom_text(aes(label = n_mod), color = "white", size = 3.5)+
       
       # Vertical lines marking heatwave start and end
       geom_vline(xintercept = as.numeric(heatwave_start - 0.5),
                  linetype = "dashed", color = "grey20", linewidth = 0.5) +
       geom_vline(xintercept = as.numeric(heatwave_end + 0.5),
                  linetype = "dashed", color = "grey20", linewidth = 0.5) +
       
       # Facet by modality
       facet_wrap(~ modality, ncol = 1) +
       
       # Annotate heatwave phases
       annotate("text", x = as.Date("2024-06-14"), y = 3,
                label = "Pre-Heatwave", size = 8,
                fontface = "plain", color = "grey20") +
       annotate("text", x = as.Date("2024-06-19"), y = 3.25,
                label = "During\nHeatwave", size = 8,
                fontface = "plain", color = "grey20") +
       annotate("text", x = as.Date("2024-06-24"), y = 3,
                label = "Post-Heatwave", size = 8,
                fontface = "plain", color = "grey20") +
       
       scale_fill_viridis_c(
         name   = "",
         option = "D",
         limits = c(0, 1),
         breaks = seq(0, 1, 0.25),
         labels = scales::percent_format()
       ) +
       
       # Format x-axis
       scale_x_date(
         date_labels = "%b %d",
         breaks      = seq(min(data$date), max(data$date), by = "3 days"),
         expand      = c(0.01, 0.01)
       ) +
       
       # Y-axis - AC vs No AC with space for annotations
       scale_y_discrete(
         expand = expansion(add = c(0, 2))
       ) +
       
       labs(title = NULL, x = NULL, y = NULL) +
       theme_minimal(base_size = 12) +
       
       # Square tiles
       coord_fixed(ratio = 1, clip = "off") +
       
       theme(
         axis.text.x      = element_text(angle = 0, hjust = 0.5,
                                         size = 18, color = "grey20"),
         axis.text.y      = element_text(size = 18, color = "grey20"),
         axis.title.x     = element_blank(),
         axis.title.y     = element_blank(),
         plot.title       = element_blank(),
         
         # Facet strip labels
         strip.text       = element_text(size = 16, face = "bold",
                                         color = "grey20"),
         strip.background = element_blank(),
         
         # Legend
         legend.position   = "left",
         legend.title      = element_text(size = 18, color = "grey20"),
         legend.text       = element_text(size = 18, color = "grey20"),
         legend.key.size   = unit(0.75, "cm"),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width  = unit(1, "cm"),
         
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.y  = unit(1.5, "lines"),
         plot.margin      = margin(t = 50, r = 5, b = 5, l = 5)
       )
     
     # Print the plot
     print(heatmap)
     
     # Save as PNG - height adapted to number of modalities
     filename_png <- file.path(
       output_dir,
       paste0("heatmap_faceted_AC_", gsub(" ", "_", outcome_name), ".png")
     )
     
     n_modalities <- length(levels_var)
     plot_height  <- 4 + (n_modalities * 3)
     
     ggsave(
       file   = filename_png,
       plot   = heatmap,
       width  = 12,
       height = plot_height,
       dpi    = 600,
       bg     = "white"
     )
     
     cat("Saved:\n")
     cat("  PNG:", filename_png, "\n")
     cat("  Modalities:", n_modalities, "\n")
     cat("  Height:", plot_height, "inches\n")
   }
   
   cat("\n All faceted AC heatmaps created\n")
   invisible(NULL)
 }
 
 