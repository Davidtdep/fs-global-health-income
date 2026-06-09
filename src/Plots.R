#*******************************************************************************
#### 0. LIBRARIES ####
#*******************************************************************************

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(gridExtra)



#*******************************************************************************
#### 1. DATA INPUT ####
#*******************************************************************************

step_1 = read_excel("~/Desktop/food-security/income/data/step_1_income_global_health.xlsx")
step_2 = read_excel("~/Desktop/food-security/income/data/step_2_income_global_health.xlsx")
step_3 = read_excel("~/Desktop/food-security/income/data/step_3_income_global_health.xlsx")


#*******************************************************************************
#### 2. COEFFICIENT SCALING ####
#*******************************************************************************

################################################################################
# Create new ORs scaled to a 1000-unit increase
# The transformation is applied ONLY when effect_unit == "OR".
################################################################################

step_1 <- step_1 %>%
  dplyr::mutate(
    estimate_interpretable_1000 = dplyr::if_else(
      effect_unit %in% c("OR", "IRR"),
      estimate_interpretable^1000,
      estimate_interpretable * 1000
    ),
    
    ci_low_interpretable_1000 = dplyr::if_else(
      effect_unit %in% c("OR", "IRR"),
      ci_low_interpretable^1000,
      ci_low_interpretable * 1000
    ),
    
    ci_high_interpretable_1000 = dplyr::if_else(
      effect_unit %in% c("OR", "IRR"),
      ci_high_interpretable^1000,
      ci_high_interpretable * 1000
    )
  )



step_2 <- step_2 %>%
  dplyr::mutate(
    estimate_interpretable_1000 = dplyr::if_else(
      effect_unit %in% c("OR", "IRR"),
      estimate_interpretable^1000,
      estimate_interpretable * 1000
    ),
    
    ci_low_interpretable_1000 = dplyr::if_else(
      effect_unit %in% c("OR", "IRR"),
      ci_low_interpretable^1000,
      ci_low_interpretable * 1000
    ),
    
    ci_high_interpretable_1000 = dplyr::if_else(
      effect_unit %in% c("OR", "IRR"),
      ci_high_interpretable^1000,
      ci_high_interpretable * 1000
    )
  )


#*******************************************************************************
#### 3. PLOTTING STEP 1 ####
#*******************************************************************************

#******************************
##### 3.1. BETA COEFFICIENTS #####
#******************************


# 1. Pre-processing
plot_data <- step_1 %>%
  filter(effect_unit == "β") %>%
  mutate(
    income_group = factor(income_group, levels = c(
      "Low income", 
      "Lower middle income", 
      "Upper middle income", 
      "High income"
    )),
    target_var = ifelse(dependent_var == "total_publications", independent_var, dependent_var),
    is_significant = ifelse(!is.na(p_value_adj_holm) & p_value_adj_holm < 0.05, "Significant", "Not Significant")
  )

# Definimos las etiquetas personalizadas para el eje X
income_labels <- c(
  "Low income"           = "LICs",
  "Lower middle income"  = "LMICs",
  "Upper middle income"  = "UMICs",
  "High income"          = "HICs"
)

human_format <- label_number(scale_cut = cut_short_scale())
unique_vars <- unique(plot_data$target_var)

for (var_name in unique_vars) {
  
  subset_df <- plot_data %>% 
    filter(target_var == var_name) %>%
    filter(!is.na(estimate_interpretable_1000))
  
  if (nrow(subset_df) == 0) next
  
  y_label <- unique(subset_df$effect_unit)[1]
  
  # 2. Create the Plot
  p <- ggplot(subset_df, aes(x = income_group, y = estimate_interpretable_1000)) +
    geom_bar(aes(fill = is_significant), stat = "identity", color = "black", width = 0.8, alpha = 0.7, linewidth = 0) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
    
    geom_line(aes(group = 1), linetype = "dashed", color = "black", linewidth = 0.8) +
    geom_point(color = "black", size = 2.5) +
    
    scale_fill_manual(values = c("Significant" = "red", "Not Significant" = "grey")) +
    
    # NUEVO: Aquí cambiamos los nombres de las etiquetas en el eje X
    scale_x_discrete(labels = income_labels) +
    
    scale_y_continuous(
      trans = "pseudo_log", 
      labels = human_format
    ) +
    
    geom_text(
      aes(
        label = human_format(estimate_interpretable_1000),
        vjust = ifelse(estimate_interpretable_1000 >= 0, -1, 2) 
      ), 
      angle = 90, 
      size = 3.5,
      fontface = "bold"
    ) +
    
    labs(
      title = paste("Model Results for:", var_name),
      subtitle = "Trend across Income Groups",
      x = "Income Group",
      y = y_label,
      fill = "P-Value < 0.05"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "none"
    )
  
  print(p)
}



#******************************
##### 3.2. IRR & OR COEFFICIENTS #####
#******************************

# 1. Filter and prepare the data for IRR and OR models
forest_data <- step_1 %>%
  # Keep ONLY models that use "IRR" or "OR"
  filter(effect_unit %in% c("IRR", "OR")) %>%
  
  mutate(
    # Identify the variable to be used as the plot title 
    plot_title = ifelse(dependent_var == "total_publications",
                        independent_var,
                        dependent_var),
    
    # NEW: Determine if the row belongs to the specific variables
    use_standard_cols = plot_title %in% c("Sex ratio", 
                                          "NURSES AND MIDWIVES (PER 1,000 PEOPLE)", 
                                          "PHYSICIANS (PER 1,000 PEOPLE)"),
    
    # NEW: Create final columns dynamically picking the correct values
    final_estimate = ifelse(use_standard_cols, estimate_interpretable, estimate_interpretable_1000),
    final_ci_low   = ifelse(use_standard_cols, ci_low_interpretable, ci_low_interpretable_1000),
    final_ci_high  = ifelse(use_standard_cols, ci_high_interpretable, ci_high_interpretable_1000)
  ) %>%
  
  # Filter out Infinite values AFTER selecting the correct columns
  filter(!is.infinite(final_estimate) & 
           !is.infinite(final_ci_low) & 
           !is.infinite(final_ci_high)) %>%
  
  mutate(
    # Recode income groups to abbreviations and set factor levels
    income_group_abbr = case_when(
      income_group == "Low income" ~ "LICs",
      income_group == "Lower middle income" ~ "LMICs",
      income_group == "Upper middle income" ~ "UMICs",
      income_group == "High income" ~ "HICs"
    ),
    
    # Ensure they are factors in the correct order for the Y-axis
    income_group_abbr = factor(income_group_abbr,
                               levels = c("LICs", "LMICs", "UMICs", "HICs")),
    
    # Create a variable for color based on adjusted p-value significance
    significance = ifelse(p_value_adj_holm < 0.05, "Significant", "Not Significant"),
    
    # Formatted label text using the new dynamically selected columns
    label_text = sprintf("%.2f (%.2f - %.2f)", 
                         final_estimate, 
                         final_ci_low, 
                         final_ci_high)
  )

# 2. Get a list of all unique non-total_publications variables
unique_variables <- unique(forest_data$plot_title)

# 3. Loop through each variable, generate its forest plot, and print it
for (var in unique_variables) {
  
  # Subset the data for the current variable
  df_subset <- forest_data %>% filter(plot_title == var)
  
  # Skip to the next iteration if the subset is empty
  if(nrow(df_subset) == 0) next
  
  # Extract the effect unit symbol to use as the x-axis label ("IRR" or "OR")
  x_axis_label <- unique(df_subset$effect_unit)[1]
  
  # Create the forest plot using the final_estimate column
  p <- ggplot(df_subset, aes(x = final_estimate, 
                             y = income_group_abbr, 
                             color = significance)) +
    
    # Add a vertical dashed reference line at x = 1 (Null effect)
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.6) +
    
    # Add the confidence intervals using the final_ci columns
    geom_errorbarh(aes(xmin = final_ci_low, 
                       xmax = final_ci_high), 
                   height = 0.2, linewidth = 1) +
    
    # Add the point estimates
    geom_point(size = 4) +
    
    # Colors: Red for significant, Grey for non-significant
    scale_color_manual(values = c("Significant" = "red", 
                                  "Not Significant" = "grey")) +
    
    # Add formatted text labels above the points
    geom_text(aes(label = label_text),
              vjust = -1.5, size = 5.5, color = "black") +
    
    # Standard log10 scale for the x-axis with comma labels
    scale_x_log10(labels = comma) +
    
    # Prevent text clipping
    coord_cartesian(clip = "off") +
    
    # Labels and dynamic title
    labs(
      title = var,
      x = x_axis_label,
      y = "Income Group"
    ) +
    
    # Clean forest plot theme
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(face = "bold", size = 13),
      plot.margin = margin(t = 25, r = 20, b = 10, l = 10) 
    )
  
  # Render the plot in RStudio
  print(p)
}





#*******************************************************************************
#### 3. PLOTTING STEP 2 ####
#*******************************************************************************


#------------------------------------------------------------------------------
# 0) OPTIONAL: nicer category titles (keeps your dependent_var text intact)
#------------------------------------------------------------------------------

category_labels <- c(
  "health_system_and_financing"    = "Health System & Financing",
  "health_outcomes_and_demography" = "Health Outcomes & Demography"
)

# Facet order (top to bottom)
category_order <- unname(category_labels)

#------------------------------------------------------------------------------
# 1) PREPARE DATA
#------------------------------------------------------------------------------

hierarchical_results <- step_2 %>%
  # Keep only valid effect units (drop NA as requested) - Added IRR
  filter(effect_unit %in% c("β", "OR", "IRR")) %>%
  mutate(
    # Coerce to numeric using the _1000 columns
    effect_size      = suppressWarnings(as.numeric(estimate_interpretable_1000)),
    effect_ci_lower  = suppressWarnings(as.numeric(ci_low_interpretable_1000)),
    effect_ci_upper  = suppressWarnings(as.numeric(ci_high_interpretable_1000)),
    
    # Clean predictor label (your independent var is always total_publications)
    indep_clean = case_when(
      independent_var == "total_publications" ~ "Publications",
      TRUE ~ independent_var
    ),
    
    # Use dependent_var as-is (already human-readable)
    dep_clean = dependent_var,
    
    # Model label: Publications → outcome
    model_label = paste0(indep_clean, " \u2192 ", dep_clean),
    
    # Category for faceting:
    # Prefer step_2$category if present; map to nicer titles when possible
    category_raw = category,
    category = ifelse(category_raw %in% names(category_labels),
                      unname(category_labels[category_raw]),
                      category_raw),
    
    # Significance based on FDR BH adjusted p-values
    significance = case_when(
      p_adj_fdr_bh < 0.001 ~ "p < 0.001",
      p_adj_fdr_bh < 0.01  ~ "p < 0.01",
      p_adj_fdr_bh < 0.05  ~ "p < 0.05",
      TRUE                 ~ "Not significant"
    ),
    
    # Label for the model family / distribution on the plot
    distribution = model_type
  ) %>%
  filter(
    !is.na(effect_size),
    !is.na(effect_ci_lower),
    !is.na(effect_ci_upper),
    !is.na(model_label),
    !is.na(category)
  ) %>%
  mutate(
    # Set a stable category order
    category = factor(category, levels = category_order)
  )

#------------------------------------------------------------------------------
# 2) ORDER ROWS (like your original: by category, then by absolute magnitude)
#------------------------------------------------------------------------------

hierarchical_results <- hierarchical_results %>%
  arrange(category, desc(abs(effect_size))) %>%
  mutate(
    # Factor levels so the first row appears at the TOP within each facet block
    model_label = factor(model_label, levels = rev(unique(model_label)))
  )

#------------------------------------------------------------------------------
# 3) FORMAT EFFECT SIZE + CI FOR THE RIGHT-SIDE TABLE
#------------------------------------------------------------------------------

# Helper: format numbers with friendly suffixes (K, M, B, T)
format_num <- function(x, unit = "β") {
  if (is.na(x)) return(NA_character_)
  
  ax <- abs(x)
  sign_str <- ifelse(x < 0, "-", "")
  
  # Valores minúsculos: mantener notación científica
  if (ax > 0 && ax < 0.001) return(sprintf("%.2e", x))
  
  # Valores astronómicos que superan los Trillones: mantener notación científica
  if (ax >= 1e15) return(sprintf("%.2e", x))
  
  # Sufijos amigables
  if (ax >= 1e12) return(sprintf("%s%.1fT", sign_str, ax / 1e12))
  if (ax >= 1e9)  return(sprintf("%s%.1fB", sign_str, ax / 1e9))
  if (ax >= 1e6)  return(sprintf("%s%.1fM", sign_str, ax / 1e6))
  if (ax >= 1e3)  return(sprintf("%s%.1fK", sign_str, ax / 1e3))
  
  # Para números normales (< 1000)
  if (unit %in% c("OR", "IRR")) {
    return(sprintf("%.3f", x))
  } else {
    if (ax < 0.01)  return(sprintf("%.5f", x))
    if (ax < 1)     return(sprintf("%.4f", x))
    if (ax < 10)    return(sprintf("%.3f", x))
    return(sprintf("%.2f", x))
  }
}

hierarchical_results <- hierarchical_results %>%
  rowwise() %>%
  mutate(
    effect_text = format_num(effect_size, effect_unit),
    ci_text     = paste0("(", format_num(effect_ci_lower, effect_unit), ", ",
                         format_num(effect_ci_upper, effect_unit), ")"),
    display_text = paste(effect_text, ci_text)
  ) %>%
  ungroup()

#------------------------------------------------------------------------------
# 4) SYMMETRIC LOG TRANSFORM (handles both negative β and large magnitudes)
#------------------------------------------------------------------------------

symlog_trans <- function(base = 10, threshold = 1, scale = 1) {
  trans <- function(x) sign(x) * log10(1 + abs(x) / threshold) * scale
  inv   <- function(x) sign(x) * (base^(abs(x) / scale) - 1) * threshold
  
  scales::trans_new(
    name = paste0("symlog-", format(threshold)),
    transform = trans,
    inverse = inv,
    domain = c(-Inf, Inf),
    breaks = scales::extended_breaks(),
    format = scales::format_format(scientific = FALSE)
  )
}

# Choose a safe threshold
abs_nonzero <- abs(hierarchical_results$effect_size[hierarchical_results$effect_size != 0])
abs_nonzero <- abs_nonzero[is.finite(abs_nonzero)]

threshold <- if (length(abs_nonzero) == 0) {
  1
} else {
  max(quantile(abs_nonzero, probs = 0.05, names = FALSE) / 10, 1e-6)
}

# Expanded breaks to handle extremely large IRR^1000 values
wide_breaks <- c(-1e100, -1e50, -1e10, -1e5, -1e3, -10, -1, -0.1, 0.1, 1, 10, 1e3, 1e5, 1e10, 1e50, 1e100)
wide_breaks_axis <- c(-1e100, -1e50, -1e10, -1e5, -1e3, -10, -1, -0.1, 0, 0.1, 1, 10, 1e3, 1e5, 1e10, 1e50, 1e100)

#------------------------------------------------------------------------------
# 5) MAIN FOREST PLOT
#------------------------------------------------------------------------------

p <- ggplot(
  hierarchical_results,
  aes(
    y = model_label,
    x = effect_size,
    xmin = effect_ci_lower,
    xmax = effect_ci_upper
  )
) +
  # Reference line at 0
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  
  # Light reference lines aligned to the breaks
  geom_vline(
    xintercept = wide_breaks,
    color = "gray90", linewidth = 0.3, linetype = "solid", alpha = 0.5
  ) +
  
  # CIs (no end caps)
  geom_errorbarh(height = 0, color = "black", linewidth = 0.3) +
  
  # Points
  geom_point(
    aes(fill = significance),
    shape = 21, color = "black", stroke = 0.4, size = 3
  ) +
  
  # Small label for model distribution/family
  geom_text(
    aes(label = distribution),
    hjust = -0.2, vjust = -0.5, size = 2.8, color = "gray50"
  ) +
  
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  
  # Same palette as your original hierarchical plot
  scale_fill_manual(values = c(
    "p < 0.001"       = "#00a6fb",
    "p < 0.01"        = "#009E73",
    "p < 0.05"        = "#D55E00",
    "Not significant" = "#CCCCCC"
  )) +
  
  # Symmetric log x-axis with Friendly Suffixes
  scale_x_continuous(
    trans = symlog_trans(threshold = threshold),
    breaks = wide_breaks_axis,
    labels = function(x) {
      sapply(x, function(val) {
        if (is.na(val)) return(NA_character_)
        if (val == 0) return("0")
        
        ax <- abs(val)
        sign_str <- ifelse(val < 0, "-", "")
        
        if (ax < 0.001)  return(scales::scientific(val))
        if (ax >= 1e15)  return(scales::scientific(val))
        
        if (ax >= 1e12) return(sprintf("%s%gT", sign_str, ax / 1e12))
        if (ax >= 1e9)  return(sprintf("%s%gB", sign_str, ax / 1e9))
        if (ax >= 1e6)  return(sprintf("%s%gM", sign_str, ax / 1e6))
        if (ax >= 1e3)  return(sprintf("%s%gK", sign_str, ax / 1e3))
        
        return(as.character(val))
      })
    }
  ) +
  
  labs(
    x = "Effect size (per 1000 units, symmetric log scale)",
    y = NULL,
    fill = "Significance"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

#------------------------------------------------------------------------------
# 6) RIGHT-SIDE TEXT TABLE (β / OR / IRR (95% CI))
#------------------------------------------------------------------------------

table_data <- hierarchical_results %>%
  select(model_label, display_text, category) %>%
  arrange(match(model_label, levels(hierarchical_results$model_label)))

p_table <- ggplot(table_data, aes(x = 1, y = model_label)) +
  geom_text(aes(label = display_text), hjust = 0, size = 2.8) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 0),
    axis.text.y = element_blank()
  )

p_table_title <- ggplot() +
  annotate("text", x = 1, y = 0.5, label = "\u03B2 / OR / IRR (95% CI)", size = 3.5, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(t = 18, r = 5, b = 0, l = 0))

# Combine title + table
table_combined <- gridExtra::grid.arrange(
  p_table_title, p_table,
  ncol = 1, heights = c(0.05, 0.95)
)

# Combine main plot + table
final_plot <- gridExtra::grid.arrange(
  p, table_combined,
  ncol = 2, widths = c(0.8, 0.2)
)

print(final_plot)

# Save if needed:
# ggsave("forest_step2_hierarchical.png", final_plot, width = 14, height = 8, dpi = 300)


#*******************************************************************************
#### 4. Scaling STEP 3 results ####
#*******************************************************************************

# Define tu factor de escala (ej. 1000 artículos)
scale_factor <- 1000 

step_3_scaled <- step_3 %>%
  mutate(
    # 1. ESCALAR LAS COLUMNAS CRUDAS (CREANDO NUEVAS COLUMNAS)
    # El argumento .names = "{.col}_scaled" crea una copia multiplicada 
    # dejando la columna original intacta.
    across(
      c(estimate_raw, se, ci_low_raw, ci_high_raw,
        x_slope_p25_raw, x_slope_p25_se, x_slope_p25_ci_low_raw, x_slope_p25_ci_high_raw,
        x_slope_p75_raw, x_slope_p75_se, x_slope_p75_ci_low_raw, x_slope_p75_ci_high_raw),
      ~ .x * scale_factor,
      .names = "{.col}_scaled"
    ),
    
    # 2. CALCULAR LAS NUEVAS COLUMNAS INTERPRETABLES BASADAS EN EL LINK
    # Usamos las nuevas columnas "*_scaled" que acabamos de crear en el paso 1.
    estimate_interpretable_scaled = case_when(
      link == "identity" ~ estimate_raw_scaled,
      link %in% c("log", "logit") ~ exp(estimate_raw_scaled),
      TRUE ~ estimate_interpretable # Respaldo en caso de error
    ),
    ci_low_interpretable_scaled = case_when(
      link == "identity" ~ ci_low_raw_scaled,
      link %in% c("log", "logit") ~ exp(ci_low_raw_scaled),
      TRUE ~ ci_low_interpretable
    ),
    ci_high_interpretable_scaled = case_when(
      link == "identity" ~ ci_high_raw_scaled,
      link %in% c("log", "logit") ~ exp(ci_high_raw_scaled),
      TRUE ~ ci_high_interpretable
    ),
    
    # 3. RECALCULAR LAS PENDIENTES SIMPLES (NUEVAS COLUMNAS)
    x_slope_p25_interpretable_scaled = case_when(
      link == "identity" ~ x_slope_p25_raw_scaled,
      link %in% c("log", "logit") ~ exp(x_slope_p25_raw_scaled),
      TRUE ~ x_slope_p25_interpretable
    ),
    x_slope_p75_interpretable_scaled = case_when(
      link == "identity" ~ x_slope_p75_raw_scaled,
      link %in% c("log", "logit") ~ exp(x_slope_p75_raw_scaled),
      TRUE ~ x_slope_p75_interpretable
    ),
    
    # 4. RECONSTRUIR LA CADENA DE TEXTO PARA EL MANUSCRITO (NUEVA COLUMNA)
    effect_ci_interpretable_scaled = if_else(
      is.na(estimate_interpretable_scaled), 
      NA_character_,
      sprintf("%s: %.4f [%.4f - %.4f]", 
              effect_unit, 
              estimate_interpretable_scaled, 
              ci_low_interpretable_scaled, 
              ci_high_interpretable_scaled)
    )
  )

