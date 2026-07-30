

# Install required packages if you don't have them
# install.packages(c("readxl", "ggplot2", "tidyr", "dplyr"))

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Load the data from your Excel file
# IMPORTANT: Replace "MTI_Data.xlsx" with your actual Excel file name
mti_raw <- read_excel("input.xlsx", sheet = "MTI")

# Convert the first column (Group names) into row names for easier manipulation
mti_data <- as.data.frame(mti_raw)
rownames(mti_data) <- mti_data[[1]]
mti_data <- mti_data[, -1] # Remove the first column now that they are row names

# 2. Format the data for ggplot (convert from a wide matrix to a long table)
mti_data$Impacted <- rownames(mti_data)
mti_long <- pivot_longer(mti_data, cols = -Impacted, names_to = "Impacting", values_to = "Impact")

# Lock in the exact ecological order of your functional groups
groups <- c("Carp", "Snakehead", "Minnows", "Clupeid", "Catfish", "Tilapia", "Knifefish", 
            "Anabas", "Prawn", "Insect/Larvae", "Zooplankton", "Phytoplankton", "Detritus", "Fleet1")

mti_long$Impacting <- factor(mti_long$Impacting, levels = groups)
mti_long$Impacted <- factor(mti_long$Impacted, levels = rev(groups)) # Reversed so the Y-axis reads top-to-bottom

## 3. Create the premium static plot (Updated)
mti_plot <- ggplot(mti_long, aes(x = Impacting, y = Impacted)) +
  geom_point(aes(size = abs(Impact), color = Impact)) +
  # Red for negative impact, Blue for positive, White for neutral
  scale_color_gradient2(
    low = "#d73027", mid = "white", high = "#4575b4", midpoint = 0,
    # Changed barheight from 1 to 0.8 to fit perfectly within the margins
    guide = guide_colorbar(barheight = unit(0.8, "npc"), title.position = "top")
  ) +
  # Hide the redundant size legend to keep the visual uncluttered
  scale_size_continuous(range = c(0, 12), guide = "none") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    # Removed the plot.title theme formatting since the title is gone
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20) 
  ) +
  labs(
    # The title line has been completely removed
    x = "Impacting Group",
    y = "Impacted Group",
    color = "Relative\nImpact"
  )

# Display the plot in RStudio
print(mti_plot)

# 4. Export as a high-quality PDF
ggsave(
  filename = "Kaptai_Lake_MTI.pdf",
  plot = mti_plot,
  width = 10,
  height = 8,
  units = "in",
  device = "pdf",
  useDingbats = FALSE
)




# 1. Install required packages (uncomment and run if you don't have them)
# install.packages(c("ggplot2", "dplyr", "tidyr", "ggrepel"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# 2. Read the weirdly formatted EwE CSV file
# Replace "niche.csv" with your actual file path if needed
raw_data <- read.csv("niche.csv", stringsAsFactors = FALSE, check.names = FALSE)

# 3. Data Wrangling: Extract the pair names from the first row
pair_names <- as.character(raw_data[1, -1])

# Remove the first row so we only have numeric data
data_numeric <- raw_data[-1, ]

# Fix column names: First column is X (Predator Overlap), the rest are Y (Prey Overlap)
colnames(data_numeric)[1] <- "Predator_Overlap"
colnames(data_numeric)[-1] <- pair_names

# Convert all columns to numeric format
data_numeric[] <- lapply(data_numeric, as.numeric)

# 4. Reshape the data from wide to long format
df_long <- data_numeric %>%
  pivot_longer(
    cols = -Predator_Overlap, 
    names_to = "Pair", 
    values_to = "Prey_Overlap", 
    values_drop_na = TRUE
  ) %>%
  filter(!is.na(Prey_Overlap))

# Create a "Primary Group" column for coloring the dots (takes the first name before the colon)
df_long$Primary_Group <- sub(":.*", "", df_long$Pair)

# 5. Create the beautiful, publication-ready plot
niche_plot <- ggplot(df_long, aes(x = Predator_Overlap, y = Prey_Overlap, color = Primary_Group)) +
  # Add the dots
  geom_point(size = 3.5, alpha = 0.8) +
  # Add non-overlapping labels using the actual group names instead of confusing numbers!
  geom_text_repel(
    aes(label = Pair), 
    size = 3.5, 
    max.overlaps = Inf,       # Forces R to label every single point
    box.padding = 0.6,        # Space between labels
    point.padding = 0.3,      # Space between label and dot
    show.legend = FALSE       # Hides 'a' from the legend
  ) +
  # Apply a clean, high-contrast scientific theme
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  # Set the axis limits based on your data (similar to the EwE software scale)
  scale_x_continuous(limits = c(0.2, 1.1), breaks = seq(0.2, 1.1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.2)) +
  # Add exact labels
  labs(
    x = "Predator Overlap Index (dimensionless)",
    y = "Prey Overlap Index (dimensionless)",
    color = "Primary Group"
  )

# Display the plot
print(niche_plot)

# 6. Export directly to high-quality PDF
ggsave(
  filename = "Kaptai_Lake_Niche_Overlap.pdf",
  plot = niche_plot,
  width = 12,       # Wide format gives the labels plenty of room to spread out
  height = 8,
  units = "in",
  device = "pdf",
  useDingbats = FALSE
)


### Plotting the stacked barplot for reconstracted catch data

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # For clean comma formatting on the y-axis

# 1. Load your reconstructed data

catch_data <- read_excel("data.xlsx", sheet = 3)

# 2. Reshape the data and map species to their Functional Groups
catch_grouped <- catch_data %>%
  # Pivot from wide to long format
  pivot_longer(
    cols = -Species, 
    names_to = "Year",
    values_to = "Catch"
  ) %>%
  # Clean up year formatting if R added an "X" or "." during import (e.g., X2010.11 -> 2010-11)
  mutate(Year = gsub("X", "", Year),
         Year = gsub("\\.", "-", Year)) %>%
  # Group the 17 species into the Functional Groups defined in your Table 1
  mutate(Functional_Group = case_when(
    Species %in% c("Labeo rohita", "Catla catla", "Cirrhinus mrigala", "Labeo calbasu", "Labeo bata") ~ "Carp",
    Species %in% c("Channa striata", "Channa punctata") ~ "Snakehead",
    Species %in% c("Puntius sarana", "A. mola") ~ "Minnows",
    Species %in% c("Corica soborna", "Gudusia chapra") ~ "Clupeid",
    Species %in% c("Wallago attu", "Ompok pabda", "Mystus gulio") ~ "Catfish",
    Species == "O. niloticus" ~ "Tilapia",
    Species == "N. notopterus" ~ "Knifefish",
    Species == "A. testudineus" ~ "Anabas",
    TRUE ~ "Other" # Fallback just in case
  )) %>%
  # Summarize the catch by Year and Functional Group to prepare for the stacked plot
  group_by(Year, Functional_Group) %>%
  summarise(Total_Catch = sum(Catch, na.rm = TRUE), .groups = 'drop')

# 3. Generate the publication-ready stacked barplot
stacked_plot <- ggplot(catch_grouped, aes(x = Year, y = Total_Catch, fill = Functional_Group)) +
  # Use stat="identity" and add a thin black border to separate stacks
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
  
  # Use a vibrant, colorblind-friendly palette
  scale_fill_viridis_d(option = "turbo") + 
  
  # Format Y-axis numbers with commas (e.g., 20,000 instead of 2e+04)
  scale_y_continuous(labels = comma) +
  
  # Add clear, academic labels
  labs(
    title = "Reconstructed Annual Catch of Kaptai Lake by Functional Group (2010–2020)",
    x = "Timeline",
    y = "Total Reconstructed Yield (Metric Tons)",
    fill = "Functional Group"
  ) +
  
  # Apply a clean theme suited for graphical abstracts
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.text = element_text(color = "black"), 
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for a cleaner look
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# View the plot
print(stacked_plot)

# 4. Save the plot in high resolution for your manuscript
 ggsave("Kaptai_Reconstructed_StackedBar.png", plot = stacked_plot, width = 10, height = 6, dpi = 300)



 ### plotting trophic spectra of biomass and catch

 # Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Recreate the data (using approximate values from your plot)
# Replace these with the exact values from your EwE output
trophic_data <- data.frame(
  Trophic_Level = c("I", "II", "III", "IV", "V", "VI", "VII"),
  Biomass = c(4.5, 9.0, 4.2, 0.2, 0, 0, 0),
  Catch = c(0, 6.2, 8.0, 0.1, 0, 0, 0)
)

# 2. Filter out the empty bars (remove rows where both metrics are 0)
trophic_filtered <- trophic_data %>%
  filter(Biomass > 0 | Catch > 0)

# Lock the factor levels so they maintain the correct Roman numeral order
trophic_filtered$Trophic_Level <- factor(trophic_filtered$Trophic_Level, levels = trophic_filtered$Trophic_Level)

# 3. Reshape and invert Catch values for the downward plot
trophic_long <- trophic_filtered %>%
  pivot_longer(cols = c(Biomass, Catch), names_to = "Metric", values_to = "Value") %>%
  # If the metric is Catch, make it negative so it plots downwards
  mutate(Plot_Value = ifelse(Metric == "Catch", -Value, Value))

# 4. Create the Mirrored Bar Chart
mirrored_plot <- ggplot(trophic_long, aes(x = Trophic_Level, y = Plot_Value, fill = Metric)) +
  
  # Add the bars
  geom_col(color = "black", width = 0.85, alpha = 0.9) +
  
  # Add a strong central zero-line (the common X-axis)
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Match the vibrant color scheme from your previous plots
  scale_fill_manual(values = c("Biomass" = "#2ecc71", "Catch" = "#e67e22")) +
  
  # Format Y-axis: Use absolute values so negative catch numbers appear positive
  scale_y_continuous(
    labels = abs, 
    breaks = seq(-10, 10, by = 2.5), # Adjust 'by' depending on your exact data range
    name = expression(bold("Amount (t/km"^2*"/year)"))
  ) +
  
  # Clean academic titles and labels
  labs(
    title = "Trophic Spectra of Kaptai Lake",
    x = "Discrete Trophic Level"
  ) +
  
  # Apply a clean, publication-ready theme suitable for a Graphical Abstract
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top", # Moving legend to the top saves horizontal space
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

# View the perfectly formatted mirrored plot
print(mirrored_plot)

# Save as a high-resolution PDF
# ggsave("Kaptai_Mirrored_Spectra.pdf", plot = mirrored_plot, width = 7, height = 6, device = "pdf")

# Keystoneness Index Scatter Plot
 
 library(ggplot2)
 library(ggrepel)
 
 # 1. Load the data
 data <- read.csv("Keystoneness_Index.csv", check.names = FALSE)
 
 # 2. Create a new combined label: "Group Name \n (Biomass)"
 # The \n puts the biomass on a new line right under the name so it stays compact
 data$Plot_Label <- paste0(data$`Group name`, "\n(", data$Biomass, ")")
 
 # 3. Create the streamlined plot
 ks_direct_plot <- ggplot(data, aes(x = `Relative total impact`, y = `Keystone index #1`)) +
   
   geom_point(aes(size = Biomass, color = `Group name`), alpha = 0.8, shape = 21, fill = "white", stroke = 1.5) +
   scale_size_continuous(range = c(5, 20)) +
   scale_color_viridis_d(option = "turbo") +
   
   # Use the new combined label
   geom_text_repel(aes(label = Plot_Label, color = `Group name`), 
                   fontface = "bold", size = 4.5, box.padding = 0.8, 
                   point.padding = 1, max.overlaps = Inf,
                   segment.color = "grey50", segment.size = 0.5,
                   lineheight = 0.9) + # lineheight tightens the gap between the two text lines
   
   labs(
     title = "Keystoneness vs. Ecosystem Impact in Kaptai Lake",
     x = "Relative Total Impact (Overall Effect)",
     y = expression(bold("Keystone Index #1"))
   ) +
   
   theme_minimal(base_size = 14) +
   theme(
     axis.text = element_text(color = "black", face = "bold", size = 12),
     axis.title = element_text(face = "bold", size = 14),
     panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
     plot.background = element_rect(fill = "white", color = NA),
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold", size = 16),
     legend.position = "none" # Completely removes ALL legends for maximum plot space
   )
 
 print(ks_direct_plot)
 ggsave("Kaptai_DirectLabel_Keystoneness.pdf", plot = ks_direct_plot, width = 10, height = 7, device = "pdf")
 
 
 
 # Modified trophic spectra plot for catch and biomass
 # Load required libraries
 library(ggplot2)
 library(dplyr)
 library(tidyr)
 
 # 1. Recreate the data (using approximate values from your plot)
 # Replace these with the exact values from your EwE output
 trophic_data <- data.frame(
   Trophic_Level = c("I", "II", "III", "IV", "V", "VI", "VII"),
   Biomass = c(4.5, 9.0, 4.2, 0.2, 0, 0, 0),
   Catch = c(0, 6.2, 8.0, 0.1, 0, 0, 0)
 )
 
 # 2. Filter out the empty bars (remove rows where both metrics are 0)
 trophic_filtered <- trophic_data %>%
   filter(Biomass > 0 | Catch > 0)
 
 # Lock the factor levels so they maintain the correct Roman numeral order
 trophic_filtered$Trophic_Level <- factor(trophic_filtered$Trophic_Level, levels = trophic_filtered$Trophic_Level)
 
 # 3. Reshape and invert Catch values for the downward plot
 trophic_long <- trophic_filtered %>%
   pivot_longer(cols = c(Biomass, Catch), names_to = "Metric", values_to = "Value") %>%
   # If the metric is Catch, make it negative so it plots downwards
   mutate(Plot_Value = ifelse(Metric == "Catch", -Value, Value))
 
 # 4. Create the Mirrored Bar Chart
 mirrored_plot <- ggplot(trophic_long, aes(x = Trophic_Level, y = Plot_Value, fill = Metric)) +
   
   # Add the bars
   geom_col(color = "black", width = 0.85, alpha = 0.9) +
   
   # Add a strong central zero-line (the common X-axis)
   geom_hline(yintercept = 0, color = "black", linewidth = 1) +
   
   # Match the vibrant color scheme from your previous plots
   scale_fill_manual(values = c("Biomass" = "#2ecc71", "Catch" = "#e67e22")) +
   
   # Format Y-axis: Use absolute values so negative catch numbers appear positive
   scale_y_continuous(
     labels = abs, 
     breaks = seq(-10, 10, by = 2.5), # Adjust 'by' depending on your exact data range
     name = expression(bold("Amount (t/km"^2*"/year)"))
   ) +
   
   # Clean academic titles and labels
   labs(
     title = "Trophic Spectra of Kaptai Lake",
     x = "Discrete Trophic Level"
   ) +
   
   # Apply a clean, publication-ready theme suitable for a Graphical Abstract
   theme_minimal(base_size = 14) +
   theme(
     axis.text = element_text(color = "black", face = "bold"),
     axis.title = element_text(face = "bold"),
     plot.title = element_text(face = "bold", hjust = 0.5),
     legend.position = "top", # Moving legend to the top saves horizontal space
     legend.title = element_blank(),
     legend.text = element_text(face = "bold", size = 12),
     panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
     plot.background = element_rect(fill = "white", color = NA),
     panel.grid.minor = element_blank()
   )
 
 # View the perfectly formatted mirrored plot
 print(mirrored_plot)
 
 # Save as a high-resolution PDF
  ggsave("Kaptai_Mirrored_Spectra.pdf", plot = mirrored_plot, width = 7, height = 6, device = "pdf")
 
 