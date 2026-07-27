

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















