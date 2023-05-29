# Re-creating and improving the plot in the file gene_plot.pdf such that:
# - Font: Times New Roman
# - .tiff format
# - (9in x 6in) with a resolution of 500

# Load packages
pacman::p_load(tidyverse, ggrepel, showtext, patchwork, scales)

# Load data - long form
data <- read_csv(
  here::here("cleaned-data", "manually-cleaned-data-long.csv")
)

# According to collaborator, -99 is code for missing value, so recode as NaN
data <- data |>
  mutate(gene_expression = na_if(gene_expression, -99))

# Recreating Jono's colour palette
col_pal <- c("#78a8d1","#d5bf98")

# Add Times New Roman font
font_add(
  family = "times",
  regular = here::here(
    "Times_New_Roman.ttf"
  )
)

# Store the labels for each plot, and the conc-gene_expression locations
labels_WT <- data.frame(
  conc = rep(10,4),
  gene_expression = c(11.43, 10.47, 41.70, 36.38),
  treatment = c("placebo","placebo","AF42","AF42"),
  label = c("CsE","bNo","JZC","fUg")
)
labels_CT101 <- data.frame(
  conc = rep(10,4),
  gene_expression = c(20.21, 24.47, 33.96, 48.96),
  treatment = c("placebo","placebo","AF42","AF42"),
  label = c("jEK", "Hoe", "Rza", "xpo")
)



# Recreate plot
showtext_auto() # Set showtext running

# Set text size as variables, since it appears multiple times
text_size <- 60
label_size <- 20

# Create plot for Wild Type
plot_WT <- data %>% 
  filter(gene_type == "WT") %>%
  ggplot(aes(x = conc, y = gene_expression, fill = treatment)) +
  geom_label_repel(
    aes(label = label, x = conc, y = gene_expression), 
    data = labels_WT, 
    # inherit.aes = FALSE,
    nudge_x = 1,
    family = "times", # set the label font to be Times New Roman
    size = label_size,
    show.legend = FALSE, # no legend for the labels
    min.segment.length = 0 # make sure it draws lines to the points
  ) + 
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = col_pal,
                    breaks = c("AF42", "placebo"),
                    labels = c("Activating factor 42", "Placebo")) + 
  theme_bw() + 
  labs(
    x = "μg/ml",
    y = "Gene Expression",
    fill = "Treatment",
    title = "Wild-type"
  ) + 
  theme(
    text = element_text(family = "times", size = text_size)
  ) + 
  scale_x_continuous(breaks = seq(0,10)) + 
  scale_y_continuous(breaks = seq(10,40,10))



# Create plot for CT101
plot_CT101 <- data %>% 
  filter(gene_type == "CT101") %>%
  ggplot(aes(x = conc, y = gene_expression, fill = treatment)) + 
  geom_label_repel(
    aes(label = label, x = conc, y = gene_expression), 
    data = labels_CT101, 
    # inherit.aes = FALSE, 
    nudge_x = 1,
    family = "times", # set the label font to be Times New Roman
    size = label_size,
    show.legend = FALSE, # no legend for the labels
    min.segment.length = 0 # make sure it draws lines to the points
  ) + 
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = col_pal,
                    breaks = c("AF42", "placebo"),
                    labels = c("Activating factor 42", "Placebo")) +   theme_bw() + 
  labs(
    x = "μg/ml",
    y = "Gene Expression",
    fill = "Treatment",
    title = "Cell-type 101"
  ) + 
  theme(
    text = element_text(family = "times", size = text_size)
  ) + 
  scale_x_continuous(breaks = seq(0,10)) + 
  scale_y_continuous(breaks = seq(10,50,10))


# Combine plots using patchwork, set legend location
plot <- plot_WT + plot_CT101 & theme(legend.position = "bottom")

# Add letter annotations, and tell to collect legends ("guides")
plot <- plot + plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = "collect")

# Exporting to .tiff format
ggsave("gene_plot_updated.tiff", plot = plot,
       dpi = 500, height = 6, width = 9, units = "in")

