# Script to create plots (and tables) of initial data for PPT slides


# Load packages
pacman::p_load(tidyverse, gt)

# Import data
data <- read_csv("manually-cleaned-data-v2.csv")

# According to collaborator, -99 is code for missing value, so recode as NaN
data <- data |>
  mutate(gene_expression = na_if(gene_expression, -99))


# TABLES
data_wide <- read_csv("manually-cleaned-data.csv")

# According to collaborator, -99 is code for missing value, so recode as NaN
data_wide <- data_wide |>
  mutate(gene_expression_WT_AF42_2 = na_if(gene_expression_WT_AF42_2, -99))

# Table for Wild Type (WT)
data_wide |>
  select(conc, 
         gene_expression_WT_placebo,
         gene_expression_WT_placebo_2,
         gene_expression_WT_AF42,
         gene_expression_WT_AF42_2,
         ) |>
  gt() |>
  tab_header(
    title = "Wild Type (WT)"
  ) |>
  tab_spanner(label = "Placebo",
              columns = c(gene_expression_WT_placebo, gene_expression_WT_placebo_2)) |>
  tab_spanner(label = "AF42",
              columns = c(gene_expression_WT_AF42, gene_expression_WT_AF42_2)) |>
  cols_label(
    conc = "Concentration",
    gene_expression_WT_placebo = "Trial 1",
    gene_expression_WT_placebo_2 = "Trial 2",
    gene_expression_WT_AF42 = "Trial 1",
    gene_expression_WT_AF42_2 = "Trial 2"
  )
  
# Table for CT10
data_wide |>
  select(conc, 
         gene_expression_CT101_placebo,
         gene_expression_CT101_placebo_2,
         gene_expression_CT101_AF42,
         gene_expression_CT101_AF42_2,
  ) |>
  gt() |>
  tab_header(
    title = "CT101"
  ) |>
  tab_spanner(label = "Placebo",
              columns = c(gene_expression_CT101_placebo, gene_expression_CT101_placebo_2)) |>
  tab_spanner(label = "AF42",
              columns = c(gene_expression_CT101_AF42, gene_expression_CT101_AF42_2)) |>
  cols_label(
    conc = "Concentration",
    gene_expression_CT101_placebo = "Trial 1",
    gene_expression_CT101_placebo_2 = "Trial 2",
    gene_expression_CT101_AF42 = "Trial 1",
    gene_expression_CT101_AF42_2 = "Trial 2"
  )

# Plot for the Wild Type (WT)
data |> 
  filter(gene_type == "WT") |>
  ggplot(aes(x = conc, y = gene_expression, col = treatment)) + 
  geom_point() + 
  labs(
    x = "Concentration of Growth Factor",
    y = "Gene Expression",
    title = "Wild Type (WT)", #: Gene expression for different concentrations of growth factor, for treatment and placebo",
    col = "Treatment")
  

# Plot for the gene type "CT101"
data |> 
  filter(gene_type == "CT101") |>
  ggplot(aes(x = conc, y = gene_expression, col = treatment)) + 
  geom_point() + 
  labs(
    x = "Concentration of Growth Factor",
    y = "Gene Expression",
    title = "Gene Type CT101", #: Gene expression for different concentrations of growth factor, for treatment and placebo",
    col = "Treatment")

#  geom_smooth()

# ggplot(data, aes(x = conc, y = gene_expression_WT_placebo)) + 
#   geom_point() + 
#   geom_point(aes(x = conc, y = gene_expression_WT_placebo_2)) +
#   geom_point(aes(x = conc, y = gene_expression_WT_AF42), col = 'red')
