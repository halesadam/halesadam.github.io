# Plotting

# Setup
library(tidyverse)
library(gganimate)
library(gifski)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)
library(knitr)
library(gt)

#load data set
df <- read_csv("./Data/Merged_DSR_Origin_Dataset.csv")

#Set appropriate factors and characters for plotting
#leave Accession ID as a character
df$Season <- as.factor(df$Season)
df$Block <- as.factor(df$Block)
df$Origin <- as.factor(df$Origin)
df$Species <- as.factor(df$Species)
df$DSR <- as.integer(df$DSR)

#1. Create a plot showing DSR progression over time
# Remove missing values in DSR and for Species
df_clean <- df %>% drop_na(DSR) %>%
  filter(!is.na(Species))

# Aggregate data: Get mean DSR for each Rep and Season
df_avg <- df_clean %>%
  group_by(Season, Rep) %>%
  summarise(Avg_DSR = mean(DSR, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Rep = as.integer(Rep)) #replicate needs to be a integer for plot to work

# Create the plot
p1_anim <- 
ggplot(df_avg, aes(x = Rep, y = Avg_DSR, group = Season, color = as.factor(Season))) +
  geom_line(size = 1) +         # Line showing progression
  geom_point(size = 2) +        # Points for each Rep        # Facet wrap by Season
  labs(title = "Average DSR Progression Over Replicates", 
       x = "Replicate", 
       y = "Average DSR",
       color = "Season") +
  transition_reveal(Rep)

#save animated plot to Plots folder
anim_save("./Plots/p1_anim.gif", animation = p1_anim)


#2. Plot accesions across the world
# Summarize to get count per country
origin_summary <- df_clean %>%
  distinct(Accession_ID, Origin) %>%  # Keep only unique accession-country pairs
  group_by(Origin) %>%
  summarise(Accessions = n()) %>%
  ungroup()

# Get country centroids for mapping
world_map <- ne_countries(scale = "medium", returnclass = "sf")

country_coords <- world_map %>%
  st_centroid() %>%
  select(name, geometry) %>%
  mutate(long = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>%
  as.data.frame()

# Join your data with country coordinates
map_data <- origin_summary %>%
  left_join(country_coords, by = c("Origin" = "name")) %>%
  drop_na(lat, long)

# Plot the map with accessions
p2 <- 
  ggplot(data = world_map) +
  geom_sf(fill = "grey", color = "white") +  # Light, modern base map
  geom_point(data = map_data, aes(x = long, y = lat, size = Accessions), 
             color = "darkblue", alpha = 0.7) +  # Clean, bold points
  scale_size_continuous(range = c(2, 8)) +  # Control point size range
  theme_minimal(base_family = "Helvetica") +  # Clean font
  labs(
    title = "Number of Accessions by Country of Origin",
    x = NULL, y = NULL,
    size = "Number of Accessions"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#save plot
ggsave("./Plots/p2_Country.png", plot = p2, width = 10, height = 6, dpi = 300)


#3. Make boxplot showing average DSR for each sp for season 1 and season 2
#split into season 1 and season 2
df_season1 <- df_clean %>%
  filter(Season == 1)

df_season2 <- df_clean %>%
  filter(Season == 2)

#plot for season 1
p3_1_DSR_by_sp <- ggplot(df_season1)+
  aes(x = Species, y = DSR, fill = Species)+
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_family = "Helvetica") +
  labs(
    title = "DSR Distribution for Season 1",
    x = "Species",
    y = "DSR"
  )

ggsave("./Plots/p3_1_DSR_by_sp.png", plot = p3_1_DSR_by_sp, width = 10, height = 6, dpi = 300)

#same for season 2
p3_2_DSR_by_sp <- ggplot(df_season2)+
  aes(x = Species, y = DSR, fill = Species)+
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_family = "Helvetica") +
  labs(
    title = "DSR Distribution for Season 2",
    x = "Species",
    y = "DSR"
  )

ggsave("./Plots/p3_2_DSR_by_sp.png", plot = p3_2_DSR_by_sp, width = 10, height = 6, dpi = 300)

#two seasons combined
p_3_DSR_by_sp <- ggplot(df_clean)+
  aes(x = Species, y = DSR, fill = Species)+
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_family = "Helvetica") +
  labs(
    title = "DSR Distribution across Study",
    x = "Species",
    y = "DSR"
  )

ggsave("./Plots/p3_3_DSR_by_sp.png", plot = p_3_DSR_by_sp, width = 10, height = 6, dpi = 300)

#4.Make col plot showing number of sp.
#dataset is long format - keep only unique accessions
accession_counts <- df_clean %>%
  distinct(Accession_ID, Species) %>%
  filter(!is.na(Species)) %>%
  count(Species, name = "Number_Accessions")

p4 <- ggplot(accession_counts, aes(x = fct_reorder(Species, -Number_Accessions), y = Number_Accessions, fill = Species)) +
  geom_col(alpha = 0.8) +
  scale_fill_brewer(palette = "Paired") +  # or your preferred palette
  theme_minimal(base_family = "Helvetica") +
  labs(
    title = "Number of Accessions Per Species",
    x = "Species",
    y = "Number of Accessions"
  ) +
  theme(legend.position = "none")

ggsave("./Plots/p4_count.png", plot = p4, width = 10, height = 6, dpi = 300)

#5. I want to know the sp count of the lowest DSR accessions
lowest_dsr <- read.csv("./Data/low_dsr.csv") 

#slice to only 50 lowest
lowest_dsr <- lowest_dsr %>% 
  slice_head(n = 50)

lowest_accession_counts <- lowest_dsr %>%
  distinct(Accession_ID, Species) %>%
  filter(!is.na(Species)) %>%
  count(Species, name = "Number_Accessions")

p5 <- ggplot(lowest_accession_counts, aes(x = fct_reorder(Species, -Number_Accessions), y = Number_Accessions, fill = Species)) +
  geom_col(alpha = 0.8) +
  scale_fill_brewer(palette = "Paired") +  # or your preferred palette
  theme_minimal(base_family = "Helvetica") +
  labs(
    title = "Number of Accessions Per Species",
    subtitle = "Based on 50 Accessions With Lowest Average DSR",
    x = "Species",
    y = "Number of Accessions"
  ) +
  theme(legend.position = "none")

ggsave("./Plots/p5_low_dsr.png", plot = p5, width = 10, height = 6, dpi = 300)

#6. I want to make a table of all accession with their germplasm ID
Accession <- read.csv("./Data/low_dsr_with_accession")

#great, now let's reorder the columns to make it pretty
Accession <- Accession %>% 
  select(Accession_ID, Accession, Species, Avg_DSR) %>% 
  rename("Study ID" = Accession_ID,
         "Germplasm Accession Number" = Accession,
         "Average DSR" = Avg_DSR)

#use knitr to make it pretty
kable(Accession)

#another way
Accession %>%
  gt() 

