

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(janitor)
library(broom)

# read data ---------------------------------------------------------------

pl_cb <- read.csv("Final Project/pl_cb.csv")


# clean data --------------------------------------------------------------

model_cb <- pl_cb %>%
  select_if(is.numeric) %>%
  select(-Season_End_Year) %>%
  remove_constant(na.rm = TRUE) %>%
  as.matrix()
model_cb <- t(na.omit(t(model_cb))) %>% as_tibble()


# fit pca model -----------------------------------------------------------

pca_cb <- prcomp(model_cb, center = TRUE, scale = TRUE)
summary(pca_cb)


# tune number of component ------------------------------------------------

pca_cb %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_cb),
             color = "darkred",
             linetype = "dashed") +
  theme_bw()


# plot PCA result ---------------------------------------------------------

arrow_style <- arrow(angle = 20, ends = "first", 
                     type = "closed", length = unit(8, "pt"))

library(ggrepel)
pca_cb %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column, size = 3)) +
  theme_bw()


  ### Scree plot

library(factoextra)
fviz_eig(pca_cb)


  ### individuals

fviz_pca_ind(pca_cb)


  ### variables

fviz_pca_var(pca_cb)

  
  ### Biplot

fviz_pca_biplot(pca_cb)


# correlation matrix ------------------------------------------------------

library(ggcorrplot)

model_cb %>%
  cor() %>%
  ggcorrplot()


model_cb %>%
  cor() %>%
  round(2) %>%
  ggcorrplot(hc.order = TRUE, type = "lower", lab = FALSE)


summary(cor(model_cb))


# Clustering on variables -------------------------------------------------

cor_matrix_cb <- cor(model_cb)
cor_dist_matrix <- as.dist(1 - abs(cor_matrix_cb))

view(cor_matrix_cb) 

library(ggdendro)

cor_dist_matrix %>%
  hclust("complete") %>%
  ggdendrogram(rotate = TRUE)



  ### Heatmap before

cor_dist_matrix %>%
  as.matrix() %>%
  as_tibble() %>%
  mutate(var1 = rownames(as.matrix(cor_dist_matrix))) %>%
  pivot_longer(cols = -var1,
               names_to = "var2",
               values_to = "distance") %>%
  ggplot(aes(x = var1, y = var2, fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")


  ### Heatmap after seriation

library(seriation)

order <- cor_dist_matrix %>%
  seriate() %>%
  get_order()

var_order <- colnames(model_cb)[order]

cor_dist_matrix %>%
  as.matrix() %>%
  as_tibble() %>%
  mutate(var1 = rownames(as.matrix(cor_dist_matrix))) %>%
  pivot_longer(cols = -var1,
               names_to = "var2",
               values_to = "distance") %>%
  mutate(var1 = fct_relevel(var1, var_order),
         var2 = fct_relevel(var2, var_order)) %>%
  ggplot(aes(x = var1, y = var2, fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")



# loadings from PCA -------------------------------------------------------

loadings_cb <- pca_cb %>%
  tidy(matrix = "loadings") %>%
  filter(PC <= 7) %>%
  group_by(column) %>%
  mutate(tot_abs_value = sum(abs(value))) %>%
  ungroup() %>%
  #arrange(by = desc(tot_abs_value)) %>%
  filter(PC == 1) %>%
  select(column, tot_abs_value)

weights_cb <- t(loadings_cb) %>%
  row_to_names(row_number = 1) %>%
  as_tibble()


  ### Rating

ratings_cb <- list()
for (i in seq(1, nrow(pl_cb))) {
  ratings_cb[i] <- rowSums(slice(model_cb, i) * as.numeric(weights_cb))
}
ratings_cb <- ratings_cb %>%
  as_tibble_col(column_name = "Rating") %>%
  mutate(Player = pl_cb$Player, Rating = as.numeric(Rating)) %>%
  select(Player, Rating) %>%
  arrange(by = desc(Rating))
