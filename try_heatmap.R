

# 1 -----------------------------------------------------------------------

q <- p %>%
  mutate(value = ifelse(value >= 10, value, NA)) %>%
  mutate(value = log(value)) %>%
  filter(!is.na(value))

q0 <- graph_from_data_frame(q) %>%
  as_adj(attr = "value")
eigen <-  eigen(q0)

eigenvec2_sort <- data.frame(eigen = eigen$vectors[, length(eigen$values) - 1]) %>%
  mutate(row = row_number(),
         names = q0@Dimnames[[1]]) %>%
  arrange(eigen)

eigen_names <- eigenvec2_sort %>% pull(names)

sss <- quantile(diff(eigenvec2_sort %>% pull(eigen) %>% Mod()), 0.95)
cummunity_df <- eigenvec2_sort %>%
  mutate(community = c(0, diff(Mod(eigen)) > sss) %>% cumsum()) %>%
  select(names, community)

lala <- q %>%
  mutate(V1_chr = as.character(V1),
         V2_chr = as.character(V2),
         V1 = factor(V1, levels = eigen_names),
         V2 = factor(V2, levels = eigen_names)) %>%
  left_join(cummunity_df, by = c("V1_chr" = "names")) %>%
  left_join(cummunity_df, by = c("V2_chr" = "names")) %>%
  mutate(community = ifelse(community.x == community.y, community.x, NA),
         community = ifelse(!is.na(value), community, NA))



qp <- lala %>%
  ggplot(aes(V1, 
             V2, 
             alpha = value, 
             fill = factor(community))) + #, 
  geom_tile(color = "grey50") + #
  scale_alpha(range = c(0.5, 1)) +
  scale_fill_brewer(palette = "Set1", na.value = "grey50") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = "none", alpha = "none") +
  coord_fixed() +
  labs(x = NULL, y = NULL, 
       title = "1", 
       subtitle = "1")
qp

plot_ly(q, 
        x = ~V1, 
        y = ~V2,
        z = ~value, 
        type = "heatmap")


ggplot(p) +
  geom_boxplot(aes(y = value))
# 2 -----------------------------------------------------------------------

d <- data.Traffic
p <- combineTable(d, "Col_Area", "Del_Area")
