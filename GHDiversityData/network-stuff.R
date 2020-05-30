
edges <- tbl_df( tab ) %>% 
  group_by( article ) %>%
  do( {    
    tmp <- combn( sort(.$user),  m = 2 )
    data.frame( a = tmp[1,], b = tmp[2,], stringsAsFactors = FALSE )
  } ) %>%
  ungroup

edges <- ksl %>%
  group_by( a, b ) %>%
  summarise( article_in_common = length(article) ) %>%
  ungroup

library(igraph)
g <- graph.data.frame( select(edges, a, b, weight = article_in_common), directed = FALSE )

plot(g)



df <- user.gender %>%
  subset(select = c("project_id", "windows", "window_idx", "num_team", "team", "genders")) %>%
  mutate(sizeClass=
           ifelse(num_team < 11, "small", 
                  ifelse(num_team > 10 & num_team < 30, "medium", 
                         "large"
                  ))) %>%
  subset(sizeClass == "large") %>% 
  filter(windows==window_idx)

df.g <- graph.data.frame(d = df, directed = FALSE)

plot(df.g)
