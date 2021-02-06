# need to source on "global.R"
# muliple correspondence analysis
library("FactoMineR") 
library("factoextra")
##################################
# MULTIPLE CORRESPONDENCE ANALYSIS
## recode outcome variables to have "postive"=1, "negative"=0
df %<>%
  mutate(
    visible_feces = fct_rev(visible_feces),
    insects_obs = fct_rev(insects_obs),
    solidwaste_obs = fct_rev(solidwaste_obs),
    floor = fct_rev(floor),
    roor = roof,
    wall = wall,
    full_clogged = full_clogged,
    handwashing_obs = fct_rev(handwashing_obs))
# select outcome variables
outcomes <- df %>%
  select(
    visible_feces,
    insects_obs,
    solidwaste_obs,
    floor,
    roof,
    wall, 
    full_clogged,
    handwashing_obs
  )

# produce dummy variables for every category (one hot encoding)
dummy_outcomes <- df %>%
  to_dummy(
    visible_feces,
    insects_obs,
    solidwaste_obs,
    floor,
    roof,
    wall, 
    full_clogged,
    handwashing_obs,
    suffix = "label"
  )

# run MCA without outpout graph
mca_out <- MCA(outcomes,
               graph = FALSE)
# extract loadings (coordinates) and eigenvalue
scores <- get_mca_var(mca_out)$coord[, 1] * (-1)
eig <- get_eigenvalue(mca_out)[1]
# compute factor weights by normalizing
weights <- (scores - min(scores)) / sqrt(eig)
scores_tbl <- bind_cols("var_name"=names(weights), "mca_scores" = scores, "weights"=weights)
# multiply loadings with dummy variables
index_scores <- as.data.frame(t(t(dummy_outcomes) * weights))

# sum the scores and divide by number of factors
index_scores %<>%
  row_sums(n = 1) %>%
  mutate(sqi = rowsums / length(outcomes),
         sqi = (sqi - min(sqi)) / (max(sqi) - min(sqi)) * 100) 
# add to data frame
df %<>%
  bind_cols("sqi" = index_scores$sqi)

##################################
# MULTIPLE CORRESPONDENCE ANALYSIS BY Country
# select outcome variables
df_nest <- df %>%
  group_by(id_country) %>%
  nest()

df_nest %<>%
  mutate(
    ctry_outcomes = map(
      data,
      ~ select(
        .,
        visible_feces,
        insects_obs,
        solidwaste_obs,
        floor,
        roof,
        wall, 
        full_clogged,
        handwashing_obs
      )
    ),
    ctry_dmyoutcomes = map(
      ctry_outcomes,
      ~ to_dummy(
        .,
        visible_feces,
        insects_obs,
        solidwaste_obs,
        floor,
        roof,
        wall, 
        full_clogged,
        handwashing_obs,
        suffix = "label"
      )
    ),
    mca.out = map(ctry_outcomes, ~ MCA(., graph = F)),
    ctry_scores = map(mca.out, ~ get_mca_var(.)$coord[, 1] * (-1)),
    ctry_eig = map(mca.out, ~ get_eigenvalue(.)[1]),
    weights = map2(ctry_scores, ctry_eig, ~ (.x - min(.x)) / sqrt(.y)),
    var_scores = map2(weights, ctry_dmyoutcomes, ~ as.data.frame(t(t(
      .y
    ) * .x))),
    sums = map(var_scores, ~ row_sums(
      .x, n = 1, append = F, var = "var_sums"
    )),
    index = map2(sums, ctry_outcomes, ~ .x / length(.y)),
    sqi_ctry = map(index, ~ (.x - min(.x)) / (max(.x) - min(.x)) *
                     100)
  )

df_new <-
  bind_rows(
    "Kenya" = df_nest$data[[1]],
    "Ghana" = df_nest$data[[2]],
    "Bangladesh" = df_nest$data[[3]],
    .id = "id_country"
  )
sqi_byctry <-
  bind_rows(df_nest$sqi_ctry[[1]], df_nest$sqi_ctry[[2]], df_nest$sqi_ctry[[3]])

df <- bind_cols(df_new, sqi_byctry)
df %<>% rename(sqi_ctry = var_sums) 

# Additive quality indicator
df %<>% 
  row_sums(as.numeric(visible_feces)-1,
           as.numeric(insects_obs)-1,
           as.numeric(solidwaste_obs)-1,
           as.numeric(floor)-1,
           as.numeric(roof)-1, 
           as.numeric(wall)-1,
           as.numeric(full_clogged)-1,
           as.numeric(handwashing_obs)-1,
           n=1) %>% 
  rename("sqi_add" = rowsums)