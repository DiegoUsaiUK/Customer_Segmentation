# turn off locale-specific sorting for messages in English
Sys.setlocale("LC_TIME", "C")

# Loading the Packages

library(tidyverse)
library(lubridate)
library(knitr)
library(readxl)
library(broom)
library(umap)
library(ggrepel)
library(plotly)

# The Data



offers_tbl <- read_excel('../00_data/WineKMC.xlsx', sheet = 'OfferInformation')
offers_tbl <- offers_tbl %>% 
        set_names(c('offer', 'campaign', 'varietal', 'min_qty_kg', 
                    'disc_pct','origin', 'past_peak'))


transac_tbl <- read_excel('../00_data/WineKMC.xlsx', sheet = 'Transactions')
transac_tbl <- transac_tbl %>% 
        set_names(c('customer', 'offer'))

# convert to `User-Item format`

wine_tbl <- transac_tbl %>% 
    left_join(offers_tbl) %>% 
    mutate(value = 1) %>%
    spread(customer,value, fill = 0) 

# Clustering the Customers

user_item_tbl <- wine_tbl[,8:107]

set.seed(196)             # for reproducibility of clusters visualisation with UMAP
kmeans_obj <- user_item_tbl %>%  
    kmeans(centers = 5,   # number of clusters to divide customer list into
           nstart = 100,  # specify number of random sets to be chosen
           iter.max = 50) # maximum number of iterations allowed - 

# quickly inspect the model 
glance(kmeans_obj) %>% glimpse()

# I build a function for a set number of `centers` (4 in this case) 

kmeans_map <- function(centers = 4) {
    user_item_tbl %>%  
        kmeans(centers = centers, nstart = 100, iter.max = 50)
}

# and check that is working on `glance()`
4 %>% kmeans_map() %>%  glance()

# Then, I create a __nested tibble__, which is a way of "nesting" columns inside a data frame.

kmeans_map_tbl <- tibble(centers = 1:15) %>%  # create column with centres 
    mutate(k_means = centers %>% 
               map(kmeans_map)) %>%           # iterate `kmeans_map` row-wise to gather 
                                              # kmeans models for each centre in column 2
    mutate(glance = k_means %>%  
               map(glance))                   # apply `glance()` row-wise to gather each
                                              # model’s summary metrics in column 3

kmeans_map_tbl %>% glimpse()


# Last, I can build a `scree plot` and look for the "elbow" on the graph, 
# the point where the number of additional clusters seem to level off. 

kmeans_map_tbl %>% 
    unnest(glance) %>%                           # unnest the glance column
    select(centers, tot.withinss) %>%            # select centers and tot.withinss
    
    ggplot(aes(x = centers, y = tot.withinss)) + 
    geom_line(colour = 'grey30', size = .8) +
    geom_point(colour = 'green4', size = 3) +
    geom_label_repel(aes(label = centers), 
                     colour = 'grey30') +
    theme_bw() +
    labs(title = 'Scree Plot')

# Visualising the Segments


# First, I create a umap object and pull out the `layout` argument 
# (containing coordinates that can be used to visualize the dataset), 
# change its format to a tibble and attach the `offer` column from the `wine_tbl`.

umap_obj <- user_item_tbl %>%  umap() 

umap_tbl <- umap_obj$layout %>% 
    as_tibble() %>%                       # change to a tibble
    set_names(c('x', 'y')) %>%            # remane columns
    bind_cols(wine_tbl %>% select(offer)) # attach offer reference

# Then, I `pluck` the 5th kmeans model from the __nested tibble__, 
# attach the _cluster_ argument from the `kmeans` function to the output, 
# and join offer and cluster to the umap_tbl.

umap_kmeans_5_tbl <- kmeans_map_tbl %>% 
    pull(k_means) %>%
    pluck(5) %>%                          # pluck element 5 
    broom::augment(wine_tbl) %>%          # attach .cluster to the tibble 
    select(offer, .cluster) %>% 
    left_join(umap_tbl, by = 'offer')     # join umap_tbl to clusters by offer


# At last, I can visualise the UMAP'ed projections of the clusters. 

g <- umap_kmeans_5_tbl %>% 
    mutate(label_text = str_glue('Offer: {offer}
                                  Cluster: {.cluster}')) %>%
    
    ggplot(aes(x,y, colour = .cluster)) +
    geom_point(aes(text = label_text)) +
    theme_light() +
    labs(title    = 'UMAP 2D Projections of K-Means Clusters',
         caption  = "") +
    theme(legend.position = 'none')

ggplotly(g, tooltip = "label_text")

# Evaluating the Clusters

# Let's first bring all information together in one data frame.

cluster_trends_tbl <- wine_tbl %>%
    left_join(umap_kmeans_5_tbl) %>%
    arrange(.cluster) %>%
    select(.cluster, offer:past_peak)


### Cluster 1 & 2
# cluster 1:  high volumes of sparkling wines (Champagne and Prosecco) 
# cluster 2: low volume purchases of different varieties

cluster_trends_tbl %>% 
    filter(.cluster ==1 | .cluster ==2) %>% 
    count(.cluster, varietal, origin, min_qty_kg, disc_pct) %>%
    select(-n) %>% 
    kable()


### Cluster 3 & 4

# Cluster 3: Pinot Noir 
# Cluster 4: French Champagne in high volumes

cluster_trends_tbl %>% 
    filter(.cluster ==3 | .cluster ==4 ) %>% 
    group_by() %>%
    count(.cluster, varietal, origin, min_qty_kg, disc_pct) %>%
    select(-n) %>%
    kable()


### Cluster 5

# more difficult to categorise . 
# only clear trend: segment picked up all available Cabernet Sauvignon offers.

cluster_trends_tbl %>% 
    filter(.cluster ==5 ) %>% 
    count(.cluster, varietal, origin, min_qty_kg, disc_pct) %>%
    select(-n) %>% 
    kable()

