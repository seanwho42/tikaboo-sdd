library(tidyverse)
library(ggmap)
library(geosphere)

# reformatted so it can be read in easily
pedigree = read_delim('franz-2024/pedigree.txt', '  ') %>% clean_names() %>% 
  # add column for ease later on with visualization and such
  mutate(selfed = (sire == dam) & !is.na(dam)) %>%
  filter(!is.na(sire))

pedigree




# # reading in parentage lods to rule out any pedigree relationships with bad LOD scores, if they exist
# parentage_lods <- read_csv("franz-grouped/all-clusters/cloneless/with_selfing/different-nmax/parentage.csv")
# parentage_lods

# reading in parentage lods to rule out any pedigree relationships with bad LOD scores, if they exist
parentage_lods <- read_csv("franz-2024/parentage.csv") %>% clean_names()
parentage_lods



filtered_parentage_lods <- parentage_lods %>%
  filter(lod > 2)

filtered_parentage_lods
  
# joining parentage and pedigree to get pedigree relationships with acceptable LOD scores
pedigree

filtered_franz <- inner_join(pedigree, filtered_parentage_lods, by = c("id" = "offspring", "sire" = "parent_1", "dam" = "parent_2")) %>%
  rename('Tree' = 'id') %>% 
  # making parent type sire or dam for now, but could be overwritten with max, min, selfed, etc
  gather(key='parent_type', value='parent_id', sire, dam)


names(filtered_franz)
nrow(filtered_franz)
?gather()


coords

# filtering by high lod scores, gathering parent ids,
# removing na parent matches then doing an inner join off of parent id
convenient_parents_to_join <- coords %>%
  rename(parent_id = Tree)

head(convenient_parents_to_join)

convenient_parents_to_join$parent_id
filtered_franz$parent_id


franz_coords <- inner_join(filtered_franz, convenient_parents_to_join, by="parent_id") %>%
  rename(parent_x = X, parent_y = Y) %>%
  inner_join(y=coords, by="Tree") %>%
  rename(offspring_x = X, offspring_y = Y) %>% 
  select(Tree, parent_id, parent_x, parent_y, offspring_x, offspring_y, selfed) %>% 
  distinct()


names(franz_coords)
nrow(franz_coords)

franz_coords %>% write_csv('franz_coords_filtered.csv')

franz_coords

?inner_join
?gather

# stamen maps were phased out of support sometime in 2024, I think.. need to use google API instead
# franz_map_area <- get_stamenmap(bbox=c(
#   left = min(franz_coords$offspring_x - 0.003),
#   bottom = min(franz_coords$offspring_y - 0.003),
#   right = max(franz_coords$offspring_x + 0.003),
#   top = max(franz_coords$offspring_y + 0.003)),
#   zoom = 14,
#   maptype = 'terrain')

summary(franz_coords)

# needed to change how we determine map area for get_googlemap so doing it like this

map_lon_center = (max(franz_coords$offspring_x)+min(franz_coords$offspring_x))/2
map_lat_center = (max(franz_coords$offspring_y)+min(franz_coords$offspring_y))/2

map_center = c(
  lon = (max(franz_coords$offspring_x)+min(franz_coords$offspring_x))/2,
  lat = (max(franz_coords$offspring_y)+min(franz_coords$offspring_y))/2
  )

?get_googlemap()

#
# AIzaSyCOTXxcPzKTjJCub58MglbKZxeHvSAKFvY
franz_map_area <- get_googlemap(center = c(lon = map_lon_center, lat = map_lat_center),
  zoom = 13,
  maptype = 'terrain')

?get_googlemap

ggmap(franz_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(franz_coords,
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y),
               arrow = arrow(
                 length = unit(0.01, "npc"),
                 type = "closed",
                 angle = 20),
               size=1)
# now we need to do analysis for the distances

franz_distances <- franz_coords %>%
  mutate(offspring_coord_vector=paste(as.character(offspring_x), as.character(offspring_y))) %>%
  mutate(parent_coord_vector=paste(as.character(parent_x), as.character(parent_y)))

# this section was just troubleshooting
head(franz_distances)
distm(as.vector(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)))
franz_coords
?strsplit

franz_distances$offspring_coord_vector[1]
strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)

c(franz_distances$offspring_coord_vector[1])
c(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T))
length(unlist(strsplit(franz_distances$offspring_coord_vector[1], " ", fixed=T)))

# okay everything above is messy and overcomplicated but this is going to work so I'm not going to mess with it..
nrow(franz_distances)
head(franz_distances)


distances_m = tibble(offspring_id = "some_id", parent_id = "other_id", distance = 0)
distances_m
for (i in 1:(nrow(franz_distances)) ){
  distances_m <- distances_m %>% add_row(
    offspring_id = franz_distances$Tree[i],
    parent_id = franz_distances$parent_id[i],
    distance = as.numeric(distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[i], " ", fixed=T))),
                     as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[i], " ", fixed=T)))))
    )
  #print(i)
  #print(franz_distances$Tree[i])
  #print(as.numeric(distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[i], " ", fixed=T))),
              #as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[i], " ", fixed=T))))))
}

distances_m

distm(as.numeric(unlist(strsplit(franz_distances$offspring_coord_vector[3], " ", fixed=T))),
      as.numeric(unlist(strsplit(franz_distances$parent_coord_vector[3], " ", fixed=T))))

distances_m <- distances_m %>% filter(offspring_id != "some_id")

distances_m


# I know the code is a mess, sorry if you are looking at this in the future...
# ran into a bunch of issues with how the vectors were wanting to do things so
# I made a new column with the coords for offspring and coords for parent and
# and then did string splitting

# now lets join this onto franz_filtered
trees_distances <- inner_join(distances_m, franz_coords, by = c("offspring_id"="Tree", 
                                               "parent_id"="parent_id"))
trees_distances

# todo: can use trees_distances to run linear regression if we want to analyze SDD in the context of lat/longitude of parent

# will want to leave out the stuff from lower down since there weren't really
# anyinferred relationships there, it had slightly different methodology, and
# the one example is the only super long SDD example



trees_distances %>%
  summarize(median_d = median(distance),
            mean_d = mean(distance),
            sd_d = sd(distance),
            max_d = max(distance),
            min_d = min(distance),
            n = n())

# histograms
trees_distances %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", binwidth=200) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x='Distance (m)', y='Density', title='Distribution of estimated dispersal distance')

# filter out the super big ones for clarity in visualization
trees_distances %>%
  filter(distance < 500, distance != 0) %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", binwidth=20) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x='Distance (m)', y='Density', title='Distribution of estimated dispersal distance')


trees_distances

# NEW GRAPHS FOR MAX/MIN VALUES
min_trees_distances <-
  trees_distances %>%
  group_by(offspring_id) %>%
  filter(selfed == F) %>% 
  summarize(offspring_id, parent_id, distance = min(distance), parent_x, parent_y, offspring_x, offspring_y, selfed)

summary(min_trees_distances)

min_trees_distances %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", binwidth=200) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x='Minimum distances (m)', y='Density', title='Distribution of estimated dispersal distance minima')


# NOW MAX

max_trees_distances <-
  trees_distances %>%
  group_by(offspring_id) %>%
  filter(selfed == F) %>% 
  summarize(offspring_id, parent_id, distance = max(distance), parent_x, parent_y, offspring_x, offspring_y, selfed)

summary(max_trees_distances)

max_trees_distances %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", binwidth=200) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x='Maximum distances (m)', y='Density', title='Distribution of estimated dispersal distance maxima')

t.test(min_trees_distances$distance, max_trees_distances$distance)

#todo filter these out so that we are only focusing on those which have one instance recorded

# feasible_distances = as.vector(trees_distances %>% filter(is.na(tt)) %>% select(distance))
# feasible_distances
#mean(feasible_distances)

# finding z score of 2006 article distance in relation to this data
# z = (x-mean)/sd
# for some reason it's not liking mean(feasible_distances) so I just used the
# values from the summarize earlier instead
# (30 - 960.3587)/2225
# -0.4181387
# note that the data doesn't seem normally distributed, so don't think a z test
# makes a lot of sense

mean(feasible_distances)



# looking into wilcox test? not working though
# this is for old data made so be cautious of hard coded numbers
trees_distances %>%
  filter(distance > 47.5) %>%
  summarize(above = n())
# half are above median, so wilcox should theoretically be valid


wilcox.test(x=trees_distances$distance, mu=30)

as.vector(trees_distances %>% filter(is.na(tt)) %>% select(distance))

?wilcox.test

# t test even though this isn't necessarily valid because of smaller sample size from pedigree
t.test(x=trees_distances$distance, mu=30.0) # t = 1.5898, df = 33, p-value = 0.1214



# visualizations for results page


trees_distances %>% select(distance) %>%
  ggplot(aes(x=distance)) +
  geom_boxplot() +
  theme_classic()



trees_distances %>%
  filter(is.na(tt)) %>%
  ggplot(aes(x = distance,y=after_stat(density))) +
  geom_histogram(fill="thistle2", bins=40) +
  geom_density(color = "red") +
  theme_classic() +
  labs(x="Distance (m)", y = "Density", title = "Distribution of Joshua tree parent-offspring pair distance")



# map cleaned up slightly
ggmap(franz_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(trees_distances %>% filter(distance > 10000),
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y),
               arrow = arrow(
                 length = unit(0.007, "npc"),
                 type = "closed",
                 angle = 20),
               size=1) +
  labs(title="Parental-offspring pairs of Joshua trees in Tikaboo Valley")
# arrows go from parent to offspring

?quantile


ggmap(franz_map_area) +
  # geom_point(geno_coords, mapping=aes(X, Y), size = 0.5) +
  geom_segment(trees_distances,
               mapping=aes(x=parent_x, y=parent_y, xend=offspring_x, yend=offspring_y),
               arrow = arrow(
                 length = unit(0.007, "npc"),
                 type = "closed",
                 angle = 20),
               size=1) +
  labs(title="Parental-offspring pairs of Joshua trees in Tikaboo Valley")


# okay now we can work on connecting the structure data

trees_distances %>%
  group_by(offspring_id) %>% 
  summarize(offspring_id, number_entries=n()) %>% 
  summary()

