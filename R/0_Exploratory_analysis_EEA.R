require('dplyr')
# total dataset ----

db = readRDS('./Data/explanatory vaars/All_dataset.rds')

db = db[, 1:5]
db_unique =  aggregate(resultObservedValue ~ lon + lat + monitoringSiteIdentifier + year, db, FUN = mean)

# total number of points per year
no_points = db %>%
  group_by(year) %>%
  count()
# total number of unique stations per year
no_points_unique = db_unique %>%
  group_by(year) %>%
  count()

head(db)
