# Getting Started
pacman::p_load(sf, tidyverse)

# Importing Geospatial Data
mpsz = st_read(dsn = "Hands-on_Ex1/data/geospatial", 
               layer = "MP14_SUBZONE_WEB_PL")

cyclingpath = st_read(dsn = "Hands-on_Ex1/data/geospatial", 
                      layer = "CyclingPathGazette")

preschool = st_read("Hands-on_Ex1/data/geospatial/PreSchoolsLocation.kml")

# Checking the Content of A Simple Feature Data Frame
st_geometry(mpsz)
glimpse(mpsz)
head(mpsz, n=5)

st_geometry(cyclingpath)
glimpse(cyclingpath)
head(cyclingpath, n=5)

st_geometry(preschool)
glimpse(preschool)
head(preschool, n=5)

# Plotting the Geospatial Data
plot(mpsz)
plot(st_geometry(mpsz))
plot(mpsz["PLN_AREA_N"])

plot(cyclingpath["PLANNING_1"])
plot(st_geometry(cyclingpath))

plot(preschool)
plot(st_geometry(preschool))

# Working with Projection
st_crs(mpsz)
mpsz3414 = st_set_crs(mpsz, 3414)
st_crs(mpsz3414)

preschool3414 = st_transform(preschool, 
                             crs = 3414)
st_geometry(preschool3414)

# Importing and Converting An Aspatial Data
listings = read_csv("Hands-on_Ex1/data/aspatial/listings.csv")
list(listings)
listings_sf = st_as_sf(listings,
                       coords = c("longitude", "latitude"),
                       crs=4326) %>%
  st_transform(crs = 3414)
glimpse(listings_sf)

# Geoprocessing with sf package
buffer_cycling = st_buffer(cyclingpath,
                            dist=5, nQuadSegs = 30)
buffer_cycling$AREA = st_area(buffer_cycling)
sum(buffer_cycling$AREA)

# Answer: 1774367 [m^2]

mpsz3414$`PreSch Count` = lengths(st_intersects(mpsz3414, preschool3414))
summary(mpsz3414$`PreSch Count`)
"""
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    4.00    7.09   10.00   72.00 
"""
top_n(mpsz3414, 1, `PreSch Count`)
# Tampines East

mpsz3414$Area = mpsz3414 %>% st_area()
mpsz3414 = mpsz3414 %>%
  mutate(`PreSch Density` = `PreSch Count`/Area * 1000000)
summary(mpsz3414$`PreSch Density`)
"""
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   5.154   6.449  10.908  35.602 
"""
top_n(mpsz3414, 1, `PreSch Density`)
# Downtown Core

# Exploratory Data Analysis (EDA)
hist(mpsz3414$`PreSch Density`)

ggplot(data=mpsz3414, 
       aes(x= as.numeric(`PreSch Density`))) +
  geom_histogram(bins=20, 
                 color="black", 
                 fill="light blue") +
  labs(title = "Are pre-school even distributed in Singapore?",
       subtitle= "There are many planning sub-zones with a single pre-school, on the other hand, \nthere are two planning sub-zones with at least 20 pre-schools",
      x = "Pre-school density (per km sq)",
      y = "Frequency")

ggplot(data=mpsz3414, 
       aes(y = `PreSch Count`, 
           x= as.numeric(`PreSch Density`)))+
  geom_point(color="black", 
             fill="light blue") +
  xlim(0, 40) +
  ylim(0, 40) +
  labs(title = "",
       x = "Pre-school density (per km sq)",
       y = "Pre-school count")





mpsz3414$`PreSch Count` = lengths(st_intersects(mpsz3414, preschool3414))
summary(mpsz3414$`PreSch Count`)
"""
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    4.00    7.09   10.00   72.00 
"""
top_n(mpsz3414, 1, `PreSch Count`)
# Tampines East

mpsz3414$Area = mpsz3414 %>% st_area()
mpsz3414 = mpsz3414 %>%
  mutate(`PreSch Density` = `PreSch Count`/Area * 1000000)
summary(mpsz3414$`PreSch Density`)
"""
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   5.154   6.449  10.908  35.602 
"""
top_n(mpsz3414, 1, `PreSch Density`)
# Downtown Core

# Exploratory Data Analysis (EDA)
hist(mpsz3414$`PreSch Density`)

ggplot(data=mpsz3414, 
       aes(x= as.numeric(`PreSch Density`))) +
  geom_histogram(bins=20, 
                 color="black", 
                 fill="light blue") +
  labs(title = "Are pre-school even distributed in Singapore?",
       subtitle= "There are many planning sub-zones with a single pre-school, on the other hand, \nthere are two planning sub-zones with at least 20 pre-schools",
       x = "Pre-school density (per km sq)",
       y = "Frequency")

ggplot(data=mpsz3414, 
       aes(y = `PreSch Count`, 
           x= as.numeric(`PreSch Density`)))+
  geom_point(color="black", 
             fill="light blue") +
  xlim(0, 40) +
  ylim(0, 40) +
  labs(title = "",
       x = "Pre-school density (per km sq)",
       y = "Pre-school count")
