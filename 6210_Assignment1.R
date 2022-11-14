# Primary Author: Thishanya Gunasekera
# Secondary Author: Corinne Jackson
# Last edited: 2022-11-14

# install.packages("tidyverse")
library(tidyverse)
# install.packages("vegan")
library(vegan)
# install.packages("tmap")
library(tmap)
# install.packages("gridExtra")
library(gridExtra)
# install.packages("dplyr")
library(dplyr)
# install.packages("sets")
library(sets, include.only = c("set"))
# install.packages("import")
import::from(sets, "%e%")

data("World")

######################################################

# We will use a function to do the processing of the American/African data for the plot because they use many of the same steps.

continent_mapping <- function(continent_vector, continent_name, continent_countries){
  # Get the number of instances for each country 
  new_col <- continent_vector %>%
    group_by(country) %>%
    count(country) %>%
    filter(!is.na(country))
  
  # Get world data for continent of interest                 
  world_data <- World[which(World$continent == continent_name),]
  
  # Continent specific filtering steps
  if(continent_name == "Africa"){
    world_data$sovereignt <- str_replace(string = world_data$sovereignt, pattern = "Republic of Congo", 
                                         replacement = "Republic of the Congo")
    world_data$sovereignt <- str_replace(string = world_data$sovereignt, pattern = "Ivory Coast", 
                                         replacement = "Cote d'Ivoire")
  }
  if(continent_name == "North America"){
    # Because we are using the sovereignt column and Puerto Rico is under the United States, the
    # we need to manually add it to the new_col dataframe with a value of 0 so that the countries
    # line up properly with world_data
    new_col <- rbind(new_col, data.frame(country = "Puerto Rico", n = 0))
    new_col <- new_col[order(new_col$country),]
    # Change the way that the United States string is written so that new_col and world_data match
    world_data$sovereignt <- str_replace(string = world_data$sovereignt, pattern = "United States of America", 
                                         replacement = "United States")
    # Specifically grab the data for Colombia since it will be excluded otherwise
    get_colombia <- World[which(World$name == "Colombia"),]
    world_data <- rbind(world_data, get_colombia)
  }
  
  # Split the countries from world_data into countries represented in our previous data and countries that are not
  our_data <- world_data[world_data$sovereignt %in% continent_countries,]
  other_data <- world_data[!world_data$sovereignt %in% continent_countries,]
  
  # Order the countries to match the data in new_col
  our_data <- our_data[order(our_data$sovereignt),] %>%
    add_column(Data_Points = NA, .after = "sovereignt")
  
  # Add the counts from new_col
  our_data$Data_Points <- new_col$n
  
  # Create a matching Data_Points column for the other countries and put 0 in the column
  other_data <- other_data[order(other_data$sovereignt),] %>%
    add_column(Data_Points = 0, .after = "sovereignt")
  
  # Append and return the dataframes
  our_data <- rbind(our_data, other_data)
}

######################################################

# Downloaded on September 26th 2022
croc <- read_tsv("bold_data.txt")

# Start with a quick overview of the information available. We start with the names and a summary of the columns in this data frame. 
str(croc)
summary(croc)
names(croc)

# We then look at some of the variables of interest. Here we find that there are no recorded values of 'habitat' (all values are NA) so this variable will be discarded
croc %>% summarise_at(c("habitat", "bin_uri", "species_name", "lat", "lon", "country"), n_distinct)

# With the information we gathered, we create a new data frame containing the variables of interest and summarize the new data frame
croc_simp <- croc[c(8, 20, 22, 47, 48, 55)]
names(croc_simp)
summary(croc_simp)

# Continuing the information summary by looking at the number of NA values in each column of the new data frame that were not seen in the summary
croc_simp %>% summarise_at(c("bin_uri", "species_name", "country"), ~ sum(is.na(.)))

# Created charts to see if there were any clear distinctions in geographical location based on latitude and longitude. We find a large gap between longitudes that are lower than -60 and higher than -20. This is the separation between information obtained in the Americas and Africa. We then plot the latitude and longitude values together to get the general distribution
hist(croc_simp$lat)
hist(croc_simp$lon)

plot(croc$lon, croc$lat)

# Create a table that displays the number of bins associated with each species when NA is removed from the bin data
view(croc_simp %>%
       filter(!is.na(bin_uri)) %>%
       group_by(species_name) %>%
       count(species_name, sort = TRUE))

# Create a table that looks at the total number of data points within each listed country
view(croc_simp %>%
       group_by(country) %>%
       count(country))

# Create a table that looks at the number of bins in each country after removing all NA values in both columns
view(croc_simp %>%
       filter(!is.na(country)) %>%
       filter(!is.na(bin_uri)) %>%
       group_by(country) %>%
       count(bin_uri))

# Now we will create two data frames; one which contains data from America, and another which contains data from Africa
american_countries <- set("Colombia", "Cuba", "Mexico", "United States")
african_countries <- set("Cameroon", "Cote d'Ivoire", "Democratic Republic of the Congo", "Egypt", "Gabon", "Gambia", "Ghana", "Guinea", "Madagascar", "Mauritania", "Nigeria", "Republic of the Congo", "Senegal", "South Africa", "Uganda", "Zimbabwe")

Amer <- croc_simp[FALSE,]
Afr <- croc_simp[FALSE,]

for(i in 1:nrow(croc_simp)){
  if(croc_simp[i,]$country %e% american_countries){
    Amer <- rbind(Amer, croc_simp[i,])
  }
  else if(croc_simp[i,]$country %e% african_countries){
    Afr <- rbind(Afr, croc_simp[i,])
  }
}
Amer <- Amer[order(Amer$country),]
Afr <- Afr[order(Afr$country),]

######################################################

# Convert back to vectors
american_countries <- as.character(american_countries)
african_countries <- as.character(african_countries)

# Get the data processed so it is in the right format for mapping
Afr_data <- continent_mapping(Afr, "Africa", african_countries)
Amer_data <- continent_mapping(Amer, "North America", american_countries)

# From the 2 newly created data frames, we use tmap to make map shapes and fill the countries using the Data_Points column that was added. These maps are then outputted together using tmap_arrange. 
afr_plot <- tm_shape(Afr_data, name = Afr$country)+
  tm_borders() +
  tm_fill("Data_Points", title = "Number of Records", breaks = c(0, 1, 10, 20, 30, 40, 50))

amer_plot <- tm_shape(Amer_data, name = Amer$country)+
  tm_borders() +
  tm_fill("Data_Points", title = "Number of Records", breaks = c(0, 1, 20, 40, 60, 80))

tmap_mode("view")

tmap_arrange(afr_plot, amer_plot)

######################################################

# We then summarize these data frames. 
summary(Amer)
summary(Afr)

# We also want to determine the number of BINs and species in each set. The Amer set has 7 BINs (including NA) and 8 species while the Afr set has 9 BINs (including NA) and 4 species. 
length(unique(Amer$bin_uri))
length(unique(Amer$species_name))
unique(Amer$bin_uri)
unique(Amer$species_name)

length(unique(Afr$bin_uri))
length(unique(Afr$species_name))
unique(Afr$bin_uri)
unique(Afr$species_name)

# Here we create tables the show the instances of each BIN number with each species for both data set. 
table(Amer$bin_uri, Amer$species_name)
table(Afr$bin_uri, Afr$species_name)

# We visualize a table that counts the number of BINs in each country. One table for each data set
view(Amer %>%
       group_by(country) %>%
       count(bin_uri))

view(Afr %>%
       group_by(country) %>%
       count(bin_uri))

# We visualize a table that counts the number of BINs for each species
view(Amer %>%
       group_by(species_name) %>%
       count(bin_uri))

view(Afr %>%
       group_by(species_name) %>%
       count(bin_uri))

######################################################
# Plots

# Create a list of colours to be used in the next plot
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#990099", "#33FF33", "#990000", "#F5793A", "#99CC00", "#CC6600", "#663300", "#0F2080", "#85C080")

# Create a scatter plot that has bin number as the x axis and country as the y axis. Different coloured points represent different species
total_data <- ggplot(data = croc_simp) +
  geom_point(mapping = aes(x = bin_uri, y = country, colour = species_name), size = 5) +
  scale_colour_manual(values = cbbPalette) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

# Create a new data frame that filters out the NAs from country and BIN columns, then remake the above graph with the new dataframe
filtered_croc_simp <- croc_simp %>%
  filter(!is.na(bin_uri)) %>%
  filter(!is.na(country))

filtered_data <- ggplot(data = filtered_croc_simp) +
  geom_point(mapping = aes(x = bin_uri, y = country, colour = species_name), size = 5) +
  scale_colour_manual(values = cbbPalette) +
  theme(axis.text.x=element_text(angle=50, vjust=0.5))

# Visualize the 2 graphs together
grid.arrange(total_data, filtered_data, ncol=2)

######################################################
# Accumulation curves to determine the level of sampling in both geographical locations. 

# Create new data frames with the count of the BINs in each country and filter out the NAs from country and BIN columns. 
afr_country_count <- Afr %>%
  group_by(country, bin_uri) %>%
  count(bin_uri) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(bin_uri))

amer_country_count <- Amer %>%
  group_by(country, bin_uri) %>%
  count(bin_uri) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(bin_uri))

# Spread the BIN data to count the number of BINs in each country. Then set any NA values to 0. We then set the row names to country name rather and ensure that there is no seperate column with country information. 
afr_spread <- pivot_wider(data = afr_country_count, names_from = bin_uri, values_from = n)

afr_spread[is.na(afr_spread)] <- 0

afr_spread <- afr_spread %>%
  remove_rownames %>%
  column_to_rownames(var = "country")

# Do the same as above for the American data
amer_spread <- pivot_wider(data = amer_country_count, names_from = bin_uri, values_from = n)

amer_spread[is.na(amer_spread)] <- 0

amer_spread <- amer_spread %>%
  remove_rownames %>%
  column_to_rownames(var = "country")

# Plot both accumulation curves
AccumCurve_afr <- specaccum(afr_spread)
Afr_accum_curve <- plot(AccumCurve_afr, xlab="Countries", ylab= "BIN Richness", sub = "African Data Accumulation Curve")

AccumCurve_amer <- specaccum(amer_spread)
Amer_accum_curve <- plot(AccumCurve_amer, xlab = "Countries", ylab = "BIN Richness", sub = "American Data Accumulation Curve")

