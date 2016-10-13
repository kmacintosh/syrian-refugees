library(tidyverse)
library(sp)
library(rgeos)
library(countrycode)
library(stringr)
library(ggalt)

syrian_data <- read_csv("472_a-2/data/syrian_refugee_data_large.csv", na = c("","*")) 
population_data <- read_csv("472_a-2/data/population-data.csv")
education_data <- read_csv("472_a-2/data/education_data.csv", na = c("", ".."))
gdp_data <- read_csv("472_a-2/data/gdp_data.csv", na = c("", ".."))
sr_canada2016 <- read_csv("472_a-2/data/syrians_canada2016.csv")
sr_eu2016 <- read_csv("472_a-2/data/eu_syria_migr.csv", na = c(":", ""))
refugees_canada <- read_csv("472_a-2/data/syrians_canada2016.csv")

# merging and creating new columns
syrian_data <- syrian_data %>% 
  mutate(total_recognized = convention_status + coalesce(nonconvention_status, 0L),
         rejected_closed = rejected + otherwise_closed) 

#selecting countries with greater than 100 asylum-seekers 2011-2014
country_selector <- syrian_data %>% 
  filter(year >= 2011, year <=2014) %>% 
  group_by(country) %>% 
  summarise(total_recognized = sum(total_recognized, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(total_recognized >= 100) %>% 
  mutate(rank = rank(desc(total_recognized))) %>% 
  select(-total_recognized)

#cleaning sr_eu2016
eu_2016 <- sr_eu2016 %>% 
  filter(CITIZEN == "Syria",
         ASYL_APP == "First time applicant",
         !GEO %in% c("European Union (28 countries)", "Total")) %>% 
  mutate(Value = str_replace_all(Value, " ", ""),
         Value = as.integer(Value),
         GEO = ifelse(GEO == "Germany (until 1990 former territory of the FRG)",
                      "Germany", GEO)) %>% 
  group_by(GEO) %>% 
  summarise(mean_monthly_rec = mean(Value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(country = GEO)
# adding in Canada and US
eu_2016 <- data_frame(country = c("Canada", "United States"), 
           mean_monthly_rec = c(31919/11, 12400/11)) %>% 
  bind_rows(eu_2016) %>% 
  mutate(mean_monthly_rec = round(mean_monthly_rec))  
  
# Cleaning
syrian_data <- syrian_data %>% 
  # filter for countries, join with country ranks, gets rid of other countries
  inner_join(country_selector, by = "country") %>%
  # RSD type to uppercase
  mutate(rsd_type = toupper(rsd_type)) %>% 
  # filter for appropriate RSD type
  filter(rsd_type %in% c("G / FI", "G / NA", "U / FI", "G / IN")) %>% 
  # filter for years since 2007
  filter(year >= 2007) %>% 
  # change names to match world bank dataset
  mutate(country = ifelse(country == "Rep. of Moldova", "Moldova", country),
         country = ifelse(country == "Czech Rep.", "Czech Republic", country),
         country = ifelse(country == "United States of America", "United States", country))


write_csv(syrian_data, "472_a-2/output/sr-data1_cleaned")

#stripping table to columns that will be used for graphs
syrian_data <- syrian_data %>% 
  group_by(country, year) %>% 
  summarise(rank = min(rank),
            total_decisions = sum(total_decisions, na.rm = TRUE), 
            total_recognized = sum(total_recognized, na.rm = TRUE), 
            rejected_closed = sum(rejected_closed, na.rm = TRUE),
            applied = sum(applied, na.rm = TRUE)) %>% 
  ungroup()

         
#filtering and joining population data
population_data <- population_data %>%
  #changes country variable name, and gathers data from wide to long 
  select(country = `Country Name`, starts_with("2")) %>% 
  gather(year, population, -country) %>% 
  mutate(year = as.integer(year),
         population = as.numeric(population))

syrian_data <- syrian_data %>% 
  inner_join(population_data, by = c("country", "year")) 

names(education_data)[1] <- "country"
education_data <- education_data %>%
  #changes country variable name, and gathers data from wide to long 
  filter(`Series Code` == "SE.XPD.TOTL.GD.ZS") %>% 
  select(country, starts_with("2")) %>% 
  gather(year, ed_expd, -country) %>% 
  mutate(year = as.integer(substr(year, 1, 4)))

syrian_data <- syrian_data %>% 
  inner_join(education_data, by = c("country", "year")) 

names(gdp_data)[1] <- "country"
gdp_data <- gdp_data %>%
  #changes country variable name, and gathers data from wide to long 
  select(country = `Country Name`, starts_with("2")) %>% 
  gather(year, gdp, -country) %>% 
  mutate(year = as.integer(substr(year, 1, 4)),
         gdp = as.numeric(gdp))

syrian_data <- syrian_data %>% 
  inner_join(gdp_data, by = c("country", "year"))

write_csv(syrian_data, "472_a-2/output/sr-data2_final-useable")

syrian_data1 <- syrian_data %>% 
  group_by(country) %>% 
  summarise(overall_by_country = sum(total_decisions, na.rm = TRUE))

# plotting
# stacked bar
country_totals <- syrian_data %>% 
  mutate(country = ifelse(rank <= 8 | country %in% c("Canada", "United States"),
                          country, "Other")) %>% 
  filter(year >= 2011) %>% 
  group_by(year, country) %>% 
  summarise(total_recognized = sum(total_recognized, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(country = factor(country, levels = c("Other", "Germany", "Sweden", 
                                              "Bulgaria", "Turkey", "Netherlands",
                                              "Lebanon", "Denmark", "Jordan", 
                                              "United States", "Canada" )),
         year = factor(year, levels = 2015:2011)) %>%  
  arrange(country)

syria_stackedbar <-  country_totals %>% 
  ggplot(aes(x = year, y = total_recognized, fill = factor(country))) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        text = element_text(family = "Helvetica Neue Light")) 





decision_outcome_bar <- syrian_data %>% 
  filter(rank <= 7 | country %in% c("Canada", "United States"),
         year >= 2011) %>% 
  group_by(country) %>% 
  summarise(total_recognized = sum(total_recognized, na.rm = TRUE),
            rejected_closed = sum(rejected_closed, na.rm = TRUE)) %>% 
  gather(decision_outcome, value, total_recognized, rejected_closed) %>%
  mutate(country = factor(country),
         country = reorder(country, -value, FUN = sum)) %>%  
  ggplot(aes(x = country, y = value, fill = decision_outcome)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000))


change_in_canada <- syrian_data %>% 
  filter(country == "Canada", 
         year >= 2011) %>% 
  select(year, total_recognized) %>% 
  bind_rows(data_frame(year = 2016, total_recognized = 16875)) %>% 
  ggplot(aes(x = factor(year), y = total_recognized)) +
  geom_bar(stat = "identity", fill = "#D52B1E") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000)) +
  coord_flip() +
  theme(axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),
      legend.title = element_blank(),
      text = element_text(family = "Helvetica Neue Light")) 

#polar graph
 comparison_polar <- syrian_data %>% 
  filter(rank <= 7 | country %in% c("Canada", "United States"),
         year >= 2011) %>%  
  group_by(country) %>% 
  summarise(total_recognized = sum(total_recognized, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(country = factor(country, levels = c("Germany", "Sweden", 
                                              "Lebanon", "Netherlands","Bulgaria",  
                                              "Denmark","Turkey", 
                                              "United States", "Canada" ))) %>% 
  arrange(country) %>% 
  ggplot(aes(x = country, y = total_recognized, fill = country)) +
  geom_bar(stat = "identity") +
  coord_polar() +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        text = element_text(family = "Helvetica Neue Light")) 
 
#canada map
 
refugees_canada <- refugees_canada %>% 
  mutate(refugees_pc = 10000 * refugee_resettled / population,
         resettled_binned = cut(refugees_pc,
                                breaks = c(0, 5, 7.5, 10, 15, 20)))
can_prov <- raster::shapefile("472_a-2/data/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.shp")
can_prov <- subset(can_prov, iso_a2 == "CA")
row.names(can_prov) <- can_prov$postal
# in order to map in ggplot, must convert to dataframe
can_prov_df <- fortify(can_prov)
can_prov_df <- can_prov_df %>% 
  left_join(refugees_canada, by = c("id" = "postal"))

ggplot(can_prov_df) +
  geom_polygon(aes(long, lat, group = group), 
               fill = "grey70", color = "white", size = 0.1) + 
  geom_polygon(aes(long, lat, group = group, fill = resettled_binned), 
               color = "white", size = 0.1) +
  coord_proj("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        text = element_text(family = "Helvetica Neue Light"))

ggplot(refugees_canada, aes(x= 10000 * refugee_resettled / population)) +
  geom_histogram()

comparison_polar2016 <- eu_2016 %>% 
  filter(country %in% c("Canada", "United States", "Germany", "Sweden",
                        "Greece")) %>% View
  mutate(country = factor(country, levels = c("Germany", "Canada", "Sweden", 
                                              "Greece", "United States"))) %>% 
  arrange(country) %>% 
  ggplot(aes(x = country, y = mean_monthly_rec, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        text = element_text(family = "Helvetica Neue Light"))+
guides(fill = "none")

