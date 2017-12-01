require(tidyverse)
require(reldist)

# read in data sourced from Wikipedia tables at:
# https://en.wikipedia.org/wiki/List_of_U.S._states_by_historical_population

data1 = read.csv("/1790-1860.csv", check.names = FALSE, stringsAsFactors = FALSE)
data2 = read.csv("/1870-1950.csv", check.names = FALSE, stringsAsFactors = FALSE)
data3 = read.csv("/1960-2010.csv", check.names = FALSE, stringsAsFactors = FALSE)

# read in 2040 projected populations
projection = read.csv("/projection.csv", check.names = FALSE, stringsAsFactors = FALSE)

# join datasets
all_data = data1 %>% inner_join(data2, by = "Name") %>% inner_join(data3, by = "Name") %>% inner_join(projection, by = "Name")

# melt data
state.pops = all_data %>% gather("year", "population", 3:26)
# clean up data
state.pops = state.pops %>% mutate(year = as.numeric(year), Admitted = as.numeric(Admitted))
state.pops = state.pops %>% mutate(population = as.numeric(gsub(",","",population)))
# The census includes populations for states before they were admitted. 
# Exclude these (since they didn't have Senate representation yet).
state.pops = state.pops %>% filter(population > 0 & year>= Admitted)
# Exclude the national totals
state.pops = state.pops %>% filter(Name != "United States")

# calculate senators per resident
state.pops = state.pops %>% mutate(year = as.numeric(year),
                                   senators = ifelse(Name == "DC", 0, 2/population))

# find all years with state admissions
admission.dates = state.pops %>% group_by(Admitted) %>% select(Admitted)%>% summarise()

# calculate gini coefficient for each census year. ungroup for plotting purposes later.
yearly.ginis = state.pops %>%
                group_by(year) %>%
                summarise(gini.coefficient = gini(senators, weights = population)) %>%
                ungroup()

# plot ginis over time (projection is included in dataset - exclude it)
yearly.ginis %>% filter(year < 2015) %>% ggplot(aes(x=year, y = gini.coefficient))+
  geom_line()+
  xlab("Year (red lines = new states)")+
  ylab("Gini coefficient")+
  ggtitle("Inequality of Senate power, 1790-2010")+
  theme(text=element_text(size=16))

# same as above but with red vertical lines for state admissions
yearly.ginis %>% filter(year < 2015) %>% ggplot(aes(x=year, y = gini.coefficient))+
  geom_line()+
  xlab("Year (red lines = new states)")+
  ylab("Gini coefficient")+
  ggtitle("Inequality of Senate power, 1790-2010")+
  theme(text=element_text(size=16))+
  geom_vline(data = admission.dates, aes(xintercept = Admitted),color="red")

