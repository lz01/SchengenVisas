
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(ggrepel)
library(ade4)
library(rworldmap)

setwd("~/Desktop/L/Notes/SchengenVisas")
visas2018 = read.csv("Data/2018-data-for-consulates.csv",h=T,stringsAsFactors=F,na.strings="")
visas2017 = read.csv("Data/2017-data-for-consulates.csv",h=T,stringsAsFactors=F,na.strings="")

## Converting numerical variables to numerical

## Function convert_rate to convert character rates with "%" sign to numerical value

convert_rate = function(x)
{
return(as.numeric(str_replace(x,"%",""))/100)
}

## Function convert_numeric to convert character numbers with possible comma separators to numeric
convert_numeric = function(x)
{
  return(as.numeric(str_replace(x,",","")))

}

 ## Define the columns to convert to numerical
col_rate = str_subset(names(visas2018),regex("rate|share",ignore_case=T))
col_numeric =
names(visas2018)[!str_detect(names(visas2018),regex("rate|share",ignore_case=T)) & str_detect(names(visas2018),regex("atv|uniform|ltv",ignore_case=T))]


## We remove the rows where Schenge.State is NA. Some are completely uninformative and others don't follow the column format with words instead of numbers and totals
visas2018 = visas2018 %>% filter(!is.na(Schengen.State))
visas2017 = visas2017 %>% filter(!is.na(Schengen.State))

## We convert columns to numerical
visas2018 = visas2018 %>% mutate_at(col_rate,convert_rate) %>% mutate_at(col_numeric,convert_numeric)


## Read World population data
worldpop = read.csv("~/Desktop/L/Notes/WorldPopulation/Data/Population-EstimatesData.csv", stringsAsFactors=F,h=T,na.strings="")
## Keeping only the data on total population in 2018
worldpop = worldpop %>%
    filter(Indicator.Name == "Population, total") %>%
    select(Country = Country.Name,Population2018 = X2018) %>%
    mutate(Country = str_to_upper(Country))


## Find how many Countries in visas2018 are in worldpop dataset
## setdiff(visas2018$Country.where.consulate.is.located,worldpop$Country)
## There are 18 countries in visas2018 not present in worldpop

worldpop = worldpop %>%
    mutate(Country = recode(Country, `UNITED STATES` = "USA",
                            `EGYPT, ARAB REP.` = "EGYPT",
                            `SLOVAK REPUBLIC` = "SLOVAKIA",
                            `KOREA, DEM. PEOPLE’S REP.` = "NORTH KOREA",
                            `KOREA, REP.` = "SOUTH KOREA",
                            `VENEZUELA, RB` = "VENEZUELA",
                            `ST. LUCIA` = "SAINT LUCIA",
                            `CABO VERDE` = "CAPE VERDE",
                            `HONG KONG SAR, CHINA` = "HONG KONG S.A.R.",
                            `CONGO, DEM. REP.` = "CONGO (DEMOCRATIC REPUBLIC)",
                            `CONGO, REP.` = "CONGO (BRAZZAVILLE)",
                            `KYRGYZ REPUBLIC` = "KYRGYZSTAN",
                            `MACAO SAR, CHINA` = "MACAO S.A.R.",
                            `IRAN, ISLAMIC REP.` = "IRAN",
                            `SYRIAN ARAB REPUBLIC` = "SYRIA",
                            `LAO PDR` = "LAOS",
                            `WEST BANK AND GAZA` = "PALESTINIAN AUTHORITY"))

## I added all except Taiwan which I couldn't find

visas2018 = left_join(visas2018,worldpop,by=c("Country.where.consulate.is.located"="Country"))

## Checking NAs per column
colSums(is.na(visas2018))



## Number of Uniform visa applications by country, summing all cities if several
visas2018 %>%
    group_by(Country.where.consulate.is.located) %>%
    summarize(sum.Uniform.visas.applied.for = sum(Uniform.visas.applied.for, na.rm=T)) %>%
    top_n(20,sum.Uniform.visas.applied.for) %>%
    ggplot(aes(reorder(Country.where.consulate.is.located,sum.Uniform.visas.applied.for),sum.Uniform.visas.applied.for)) +
    geom_point(col = "blue") +
    geom_segment(aes(x = Country.where.consulate.is.located,
                 xend = Country.where.consulate.is.located,
                 y = 0,
                 yend = sum.Uniform.visas.applied.for)) +
    coord_flip() +
    xlab("Country") +
    ylab("Total number of uniform visa applications")



## Number of Uniform visa applications by country, divided by Country population size
## Which countries ask the more for visas, normalized by population size?
visas2018 %>%
    filter(!is.na(Population2018) & !is.na(Uniform.visas.applied.for)) %>%
    group_by(Country.where.consulate.is.located) %>%
    summarize(Uniform.applications.per.100000.person = 100000*sum(Uniform.visas.applied.for)/unique(Population2018)) %>%
    top_n(20,Uniform.applications.per.100000.person) %>%
 ggplot(aes(reorder(Country.where.consulate.is.located,Uniform.applications.per.100000.person),Uniform.applications.per.100000.person)) +
    geom_point(col = "blue") +
    geom_segment(aes(x = Country.where.consulate.is.located,
                 xend = Country.where.consulate.is.located,
                 y = 0,
                 yend = Uniform.applications.per.100000.person)) +
    coord_flip() +
    xlab("Country") +
    ylab("Number of uniform visa applications per 100000 people")





## Nb of consulates per country
visas2018 %>%
    group_by(Country.where.consulate.is.located) %>%
    summarize(nb_of_consulates = n()) %>%
    top_n(20,nb_of_consulates) %>%
    ggplot(.,aes(reorder(Country.where.consulate.is.located,nb_of_consulates),nb_of_consulates)) +
    geom_point(col = "blue") +
    geom_segment(aes(x = Country.where.consulate.is.located,
                 xend = Country.where.consulate.is.located,
                 y = 0,
                 yend = nb_of_consulates)) +
    coord_flip() +
    xlab("Country") +
    ylab("Number of consulates")


## Nb of consulates per country divided by population size
visas2018 %>%
    group_by(Country.where.consulate.is.located) %>%
    summarize(nb_of_consulates_per100000 = 100000* n()/unique(Population2018)) %>%
    top_n(20,nb_of_consulates_per100000) %>%
    ggplot(.,aes(reorder(Country.where.consulate.is.located,nb_of_consulates_per100000),nb_of_consulates_per100000)) +
    geom_point(col = "blue") +
    geom_segment(aes(x = Country.where.consulate.is.located,
                 xend = Country.where.consulate.is.located,
                 y = 0,
                 yend = nb_of_consulates_per100000)) +
    coord_flip() +
    xlab("Country") +
    ylab("Number of consulates per 100000 people")

##  Number of visa applications divided by number of consulates per country

visas2018 %>%
group_by(Country.where.consulate.is.located) %>%
summarize(Average_visa_app_per_consulate = sum(Total.ATVs.and.uniform.visas.applied.for)/n()) %>%
top_n(20,Average_visa_app_per_consulate) %>%
ggplot(aes(reorder(Country.where.consulate.is.located,Average_visa_app_per_consulate),Average_visa_app_per_consulate)) +
    geom_point(col = 4) +
    geom_segment(aes(x = Country.where.consulate.is.located,
                 xend = Country.where.consulate.is.located,
                 y = 0,
                 yend = Average_visa_app_per_consulate)) +
    coord_flip() +
    xlab("Country") +
    ylab("Average number of visa appications per consulate")

## Average number of visa applications per consulate for each country vs nb of consulates in the country
## Do some countries have few applications per consulate but a lot of consulates?
## Like US
countries_to_label = c("BELARUS", "ALGERIA", "MOROCCO", "INDIA", "TURKEY", "CHINA", "RUSSIAN FEDERATION", "AUSTRALIA"  , "BRAZIL",   "CANADA" ,  "GERMANY" , "ISRAEL" , "JAPAN"  , "USA")
country_list = sort(unique(visas2018$Country.where.consulate.is.located))
country_label = rep("",length(country_list))
country_label[country_list %in% countries_to_label] = sort(countries_to_label)
group_color = ifelse(country_label == "" ,"grey","black")
group_color[country_label == "USA"] = "red"
## I define alpha=0.5 for grey points to see more the density
group_alpha = rep(1,length(country_list))
group_alpha[group_color == "grey"] = 0.5


visas2018 %>%
	group_by(Country = Country.where.consulate.is.located) %>%
	summarize(Average_visa_app_per_consulate = 	sum(Total.ATVs.and.uniform.visas.applied.for,na.rm=T)/n(), Number_of_consulates = n()) %>%
	ggplot(aes(Number_of_consulates, Average_visa_app_per_consulate)) +
	geom_point(col = group_color, alpha = group_alpha) +
	geom_abline(aes(intercept=0,slope=550),col="grey",linetype="dashed") +
	geom_text_repel(aes(Number_of_consulates, Average_visa_app_per_consulate), label = country_label, size=3, box.padding = 0.5) +
    xlab("Number of consulates") +
    ylab("Average number of visa appications per consulate")


## Find out which countries to label
    ## Countries with label: BELARUS, ALGERIA, MOROCCO, INDIA, TURKEY, CHINA, USA, RUSSIAN FEDERATION
    ## Who has few avg nb and many consulates?
visas2018 %>%
	group_by(Country = Country.where.consulate.is.located) %>%
	summarize(Average_visa_app_per_consulate = 	sum(Total.ATVs.and.uniform.visas.applied.for,na.rm=T)/n(), Number_of_consulates = n()) %>%
	filter(Number_of_consulates>24 & Average_visa_app_per_consulate<5000)
## AUSTRALIA   BRAZIL   CANADA   GERMANY  ISRAEL  JAPAN   USA



## Nb of Schengen states that have a consulate in the country
## Dot plot
visas2018 %>%
    group_by(Country.where.consulate.is.located) %>%
    summarize(nb_of_states = n_distinct(Schengen.State)) %>%
    top_n(30,nb_of_states) %>%
    ggplot() +    geom_point(aes(reorder(Country.where.consulate.is.located,nb_of_states),nb_of_states)) +
    coord_flip() +
    xlab("Number of Schengen states with a consulate") +
    ylab("Country")

## List of countries with 20-25, 15-20, 10-15, 5-10, 1-5 consulates
nb_of_states = visas2018 %>%
    group_by(Country.where.consulate.is.located) %>%
    summarize(nb_of_states = n_distinct(Schengen.State)) %>%
    mutate(nb_of_states_cat = case_when(nb_of_states>20 ~ "21-25", nb_of_states<=20 & nb_of_states>15 ~ "16-20", nb_of_states<=15 & nb_of_states>10 ~ "11-15", nb_of_states<=10 & nb_of_states>5 ~ "06-10", nb_of_states<=5 ~ "01-05")) %>%
    group_by(nb_of_states_cat) %>%
    summarize(list_countries = list(Country.where.consulate.is.located))
nb_of_states %>%
    ggplot() +
    geom_col(aes(nb_of_states_cat,lengths(list_countries))) +
    xlab("Number of Schengen states with a consular presence in the country") +
    ylab("Number of countries")

## There are 11 countries with more than 20 Schengen states with a consular presence
"CANADA"             "CHINA"              "EGYPT"              "INDIA"    "ISRAEL"             "JAPAN"              "RUSSIAN FEDERATION" "TURKEY"  "UKRAINE"            "UNITED KINGDOM"     "USA"
## There are 26 countries with 16 to 20 states
 "ALGERIA"              "ARGENTINA"            "AUSTRALIA"            "BULGARIA"
 "CROATIA"              "CUBA"                 "ETHIOPIA"             "FRANCE"
 "GERMANY"              "INDONESIA"            "IRAN"                 "IRELAND"
 "KENYA"                "LEBANON"              "MEXICO"               "MOROCCO"
 "NIGERIA"              "PAKISTAN"             "ROMANIA"              "SAUDI ARABIA"
 "SERBIA"               "SOUTH AFRICA"         "SOUTH KOREA"          "THAILAND"
"UNITED ARAB EMIRATES" "VIETNAM"


## Rate of refused Uniform visas refused by recipient country for countries that asked for more than 100000 visas in the year
## There are 24 such countries
visas2018 %>%
    group_by(Country.where.consulate.is.located) %>%
    filter(sum(Uniform.visas.applied.for,na.rm=T)>=100000) %>%
    summarize(rate_refused = sum(Uniform.visas.not.issued,na.rm=T)/sum(Uniform.visas.applied.for,na.rm=T)) %>%
    ggplot() +
    geom_col(aes(reorder(Country.where.consulate.is.located,rate_refused),rate_refused)) +
    coord_flip() +
    ylab("Proportion of uniform visa applications refused") +
    xlab("Country of residence")


## Rate of refused Uniform visas refused by issuing Schengen state
visas2018 %>%
    group_by(Schengen.State) %>%
    summarize(rate_refused = sum(Uniform.visas.not.issued,na.rm=T)/sum(Uniform.visas.applied.for,na.rm=T)) %>%
    ggplot() +
    geom_col(aes(reorder(Schengen.State,rate_refused),rate_refused)) +
    coord_flip() +
    ylab("Proportion of uniform visa applications refused") +
    xlab("Schengen state issuing visas")

## Standard deviation of rate of refusal for Uniform visas per Schengen state, after filtering out consulates that recieve less than 10 applications
visas2018 %>%
    filter(Uniform.visas.applied.for>10) %>% group_by(Schengen.State) %>%
    summarize(sd_rate_refused = sd(Uniform.visas.not.issued/Uniform.visas.applied.for,na.rm=T)) %>%
    ggplot() +
    geom_col(aes(reorder(Schengen.State,sd_rate_refused),sd_rate_refused)) +
    coord_flip() +
    ylab("Std deviation of the proportion of refused uniform visa applications") +
    xlab("Schengen state issuing visas")

## Same but with colour gradient
visas2018 %>%
    filter(Uniform.visas.applied.for>10) %>% group_by(Schengen.State) %>%
    summarize(sd_rate_refused = sd(Uniform.visas.not.issued/Uniform.visas.applied.for,na.rm=T)) %>%
    ggplot() +
    geom_col(aes(reorder(Schengen.State,sd_rate_refused),sd_rate_refused,fill=sd_rate_refused)) +
    scale_fill_gradient("Blues",trans = "reverse") + coord_flip() +
    ylab("Std deviation of the proportion of refused uniform visa applications") +
    xlab("Schengen state issuing visas")


## Who asks for ATV visas and to go where?
visas2018 %>%
    filter(!is.na(Airport.transit.visas..ATVs..applied.for)) %>%
    group_by(Country = Country.where.consulate.is.located) %>%
    summarize(ATV.applied = sum(Airport.transit.visas..ATVs..applied.for)) %>% filter(ATV.applied>=10) %>%
    arrange(desc(ATV.applied))


## Share of Mutiple Entry Visas among Uniform visas issued
## Which countries are generous?

visas2018 %>% filter(Share.of.MEVs.on.total.number.of.uniform.visas.issued>1)
## returns 12 rows with a higher number of MEVs issued than of Uniform (inluding MEVs). Weird..

visas2018 %>%
	filter(Share.of.MEVs.on.total.number.of.uniform.visas.issued<=1) %>%
	group_by(Schengen.State) %>%
 	summarize(share.MEV = 	sum(Multiple.entry.uniform.visas..MEVs..issued, na.rm=T)/sum(Total..uniform.visas.issued..including.MEV..., na.rm=T)) %>% 	ggplot(aes(reorder(Schengen.State,share.MEV),share.MEV)) +
	 geom_col() +
 	coord_flip() +
 	xlab("Schengen state") +
 	ylab("Share of MEV among Uniform visas issued")

## Share of MEVs issued vs rate of Uniform issued
tmp = visas2018 %>%
	filter(Share.of.MEVs.on.total.number.of.uniform.visas.issued<=1) %>%
	group_by(Schengen.State) %>%
 	summarize(share.MEV = 	sum(Multiple.entry.uniform.visas..MEVs..issued, na.rm=T)/sum(Total..uniform.visas.issued..including.MEV..., na.rm=T),
                  uniform.issued = 1-sum(Uniform.visas.not.issued,na.rm=T)/sum(Uniform.visas.applied.for,na.rm=T))
tmp %>%
 	ggplot(aes(uniform.issued,share.MEV,label=Schengen.State)) +
	 geom_point(position="jitter") +
	 geom_smooth(method="lm") +
	 geom_label(size=2) +
 	 xlab("Rate of Uniform visas issued") +
 	ylab("Share of MEV among Uniform visas issued")

cor.test(tmp$share.MEV,tmp$uniform.issued)
## 0.417



## For each Schengen state, a histogram of the rate of refused Uniform visas, one value for each consulate. How variable is the rejection rate?
## I have to do group_by and calculate average as geom_vline doesn't include the facetting in its calculations
	visas2018 %>%
	filter(!is.na(Not.issued.rate.for.uniform.visas)) %>%
	filter(Uniform.visas.applied.for>10) %>%
	group_by(Schengen.State) %>%
	mutate(avg.rate = sum(Uniform.visas.not.issued,na.rm=T)/sum(Uniform.visas.applied.for,na.rm=T)) %>%
	ggplot(aes(Not.issued.rate.for.uniform.visas)) +
	geom_histogram(binwidth=0.1) +
	geom_vline(aes(xintercept = avg.rate),linetype = "dashed", col = 4) +
	xlab("Rate of Uniform visas refused") +
	facet_wrap(vars(Schengen.State))


## Ordering consulates according to their refusal rate
visas2018 %>%
filter(Uniform.visas.applied.for>10) %>%
filter(!is.na(Not.issued.rate.for.uniform.visas)) %>%
select(Schengen.State, Country = Country.where.consulate.is.located, Consulate, rate = Not.issued.rate.for.uniform.visas, nb.applications = Uniform.visas.applied.for) %>%
arrange(desc(rate)) %>%
slice(1:30)


## Histogram for each Schengen State of share of MEVs granted among Uniform visas issued, with a dashed line for the average share

visas2018 %>%
	filter(!is.na(Not.issued.rate.for.uniform.visas)) %>%
	filter(Uniform.visas.applied.for>10) %>% filter(Share.of.MEVs.on.total.number.of.uniform.visas.issued<=1) %>%
	group_by(Schengen.State) %>%
	mutate(share.MEV = 	sum(Multiple.entry.uniform.visas..MEVs..issued, na.rm=T)/sum(Total..uniform.visas.issued..including.MEV..., na.rm=T)) %>%
	ggplot(aes(Share.of.MEVs.on.total.number.of.uniform.visas.issued)) +
	geom_histogram(binwidth=0.1) +
	geom_vline(aes(xintercept = share.MEV),linetype = "dashed", col = 4) +
	xlab("Share of MEVs among uniform visas issued") +
	facet_wrap(vars(Schengen.State))




## Map of Schengen countries
schengen = c("Austria","Belgium","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland")

map_world = map_data(map = "world",region = "(?!Antarctica)")
map_world$schengen = as.numeric(map_world$region %in% schengen)

## For some reason, not using expand_limits returns blank map, or if using x and y aesthetics, returns the map but throws a warning
ggplot() +
    geom_map(data = map_world, map = map_world, aes(map_id = region, fill = schengen)) +
    expand_limits(x = map_world$long, y = map_world$lat) +
    theme(legend.position="none") +
    ggtitle("Schengen countries")



## Creating small visas with columns: Schengen.State, Country, Uniform.applied, Uniform.not.issued
visas2018.small = visas2018 %>% select(Schengen.State, Country = Country.where.consulate.is.located, Uniform.applied = Uniform.visas.applied.for, Uniform.not.issued = Uniform.visas.not.issued) %>% group_by(Country, Schengen.State) %>% summarize(Uniform.applied = sum(Uniform.applied, na.rm=T), Uniform.not.issued = sum(Uniform.not.issued, na.rm=T)) %>% mutate(rate = Uniform.not.issued/Uniform.applied) %>% ungroup()

## Creating a wide data.frame with rows Schengen states, columns countries, and cells rate. We replace NAs by 0s
visas2018.small.rate.bycountry = visas2018.small %>% select(Schengen.State,Country,rate) %>% spread(Country,rate) %>% mutate_all(replace_na,0)

## Creating a wide data.frame with rows countries, columns Schengen states, and cells rate. We replace NAs by 0s
visas2018.small.rate.bySchengen = visas2018.small %>% select(Schengen.State,Country,rate) %>% spread(Schengen.State,rate) %>% mutate_all(replace_na,0)


