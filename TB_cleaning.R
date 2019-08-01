library(tidyverse)

# WHO TB dataset from <https://www.who.int/tb/country/data/download/en/>

tb <- read_csv("TB_burden_countries_2019-07-12.csv")

# cleaning
progress <- tb %>%
  select(-contains("iso"), -contains("_lo"), -contains("_hi"))

progress <- progress %>% 
  rename(who_region = g_whoregion,
         pop = e_pop_num,
         incidence_100k = e_inc_100k,
         incidence_number = e_inc_num,
         hiv_percent = e_tbhiv_prct,
         hiv_incidence_100k = e_inc_tbhiv_100k,
         hiv_number = e_inc_tbhiv_num,
         mort_nohiv_100k = e_mort_exc_tbhiv_100k,
         mort_nohiv_number = e_mort_exc_tbhiv_num,
         mort_hiv_100k = e_mort_tbhiv_100k,
         mort_hiv_number = e_mort_tbhiv_num,
         mort_100k = e_mort_100k,
         mort_number = e_mort_num,
         case_fatality_ratio = cfr,
         new_incidence_100k = c_newinc_100k,
         case_detection_percent = c_cdr
         )

glimpse(progress)

# maybe we could do some cool cleaning stuff with countries
progress %>%
  count(country) 

#most countries have 18 years of data, but which do not
progress %>%
  count(country) %>%
  filter(n < 18) %>%
  print(n = Inf)
# interesting, maybe there is something cool we could do with that

# what regions are there
progress %>%
  count(who_region)

write_csv(progress, "tb.csv")

# trend for each country within each who region and highlight favorite country
fav_country <- progress %>%
  filter(country == "Madagascar")

ggplot() + 
  geom_line(data = progress, 
            aes(x = year, y = incidence_100k, group = country), 
            alpha = .1) +
  facet_wrap(~who_region) + 
  geom_smooth(data = progress, 
              aes(x = year, y = incidence_100k), 
              color = "black", se = FALSE) + 
  geom_line(data = fav_country, 
            aes(x = year, y = incidence_100k), 
            color = "red", lwd = 1.5) +
  theme_classic()

