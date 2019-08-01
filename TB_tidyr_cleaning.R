library(tidyverse)

# who TB dataset from tidyr package
# bad thing is that this dataset looks kind of old (up to 2012)

?tidyr::who

# cleaning
# first steps are taken from R4DS Tidy chapter (up to rename)
who_clean <- who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "diagnosis_method", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1) %>%
  rename(cases = value) %>%
  mutate(age = recode(age, "014" = "cat0_14", "1524" = "cat15_24", "2534" = "cat25_34", "3544" = "cat35_44", "4554" = "cat45_54", "5564" = "cat55_64", "65" = "cat_above65")) %>%
  mutate(age = factor(age, levels = c("cat0_14", "cat15_24", "cat25_34", "cat35_44", "cat45_54", "cat55_64","cat_above65")))

write_csv(who_clean, file = "Tuberculosis_WHO.csv")

# introductory plot
who_clean %>%
  filter(diagnosis_method == "sp") %>%
  filter(country %in% c("Madagascar", "United States of America")) %>%
  ggplot(aes(x = year, y = cases)) + 
  geom_point(alpha = .5) +
  geom_smooth(aes(color = sex), se = FALSE, lwd = 2) +
  facet_wrap(~country) +
  theme_minimal()

# ask students to change "Madagascar" to a different country

# turn to neighbor, introduce yourself, what country did you choose and why, interpret your plot. What else would you like to know?

# testing out loading csv from github URL
# needs to be the *RAW* version
website <- "https://raw.githubusercontent.com/HSL-Data/HSL_Workshops/master/data/brauer2007_tidy.csv"

br <- read.csv(url(website))
  