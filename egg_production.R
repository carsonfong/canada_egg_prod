# How has egg production shifted over the years in Canada?

# Libraries
library(tidyverse)
library(lubridate)

# Load data (from here: https://open.canada.ca/data/en/dataset/91128b7e-bae0-4659-929e-6fb4bbe8a12a)
df <- read_csv("32100121.csv",
               col_types = cols(
                 REF_DATE = col_character(),
                 GEO = col_character(),
                 DGUID = col_character(),
                 `Production and disposition` = col_character(),
                 UOM = col_character(),
                 UOM_ID = col_double(),
                 SCALAR_FACTOR = col_character(),
                 SCALAR_ID = col_double(),
                 VECTOR = col_character(),
                 COORDINATE = col_character(),
                 VALUE = col_double(),
                 STATUS = col_logical(),
                 SYMBOL = col_logical(),
                 TERMINATED = col_logical(),
                 DECIMALS = col_double()
               )
)
# Add "-01" to Month column, and then convert to date type
df_edit <- df %>%
  mutate(date = parse_date(str_c(REF_DATE,"-01")))

# Filter for Production of eggs in shell rows only, and exclude Canada rows so that data is mutually exclusive
egg_prod_prov <- df_edit %>%
  filter(`Production and disposition` == "Production of eggs in shell  [116111]",
         GEO != "Canada") %>%
  mutate(abs_eggs = VALUE * 12000) %>%
  select(date,GEO,VALUE,abs_eggs)

# Do the same thing, but just do Canada totals instead of the provinces
egg_prod_can <- df_edit %>%
  filter(`Production and disposition` == "Production of eggs in shell  [116111]",
         GEO == "Canada",
         date < "2020-01-01") %>%
  mutate(abs_eggs = VALUE * 12000 / 10^9,
         year = year(date)) %>%
  group_by(year) %>%
  summarize(Total = sum(abs_eggs))

# ###############
# # Diversion - Check to make sure that the provinces egg production adds up to the Canada totals
# # First, add up all the provinces
# prov_to_can <- egg_prod_prov %>% 
#   group_by(date) %>%
#   summarize(total = sum(VALUE))
# 
# # Then, join the table with the Canada totals and create a new column that is the difference
# combine_check <- prov_to_can %>%
#   left_join(egg_prod_can, by = "date") %>%
#   mutate(diff = total - Total)
# 
# combine_check %>% summarize(sum = sum(diff))
# # Total diff is 88. Given the magnitude of the values, this looks negligible or even a rounding error!
# # We could dig into this more, but at this point I'm confident that the provinces add up to the country totals
# ################

# Plot the country total
ggplot(egg_prod_can, aes(x = year, y=Total)) +
  geom_point(size = 3) +
  expand_limits(y=0) + 
  labs(x = "", y = "") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  annotate("rect", xmin = 1973, xmax = 1976, ymin = 3.75, ymax = 5, color = "blue",
           alpha = .2) +
  annotate("text", x = 2000, y = 0.2, label = "Data Source: Statistics Canada - Production and disposition of eggs") +
  ggtitle("Billions of Eggs Produced in Canada Annually") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))
  
