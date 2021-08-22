An Examination of Population Density’s Relationship with Life Expectancy
================
by Ye Master of R, Zander

We start by obtaining the information about countries’ areas in square
kilometers from GeoNames.orgs using a function from the geonames
package. I slow the rate of iteration so as to prevent any issues with
overloading their server. We then save the gapminder data from the
gapminder package as an explicitly defined object in R. This latter data
frame contains information about the life expectancy and population of
many countries over time during recent history.

``` r
# slow rate of scraping function
country_info_slow <- slowly(f = GNcountryInfo, 
                  rate = rate_delay(1))

# get data from GeoNames.org
country_info <- country_info_slow() %>% 
    select(countryCode, areaInSqKm) # selecting variables of interest

# load gapminder data
gapminder_data <- gapminder
```

In order to find the population density of each country, we `left_join`
the country\_info data to the gapminder object. We first adjust the
naming convention of the gapminder data to reflect the iso convention
used in the “countryCode” column of gapminder. This creates a key which
the `left_join` function can use to relate the two data sets; I store
the related data set as a new object.

``` r
# adjust country naming convention of countries in gapminder
country_codes <- gapminder_data$country %>% 
  countrycode(origin = "country.name", destination = "iso2c")

gapminder_data <- gapminder_data %>% 
  mutate(countryCode = country_codes)

# join gapminder_data and country_info
merged_data <- gapminder_data %>% 
  left_join(country_info, by = "countryCode") %>% 
  rename(life_exp = lifeExp, population = pop, 
         area = areaInSqKm) %>% 
    # converting names to snake case
  select(country, continent, year, 
         life_exp, population, area) 
    # selecting variables of interest
```

With the two data sets now merged, we `mutate` a new “population
density” variable as a function of each observation’s population divided
by its area.

Calculate the population density for each observation.

``` r
# mutate a "pop_density" variable
merged_data <- merged_data %>% 
  mutate(area = as.numeric(area), 
    # convert area to numeric
    pop_density = population / area)
```

I graph the results twice, first as a basic scatter plot with smoothing
line of population density vs. life expectancy and then another more
complicated scatter broken down by continent and eached fitted with
trend lines. I take a logarithmic transformation of the x-axis in both
plots for enhanced visual interpretability.

``` r
# visualize population density versus life expectancy as a line graph
merged_data %>% 
  ggplot(aes(x = pop_density, y = life_exp), 
        position = "jitter") + # jitter to address overplotting 
    geom_point(aes(alpha = .8), show.legend = FALSE) + 
  geom_smooth(color = "white", position = "jitter") + 
    geom_point(aes(alpha = .8), show.legend = FALSE) + 
  geom_smooth(color = "white") +
  scale_x_continuous(trans = 'log10') + # logarithmic transformation
  labs(
    title = "Life Expectacy vs. Population Density",
    x = "Poplulation Density (People per km2)", 
    y = "Life Expectancy (years)") + 
  theme_dark()
```

![](gapminder_files/figure-gfm/basic%20line%20graph-1.png)<!-- -->

As one can see from the graph above, it looks like population density
and life expectancy are positively correlated. It appears that
observations which have a greater population density also tend to have a
greater life expectancy. The fitted regression line confirms this
observation because of its positive slope, which increases slightly and
then more sharply between a population density of 10 and 100.

There is, however, substantial variation, which indicates that other
factors may explain the life expectancy. To investigate, I visually
break down the same relationship by continent below:

``` r
merged_data %>% 
  ggplot(aes(x = pop_density, y = life_exp),
         position = "jitter") + # jitter to address overplotting 
  geom_point(aes(color = continent, 
      alpha = .8), # make data points transparent as well
    show.legend = FALSE) + 
  geom_smooth(color = "white") +
  facet_wrap(~ continent) + 
  scale_x_continuous(trans = 'log10') + # logarithmic transformation
  labs(
    title = "Life Expectancy vs. Population Density by Continent",
    x = "Poplulation Density (People per km2)", 
    y = "Life Expectancy (years)", 
    caption = "Look at those pretty colors"
  ) + 
  scale_color_brewer(palette = "Set3") + 
  theme_dark()
```

![](gapminder_files/figure-gfm/complicated%20line%20graph-1.png)<!-- -->

The hypothesized general increase in life expectancy between 10 and 100
people per square kilometer holds, but there some interesting variation
between continents. Life expectancy goes down somewhat noticeably in the
Americas, Europe, and Oceania after around 10 people per square
kilometer and then recovers soon after. Africa and Asia do not share
this trend. Perhaps it marks the devastation of both World Wars, which
devastated the three former continents more sharply than the latter two.
This is a hypothesis that merits further investigation.