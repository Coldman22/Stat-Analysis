?mean
?ChickWeight

#objects and Functions
5 + 6

a <- 5
b <- 6

a + b

sum(a,b)

ages <- c(5, 6)
ages
sum(ages)

names <- c("John", "James")

friends <- data.frame(names, ages)
View(friends)
str(friends)

friends$ages

sum(friends$ages)

friends$names

friends[1,1]
friends[1, ]
friends[ ,1]

# Built in data sets to practice with
data()
View(starwars)

# Installing and using Package
install.packages("tidyverse")
library(tidyverse)

starwars %>%
  filter(height > 150 & mass < 200) %>%
  mutate(height_in_meters = height/100) %>%
  select(height_in_meters, mass) %>%
  arrange(mass) %>%
  #View()
  plot()


# Explore 
# Data Structure and Types of Variable
View(msleep)

glimpse(msleep)

head(msleep)

class(msleep$name)

length(msleep)

length(msleep$name)

names(msleep)

unique(msleep$vore)

missing <- !complete.cases(msleep)

msleep[missing, ]


# Clean
# Select Variables
starwars %>%
  select(name, height, mass)

starwars %>%
  select(1:3)

starwars %>%
  select(ends_with("color")) 

# Changing Variable order

starwars %>%
  select(name, mass, height, everything()) 

# Changing variable Name
starwars %>%
  rename("character" = "name") %>%
  head()

# Changing a variable type
class(starwars$hair_color)
starwars$hair_color <- as.factor(starwars$hair_color)
class(starwars$hair_color)

starwars %>%
  mutate(hair_color = as.factor(hair_color)) %>%
  glimpse()

# Changing a factor level
df <- starwars
df$sex <- as.factor(df$sex)

levels(df$sex)

df <- df %>%
  mutate(sex = factor(sex,
                      levels = c("male", "female", "hermaphroditic", "none"
                                 )))
levels(df$sex)

# Filter Rows
starwars %>%
  select(mass, sex) %>%
  filter(mass< 55 &
           sex == "male")

# Re-code Data
starwars %>%
  select(sex) %>%
  mutate(sex = recode(sex,
                      "male"= "man",
                      "female" = "woman"))

# Dealing with missing Data
mean(starwars$height, na.rm = TRUE)

starwars %>%
  select(name, gender, hair_color, height)

# Dealing With duplicates
Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22,33,44,22)

friends <- data.frame(Names, Age)
friends

friends %>%
  distinct()

distinct(friends)

# Manipulate
# Create and change a variable (mutate)
starwars %>%
  mutate(height_m = height/100) %>%
  select(name, height, height_m)

# Conditional Change (if_else)
starwars %>%
  mutate(height_m = height/100) %>%
  select(name, height, height_m) %>%
  mutate(tallness = 
           if_else(height_m < 1,
                   "short",
                   "tall"))

# Reshape data with Pivot Wider
library(gapminder)
View(gapminder)

data <- select(gapminder, country, year, lifeExp)

View(data)

wide_data <- data %>%
  pivot_wider(names_from = year, values_from = lifeExp)
View(wide_data)

# Reshape data with Pivot Longer
long_data <- wide_data %>%
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp")
View(long_data)

# Describe
# Range/ Spread
View(msleep)

min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)

# Centrality
mean(msleep$awake)
median(msleep$awake)

# Variance
var(msleep$awake)

summary(msleep$awake)

msleep %>%
  select(awake, sleep_total) %>%
  summary()

# Summarize Your DATA
msleep %>%
  drop_na(vore) %>%
  group_by(vore) %>%
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Difference = 
              max(sleep_total)-min(sleep_total)) %>%
  arrange(Average) %>%
  View()
  #plot()

# Create tables
table(msleep$vore)

msleep %>%
  select(vore, order) %>%
  filter(order %in% c("Rodentia", "Primates")) %>%
  table()


# Visualize
plot(pressure)
# The grammar of graphics
   # data
   # mapping
   # geometry

# bar plots
ggplot(data = starwars,
       mapping = aes(x = gender))+
  geom_bar()

# Histograms
starwars %>%
  drop_na(height) %>%
  ggplot(mapping = aes(x = height))+
  geom_histogram()

# Box plots
starwars %>%
  drop_na(height) %>%
  ggplot(mapping = aes(x = height))+
  geom_boxplot(fill = "steelblue")+
  theme_bw()+
  labs(title = "Boxplot of height",
       x = "Height of character")

# Density Plots
starwars %>%
  drop_na(height) %>%
  filter(sex %in% c("male", "female")) %>%
  ggplot(mapping = aes(x = height,
                       color = sex,
                       fill = sex))+
  geom_density(aplha = 0.1)+
  theme_bw()

# Scatter plots
starwars %>%
  filter(mass < 200) %>%
  ggplot(mapping = aes(x = height, y = mass, color = sex))+
  geom_point(size = 5, alpha = 0.5)+
  theme_minimal()+
  labs(title = "Height and mass by sex")

# Smoothed Model
starwars %>%
  filter(mass < 200) %>%
  ggplot(mapping = aes(x = height, y = mass, color = sex))+
  geom_point(size = 3, alpha = 0.8)+
  geom_smooth()+
  facet_wrap(~sex)+
  theme_bw()+
  labs(title = "Height and mass by sex")


# Analyze
# Hypothesis Testing
# T-test
library(gapminder)
View(gapminder)
t_test_plot

gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = .,
         alternative = "two.sided",
         paired = FALSE)

# ANOVA
ANOVA_plot

gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .)%>%
  summary()

gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD()

gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD()
  
# Chi squared
chi_plot

head(iris)

flowers <- iris %>%
  mutate(size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("Small", "Medium", "Large"))) %>%
  select(Species, size)
  
# chi squared goodness of fit test
flowers %>%
  select(size) %>%
  table()
  chisq.test()
  
# chi squared test of independence
flowers %>%
  table()
  chisq.test()

# Linear Model 1
head(cars, 10)

cars %>%
  lm(dist ~ speed, data = .) %>%
  summary()
