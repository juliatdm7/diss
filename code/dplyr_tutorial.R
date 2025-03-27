### Data management in R using dplyr ###

library(dplyr)
dim(starwars)
View(starwars)

#Using filter() function in dplyr to select rows with specific column values:
starwars %>% filter(skin_color == "light", eye_color == "brown")  # in this case, we select rows (cases) that have "light" in the column "skin_color" and "brown" in the column "eye_color"

#Using arrange() function in dplyr to order cases (rows) according to values in columns (if we input more than one column, subsequent columns are used to break ties)
starwars %>% arrange(height, mass)  # we rearrange rows in ascending order of "height", and we use values in column "mass" to break ties whenever we find ties
starwars %>% arrange(desc(height))  # we can use the function desc() within arrange() to rearrange in descending order

#Using slice() function in dplyr to select, remove or duplicate rows based on their location:
starwars %>% slice(5:10)  # here, we're selecting or extracting rows or cases between positions 5 and 10
starwars %>% slice_head(n = 3)  #here, we're selecting the 3 first cases (rows) in the dataset...
starwars %>% slice_tail(n = 3)  #...and here, we're selecting the 3 last ones
starwars %>% slice_sample(n = 5)  # we can also use slice_sample() to randomly select cases, in this case 5 cases or rows
starwars %>% slice_sample(prop = 0.1)  # we can randomly select cases by establishing the proportion of cases that we want in relation to the whole database size
starwars %>%
  filter(!is.na(height)) %>%
  slice_max(height, n = 3)  # we can also select cases with the maximum value in a specific variable or column (in this case, maximum "height"), but we ALWAYS have to exclude NAs first
# We can also use select() to select columns or variables rather than rows or cases:
# Select columns by name:
starwars %>% select(hair_color, skin_color, eye_color)  # select all columns between hair_color and eye_color (inclusive)
starwars %>% select(hair_color:eye_color)  # select all columns except those from hair_color to eye_color (inclusive)
starwars %>% select(!(hair_color:eye_color))  # select all columns ending with color
starwars %>% select(ends_with("color"))  # we can also select based on how the name of the columns end
# There are a number of helper functions you can use within select(), like starts_with(), ends_with(), matches() and contains(). These let you quickly match larger blocks of variables that meet some criterion. See ?select for more details.

# Renaming variables (columns) using rename():
starwars %>% rename(home_world = homeworld)

# Adding new columns using mutate()
starwars %>% mutate(height_m = height / 100)  # here we're creating a new column that will have "height" of characters in metres ("height_m")
starwars %>%
  mutate(height_m = height / 100) %>%
  select(height_m, height, everything())  # we couldn't see the new column, but we can rearrange columns using select() so that "height_m" is the first column, "height" is the second one and after it we have all other columns
starwars %>%
  mutate(
    height_m = height / 100,
    BMI = mass / (height_m^2),
    .keep = "none"
  )  # we can use the argument .keep = "none" withing (mutate) if we want to keep only the columns we've created

# Rearranging column order using relocate():
starwars %>% relocate(sex:homeworld, .before = height)  # here we're placing the columns between "sex" and "homeworld" before height; and after, all others

# Summarising values using summarise()
starwars %>% summarise(height = mean(height, na.rm = TRUE))  # Collapses dataframes into a single row

starwars %>% group_by(species,sex)



###########################
### group_by() tutorial ###
###########################

by_cyl <- mtcars %>% group_by(cyl)

# grouping doesn't change how the data looks (apart from listing how it's grouped):
by_cyl

# It changes how it acts with the other dplyr verbs:
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>% filter(disp == max(disp))

# Each call to summarise() removes a layer of grouping
by_vs_am <- mtcars %>% group_by(vs, am)
by_vs <- by_vs_am %>% summarise(n = n())
by_vs
by_vs %>% summarise(n = sum(n))

# To removing grouping, use ungroup
by_vs %>%
  ungroup() %>%
  summarise(n = sum(n))

# By default, group_by() overrides existing grouping
by_cyl %>%
  group_by(vs, am) %>%
  group_vars()

# Use add = TRUE to instead append
by_cyl %>%
  group_by(vs, am, .add = TRUE) %>%
  group_vars()

# You can group by expressions: this is a short-hand
# for a mutate() followed by a group_by()
mtcars %>%
  group_by(vsam = vs + am)

# The implicit mutate() step is always performed on the
# ungrouped data. Here we get 3 groups:
mtcars %>%
  group_by(vs) %>%
  group_by(hp_cut = cut(hp, 3))

# If you want it to be performed by groups,
# you have to use an explicit mutate() call.
# Here we get 3 groups per value of vs
mtcars %>%
  group_by(vs) %>%
  mutate(hp_cut = cut(hp, 3)) %>%
  group_by(hp_cut)

# when factors are involved and .drop = FALSE, groups can be empty
tbl <- tibble(
  x = 1:10,
  y = factor(rep(c("a", "c"), each  = 5), levels = c("a", "b", "c"))
)
tbl %>%
  group_by(y, .drop = FALSE) %>%
  group_rows()



### case_when() tutorial ###


x <- 1:70
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  .default = as.character(x)
)

# Like an if statement, the arguments are evaluated in order, so you must
# proceed from the most specific to the most general. This won't work:
case_when(
  x %%  5 == 0 ~ "fizz",
  x %%  7 == 0 ~ "buzz",
  x %% 35 == 0 ~ "fizz buzz",
  .default = as.character(x)
)

# If none of the cases match and no `.default` is supplied, NA is used:
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
)

# Note that `NA` values on the LHS are treated like `FALSE` and will be
# assigned the `.default` value. You must handle them explicitly if you
# want to use a different value. The exact way to handle missing values is
# dependent on the set of LHS conditions you use.
x[2:4] <- NA_real_
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "nope",
  .default = as.character(x)
)

# `case_when()` evaluates all RHS expressions, and then constructs its
# result by extracting the selected (via the LHS expressions) parts.
# In particular `NaN`s are produced in this case:
y <- seq(-2, 2, by = .5)
case_when(
  y >= 0 ~ sqrt(y),
  .default = y
)

# `case_when()` is particularly useful inside `mutate()` when you want to
# create a new variable that relies on a complex combination of existing
# variables
starwars %>%
  select(name:mass, gender, species) %>%
  mutate(
    type = case_when(
      height > 200 | mass > 200 ~ "large",
      species == "Droid" ~ "robot",
      .default = "other"
    )
  )


# `case_when()` is not a tidy eval function. If you'd like to reuse
# the same patterns, extract the `case_when()` call in a normal
# function:
case_character_type <- function(height, mass, species) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    species == "Droid" ~ "robot",
    .default = "other"
  )
}

case_character_type(150, 250, "Droid")
case_character_type(150, 150, "Droid")

# Such functions can be used inside `mutate()` as well:
starwars %>%
  mutate(type = case_character_type(height, mass, species)) %>%
  pull(type)

# `case_when()` ignores `NULL` inputs. This is useful when you'd
# like to use a pattern only under certain conditions. Here we'll
# take advantage of the fact that `if` returns `NULL` when there is
# no `else` clause:
case_character_type <- function(height, mass, species, robots = TRUE) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    if (robots) species == "Droid" ~ "robot",
    .default = "other"
  )
}

starwars %>%
  mutate(type = case_character_type(height, mass, species, robots = FALSE)) %>%
  pull(type)

