install.packages('tidyverse')
library(tidyverse)

fp1_data = as_tibble(read_tsv("./data/spain_gp_fp1.csv"))
fp2_data = as_tibble(read_tsv("./data/spain_gp_fp2.csv"))
fp2_2_data = as_tibble(read_tsv("./data/spain_gp_fp2_2.csv"))

filterByLap <- function(tibble) {
  max_bin = max(tibble$binIndex)
  data = tibble %>% filter(binIndex == max_bin & validBin == 1)
  return (data)
}

tibbles = list(fp1_data, fp2_data, fp2_2_data)
tibbles = map(tibbles, filterByLap)
practice_data = bind_rows(tibbles, .id = "id") %>% 
  select(!(lapNum | lap_number)) %>% 
  rowid_to_column("lap_number")

max_bin = max(team_data$binIndex)

# Get Valid Laps

front_wing_df = tibble(practice_data$wing_setup_0, practice_data$lap_time)

model = lm(practice_data$lap_time ~ poly(practice_data$wing_setup_0, 2))

summary(model)
confint(model, level=0.95)

coefficients <- coef(model)

a <- coefficients["poly(practice_data$wing_setup_0, 2)1"]

# Coefficients for the linear term (x)
b <- coefficients["poly(practice_data$wing_setup_0, 2)2"]

# Constant term
c <- coefficients["(Intercept)"]

# Calculate x-coordinate of the vertex
x_vertex <- -b / (2 * a)

# Calculate y-coordinate of the vertex
y_vertex <- a * x_vertex^2 + b * x_vertex + c

cat("Vertex coordinates:", "\n")
cat("x:", x_vertex, "\n")
cat("y:", y_vertex, "\n")


# Show data with outliers
lap_graph = practice_data %>% 
  ggplot() + 
  geom_point(mapping = aes(wing_setup_0, lap_time)) +
  geom_smooth(mapping = aes(wing_setup_0, lap_time), method='lm', formula= y~poly(x, 1))

diff(range(model$fitted))

ggplotRegression(fit1)

lap_graph

# Filter outliers and show graph
lap_data = filter(lap_data, lap_data$lap_time <= 98.75)
lap_graph = lap_data %>% ggplot(aes(lap_time, wing_setup_0)) + geom_boxplot()
lap_graph

