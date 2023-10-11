library(tidyverse)

phi_stat <- function(obs, exp) {
  # Calculate chi-sq for non-zero elements
  phi <- sum(abs(exp - obs)/((abs(exp) + abs(obs))*2))
  return(phi)
}


phi_stat(
  obs = c(.5,.5),
  exp = c(1,0),
)


phi_stat(
  obs = c(.25,.25,.25,.25),
  exp = c(.5,0,.5,0)
)

phi_stat(
  obs = c(0,1),
  exp = c(1,0)
)

phi_stat(
  obs = c(0,1,0),
  exp = c(1,0,0)
) * 2/3


phi_stat(
  obs = c(0,.5,0,.5,0),
  exp = c(.5,0,.5,0,0)
) / 3


phi_stat(
  obs = c(.25,.25,.25,.25),
  exp = c(1,0,0,0)
)

phi_stat(
  obs = rep(1/8,8),
  exp = rep(c(1/4,0),4)
)

phi_stat(
  obs = rep(1/8,8),
  exp = c(1,rep(0,7))
)

phi_stat(
  obs = c(.25,.25,.25,.25),
  exp = c(.26,.25,.25,.24)
)


tibble(
  e = c(1,1,0,0),
  o = c(0,1,0,1),
  i = c(1,1,2,2),
  j = c(1,2,1,2),
  z = c(1,1,1,1)) %>%
  summarise(
    e = sum(e*z),
    o = sum(o*z),
    .by = j
    ) #%>%
  summarise(phi = sum(
    abs(e - o)/((abs(e) + abs(o))*2)
            )
  )

tibble(
    a = c(1,2),
    e = c(1,0),
    o = c(0,1),
    ) -> A



Z$z[Z$i == A$a,
    Z$j == A$a]

Z$i


A <- tibble(
  G = c("A","A","B"),
  a = c("a", "b","c"),
  e = c(1, 2, 3),
  o = c(0,2,1))

Z <- tibble(
  i = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
  j = c("a", "b", "c", "a", "b", "c", "a", "b", "c"),
  z = c(1, 1, 0, 0, 1, 1, 0, 0, 1))

###

Z <- tibble(
  i = c("a", "a", "b", "b"),
  j = c("a", "b", "a", "b"),
  z = c(1, 1, 1, 1))

A <- tibble(
  G = c("A","A"),
  a = c("a", "b"),
  e = c(0, 0),
  o = c(1, 0))

A %>%
  mutate(w = map(a,
                 ~ Z$z[Z$i == .x & Z$j %in% a]),
         .by = "G") %>%
  mutate(e = map_dbl(w, ~ sum(.x * e)),
         o = map_dbl(w, ~ sum(.x * o)),
         e = e/sum(e),
         o = o/sum(o),
         .by = "G") %>%
  summarise(
    phi = sum(abs(e - o)/((abs(e) + abs(o))*2),na.rm = T)
  )

A

A$w


library(ggplot2)

# Sample data
data <- data.frame(
  Group = rep(letters[1:3], each = 20),
  X = rnorm(60)
)

# Calculate group averages
group_averages <- aggregate(. ~ Group, data = data, FUN = mean)

# Create the ggplot with marginal density plots and lines for group averages
gg <- ggplot(data, aes(x = X, fill = Group)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.5) +
  facet_grid(~Group, margins = "both") +
  GGMorph::geom_marginal_densities(fill = "white", color = "black") +
  stat_function(fun = function(x) dnorm(mean(x)), geom = "line", linetype = "dashed",
                aes(color = Group), data = group_averages) +
  scale_color_manual(values = c("a" = "red", "b" = "green", "c" = "blue")) +
  theme_minimal()

print(gg)


tibble(
  A = list(
    tibble(x = 1:3, institution_display_name = "Univ 1"),
    tibble(x = 4:6, institution_display_name = "Univ 2")
  )  
) -> my_tibble

my_tibble %>%
  unnest(institution_display_name = map(A, "institution_display_name")) %>%
  mutate(institution_display_name = map_chr(institution_display_name, ~paste0('"', .x, '"')))

papers %>%
  filter(id == "https://openalex.org/W2886903294") %>%
  unnest(concepts,
         sep = "_") %>%
  filter(level < 1) %>%
  View()
