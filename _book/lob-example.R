library(tidyverse, quietly = TRUE)
options(dplyr.summarise.inform = FALSE)

set.seed(10)
day_bias <- rnorm(3, 0, 0.3)
sample_bias <- rnorm(4, 0, 0.5)
reagent_bias <- rnorm(2, 0, 1)
sd_repeat <- 3

llob_study <- list()

for(i in 1:3){
  
  llob_study[[i]] <- tibble(
    day = as.integer(i),
    sample_id = rep(1:4, each = 5),
    day_offset = day_bias[i],
    sample_offset = rep(sample_bias, each = 5),
    repeat_error = rnorm(20, 0, sd_repeat),
    dose = day_offset + sample_offset + repeat_error
  )
  
  
}


prob_ticks <- c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)
z_ticks <- qnorm(prob_ticks)

z_ticks <- seq(-3, 3, by = 0.5)
prob_ticks <- pnorm(z_ticks)
z_to_prob <- function(z) {
  pnorm(z)
}

prob_labels <- function(p) {
  v <- signif(p * 100, digits = 3)
  paste0(v, "%")
}

dstudy <- bind_rows(llob_study)

ggplot(dstudy, aes(day, dose)) +
  geom_point() +
  facet_wrap(vars(sample_id))


ggplot(dstudy, aes(dose)) +
  geom_histogram()



g <- ggplot(dstudy, aes(sample = dose)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm, lty = 2, color = "blue") +
  coord_flip() +
 scale_x_continuous(
    breaks = z_ticks,
    labels = prob_labels(prob_ticks)
  ) +
  theme_bw() +
  ylab("Measured Concentration") +
  xlab("Normal Probability") +
  ggtitle("QQ-Plot of LoB Study Samples")


ggExtra::ggMarginal(g, type = "histogram", margins = "x")


#page 15 shows that the LoB can be calcuated using
#. all lob sample results

rank_position <- function(x){
  n <- length(x)
  i <- 0.5 + 0.95 * n
  i_lower  <- as.integer(floor(i))
  i_upper <- as.integer(ceiling(i))
  
  y <- sort(x)
  
  z <- y[i_lower] + (i - i_lower) * (y[i_upper] - y[i_lower])
  
  dindex <- tibble(
    rank = i,
    rank_lower = i_lower,
    rank_upper = i_upper,
    rank_delta = i - i_lower,
    y_lower = y[i_lower],
    y_upper = y[i_upper],
    y_delta = y_upper - y_lower,
    y_lob = y_lower + rank_delta * y_delta
  )
  
  return(list(dindex = dindex, x = x))
  
}

rank_position(rnorm(65))
rank_position(rnorm(60))

da1 <- read_csv("blank_replicates.csv",show_col_types = FALSE)
names(da1) <- janitor::make_clean_names(names(da1))
da1 <- da1 %>% 
  pivot_longer(starts_with("blank"), names_to = "sample_id", values_to = "concentration")

llob1 <- rank_position(da1 %>% pull(concentration))

da2 <- read_csv("blank_replicates_2.csv",show_col_types = FALSE)
names(da2) <- janitor::make_clean_names(names(da2))
da2 <- da2 %>% 
  pivot_longer(starts_with("blank"), names_to = "sample_id", values_to = "concentration")

llob2 <- rank_position(da2 %>% pull(concentration))

da1_2 <- bind_rows(
  da1 %>% 
    mutate(reagent_lot = "lot-1"),
  da2 %>% 
    mutate(reagent_lot = "lot-2")
)

dlob <- bind_rows(
  llob1$dindex %>% mutate(reagent_lot = "lot-1"),
  llob2$dindex %>% mutate(reagent_lot = "lot-2")
)

ggplot(da1_2, aes(concentration, reagent_lot)) +
  geom_point(alpha = 0.5) +
  geom_point(data = dlob, aes(y_lob, reagent_lot), color = "red", shape = 13, size = 4) +
  geom_rug(data = dlob, aes(y_lob, reagent_lot), inherit.aes = FALSE, sides = "b", color = "red") +
  theme_bw() +
  ylab("Reagent Lot") +
  xlab("Concentration (pg/mL)") +
  ggtitle("CLSI EP17 Appendix A LoB", subtitle = "Non-Parametric Estimate by Reagent Lot")
  

# tying this back to a non-linear calibration may be helpful,
#  with an asymptotic linear near zero on a 4PL curve, you would expect a lack
#  symmetry

# this data could be considered normally distributed

# think about the method with the reflextion around the median to estimate where the max value can go
