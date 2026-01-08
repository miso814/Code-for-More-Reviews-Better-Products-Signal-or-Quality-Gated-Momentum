library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(fixest)

table1 <- stream_in(file("All_Beauty.jsonl"))
table1$parent_asin <- as.character(table1$parent_asin)


#Build year-level time variables
year_data <- table1 %>%
  mutate(
    ts_s     = as.numeric(timestamp) / 1000,
    calendar = as.POSIXct(ts_s, origin = "1970-01-01", tz = "UTC"),
    year     = lubridate::year(calendar)
  ) %>%
  filter(year >= 2013, year <= 2023)

range(year_data$calendar)

#Aggregate to product-year
panel_year <- year_data %>%
  mutate(
    verified = as.integer(verified_purchase),
    has_img  = as.integer(lengths(images) > 0),
    text_len = nchar(ifelse(is.na(text), "", text))
  ) %>%
  group_by(parent_asin, year) %>%
  summarise(
    new_reviews    = n(),
    stars_year     = sum(rating, na.rm = TRUE),
    verified_share = mean(verified, na.rm = TRUE),
    img_share      = mean(has_img, na.rm = TRUE),
    helpful_mean   = mean(helpful_vote, na.rm = TRUE),
    text_len_mean  = mean(text_len, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(parent_asin, year)

#Complete years within 2013–2023
all_years <- 2013:2023

panel_year_full <- panel_year %>%
  group_by(parent_asin) %>%
  complete(year = all_years, fill = list(new_reviews = 0, stars_year = 0)) %>%
  arrange(parent_asin, year) %>%
  mutate(
    cum_reviews      = cumsum(new_reviews),
    cum_stars        = cumsum(stars_year),
    cum_avg_rating   = ifelse(cum_reviews > 0, cum_stars / cum_reviews, NA_real_),
    lag_cum_reviews  = lag(cum_reviews, 1, default = 0),
    lag_cum_avg_rating = lag(cum_avg_rating, 1)
  ) %>%
  ungroup()


#Drop pre-entry zero years (before first observed review)
panel_year_full <- panel_year_full %>%
  group_by(parent_asin) %>%
  mutate(first_year = min(year[new_reviews > 0], na.rm = TRUE)) %>%
  ungroup() %>%
  filter(is.finite(first_year), year >= first_year) %>%
  select(-first_year)

#Construct attention stock and product age
panel_year_full <- panel_year_full %>%
  mutate(attn = log1p(lag_cum_reviews))  # log(1 + lag cumulative reviews)

panel_year_full <- panel_year_full %>%
  group_by(parent_asin) %>%
  mutate(entry_year = min(year), age = year - entry_year) %>%
  ungroup() %>%
  select(-entry_year)

df_est <- panel_year_full %>% filter(!is.na(lag_cum_avg_rating))

mean_qual <- mean(df_est$lag_cum_avg_rating, na.rm = TRUE)

df_est <- df_est %>%mutate(qual_c = lag_cum_avg_rating - mean_qual)

#Signal test
m_signal <- fepois(
  new_reviews ~ attn + lag_cum_avg_rating | parent_asin + year,
  data = df_est,
  vcov = ~ parent_asin
)

#Quality-gated momentum (uncentered)
m_qgate <- fepois(
  new_reviews ~ attn * lag_cum_avg_rating | parent_asin + year,
  data = df_est,
  vcov = ~ parent_asin
)

#Quality-gated momentum (centered)
m_qgate_c <- fepois(
  new_reviews ~ attn * qual_c | parent_asin + year,
  data = df_est,
  vcov = ~ parent_asin
)

#Robustness-product-age
m_qgate_c_age <- fepois(
  new_reviews ~ attn * qual_c | parent_asin + age,
  data = df_est,
  vcov = ~ parent_asin
)

etable(m_signal, m_qgate, m_qgate_c, m_qgate_c_age)

b_attn <- coef(m_qgate)["attn"]
b_int  <- coef(m_qgate)["attn:lag_cum_avg_rating"]
threshold_rating <- -b_attn / b_int
threshold_rating


#Plots
#Binned patterns by quality group
df_est <- df_est %>%
  mutate(
    qual_group = case_when(
      lag_cum_avg_rating < 4.0 ~ "Low (<4.0)",
      lag_cum_avg_rating < 4.5 ~ "Mid (4.0–4.5)",
      TRUE ~ "High (>=4.5)"
    ),
    attn_bin = ntile(attn, 20)
  )

df_bin_lines <- df_est %>%
  group_by(qual_group, attn_bin) %>%
  summarise(attn_mean = mean(attn), mean_new = mean(new_reviews), .groups = "drop")

ggplot(df_bin_lines, aes(x = attn_mean, y = mean_new, color = qual_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Attention stock (bin mean): log(1 + lag cumulative reviews)",
    y = "Mean new reviews",
    color = "Perceived quality group",
    title = "Binned patterns: attention converts differently by perceived quality"
  ) +
  theme(legend.position = "top")


#Marginal effect of attention vs quality with 95% CI
V <- vcov(m_qgate_c)
b <- coef(m_qgate_c)

b1 <- b["attn"]
b3 <- b["attn:qual_c"]

var_b1 <- V["attn","attn"]
var_b3 <- V["attn:qual_c","attn:qual_c"]
cov_b1b3 <- V["attn","attn:qual_c"]

grid_me <- data.frame(qual_c = seq(min(df_est$qual_c), max(df_est$qual_c), length.out = 400))
grid_me$me <- b1 + b3 * grid_me$qual_c
grid_me$se <- sqrt(var_b1 + (grid_me$qual_c^2) * var_b3 + 2 * grid_me$qual_c * cov_b1b3)
grid_me$lo <- grid_me$me - 1.96 * grid_me$se
grid_me$hi <- grid_me$me + 1.96 * grid_me$se

grid_me$rating <- grid_me$qual_c + mean_qual  # back to rating scale

ggplot(grid_me, aes(x = rating, y = me)) +
  geom_line() +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Lag cumulative average rating",
    y = "Marginal effect of attention on log E[new_reviews]",
    title = "Model-implied attention effect by quality (95% CI)"
  )

#Average lifecycle profile by product age
panel_year_full <- panel_year_full %>%
  group_by(parent_asin) %>%
  mutate(entry_year = min(year), age = year - entry_year) %>%
  ungroup() %>%
  select(-entry_year)

df_age <- panel_year_full %>%
  group_by(age) %>%
  summarise(mean_new = mean(new_reviews), median_new = median(new_reviews), .groups = "drop")

ggplot(df_age, aes(x = age, y = mean_new)) +
  geom_line() +
  labs(
    x = "Product age (years since entry)",
    y = "Mean new reviews",
    title = "Average review lifecycle profile (by product age)"
  )

