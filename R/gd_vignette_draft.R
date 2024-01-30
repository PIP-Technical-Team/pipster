## CASE 1: Simple Welfare Analysis
### Welfare share at a given population share
selected_popshare <- 0.5
welfare_share_50 <- pipgd_welfare_share_at(welfare = pip_gd$L,
                                               weight = pip_gd$P,
                                               popshare = selected_popshare,
                                               complete = FALSE)

# Format the string with the given values
formatted_message <- sprintf("%.0f%% of the population owns %.0f%% of welfare",
                             selected_popshare * 100,
                             welfare_share_50$dist_stats$welfare_share_at[[1]] * 100)

print(formatted_message)

### Quantile share vs cumulative share
quantile_welfare_share <- pipgd_quantile_welfare_share(welfare = pip_gd$L,
                                                       weight = pip_gd$P,
                                                       n = 5,
                                                       complete = FALSE)
quantile_welfare_share_at <- pipgd_welfare_share_at(welfare = pip_gd$L,
                                                    weight = pip_gd$P,
                                                    n = 5,
                                                    complete = FALSE)

# Creating the dataframe
df_combined <- data.frame(
  popshare = quantile_welfare_share$dist_stats$popshare,
  quantile_share = quantile_welfare_share$dist_stats$quantile_welfare_share,
  cumulative_share = quantile_welfare_share_at$dist_stats$welfare_share_at
)

# View the combined dataframe
print(df_combined)

### Lorenz Curve








