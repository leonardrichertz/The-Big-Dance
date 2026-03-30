# =============================================================================
# The Big Dance: Evaluating Three-Point Shooters in NCAA Women's Basketball
# =============================================================================
# Install and load required packages
if (!requireNamespace("wehoop", quietly = TRUE)) {
    install.packages("wehoop")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
}
if (!requireNamespace("lme4", quietly = TRUE)) {
    install.packages("lme4")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
}
if (!requireNamespace("gt", quietly = TRUE)) {
    install.packages("gt")
}
# =============================================================================
# 1. DATA ACQUISITION
# =============================================================================

# Download player box score data for the current season (2025-26)
player_box <- wehoop::load_wbb_player_box(seasons = 2026)
head(player_box)
# Download schedule data to get conference information
schedule <- wehoop::load_wbb_schedule(seasons = 2026)
head(schedule)
# =============================================================================
# 2. DATA PREPARATION
# =============================================================================

# Look at the structure of the data
dplyr::glimpse(player_box)
dplyr::glimpse(schedule)

# Select relevant columns from player box scores
player_data <- player_box |>
    dplyr::select(
        game_id,
        athlete_id,
        athlete_display_name,
        team_id,
        team_short_display_name,
        three_point_field_goals_made,
        three_point_field_goals_attempted,
        # include free throws for future extra analysis of over/underestimation. We could assume that a great free throw shooter is likely a good shooter overall, and look for players with large discrepancies between their three-point shooting and free throw shooting to identify potential outliers or interesting cases. # nolint: line_length_linter.
        free_throws_made,
        free_throws_attempted # include free throws for extra analysis in teh future
    ) |>
    dplyr::rename(
        player_id = athlete_id,
        player_name = athlete_display_name,
        team_name = team_short_display_name,
        fg3_made = three_point_field_goals_made,
        fg3_att = three_point_field_goals_attempted,
        ft_made = free_throws_made,
        ft_att = free_throws_attempted
    )
dplyr::glimpse(player_data)
# Get conference information from schedule
# Extract home and away team conference info
home_info <- schedule |>
    dplyr::select(game_id, home_id, home_conference_id) |>
    dplyr::rename(team_id = home_id, conference_id = home_conference_id)

away_info <- schedule |>
    dplyr::select(game_id, away_id, away_conference_id) |>
    dplyr::rename(team_id = away_id, conference_id = away_conference_id)

team_conference <- dplyr::bind_rows(home_info, away_info) |>
    dplyr::distinct()
dplyr::glimpse(team_conference)
# Get opponent information from schedule
opponent_info <- schedule |>
    dplyr::select(game_id, home_id, away_id, home_conference_id, away_conference_id)

# Merge player data with conference information
player_data <- player_data |>
    dplyr::left_join(team_conference, by = c("game_id", "team_id"))
dplyr::glimpse(player_data)
# Add opponent information more efficiently
game_opponents <- schedule |>
    dplyr::select(game_id, home_id, away_id, home_conference_id, away_conference_id)

# Create opponent lookup for home teams
home_lookup <- game_opponents |>
    dplyr::select(game_id,
        team_id = home_id,
        opp_team_id = away_id,
        opp_conference_id = away_conference_id
    )

# Create opponent lookup for away teams
away_lookup <- game_opponents |>
    dplyr::select(game_id,
        team_id = away_id,
        opp_team_id = home_id,
        opp_conference_id = home_conference_id
    )

opponent_lookup <- dplyr::bind_rows(home_lookup, away_lookup)

# Merge opponent info
player_data <- player_data |>
    dplyr::left_join(opponent_lookup, by = c("game_id", "team_id"))

# Filter to only include games with three-point attempts
model_data <- player_data |>
    dplyr::filter(fg3_att > 0) |>
    dplyr::filter(!is.na(conference_id) & !is.na(opp_conference_id) & !is.na(opp_team_id)) |>
    dplyr::mutate(
        player_id = as.factor(player_id),
        team_id = as.factor(team_id),
        conference_id = as.factor(conference_id),
        opp_team_id = as.factor(opp_team_id),
        opp_conference_id = as.factor(opp_conference_id)
    )

dplyr::glimpse(model_data)
# Check data dimensions
cat("Number of player-game observations:", nrow(model_data), "\n")
cat("Number of unique players:", dplyr::n_distinct(model_data$player_id), "\n")
cat("Number of unique teams:", dplyr::n_distinct(model_data$team_id), "\n")
cat("Number of unique conferences:", dplyr::n_distinct(model_data$conference_id), "\n")
cat("Number of unique opponents:", dplyr::n_distinct(model_data$opp_team_id), "\n")

# =============================================================================
# 3. HIERARCHICAL MODEL
# =============================================================================

# Fit a hierarchical binomial model for three-point shooting
# Model: fg3_made ~ Binomial(fg3_att, p)
# logit(p) = mu + player_effect + conference_effect + opp_conference_effect + opp_team_effect
#
# Random Effects:
# - player_id: captures individual player true talent
# - conference_id: captures conference-level shooting ability
# - opp_conference_id: captures defensive strength at conference level
# - opp_team_id: captures team-specific defensive ability

model_full <- lme4::glmer(
    cbind(fg3_made, fg3_att - fg3_made) ~ 1 +
        (1 | player_id) +
        (1 | conference_id) +
        (1 | opp_conference_id) +
        (1 | opp_team_id),
    data = model_data,
    family = binomial(link = "logit")
)

# Model summary
summary(model_full)

# =============================================================================
# 4. EXTRACT PLAYER TRUE TALENT ESTIMATES
# =============================================================================

# Extract random effects
player_effects <- lme4::ranef(model_full)$player_id
conference_effects <- lme4::ranef(model_full)$conference_id

# Get fixed effect (intercept = population mean on logit scale)
intercept <- lme4::fixef(model_full)["(Intercept)"]

# Calculate player true talent on probability scale
# For player talent, we include: intercept + player effect + (conference effect)
# We DO NOT include opponent effects as those are defensive adjustments

# Create player-level summary
player_summary <- model_data |>
    dplyr::group_by(player_id, player_name, team_id, team_name, conference_id) |>
    dplyr::summarise(
        games = dplyr::n(),
        total_fg3_made = sum(fg3_made),
        total_fg3_att = sum(fg3_att),
        observed_pct = total_fg3_made / total_fg3_att,
        .groups = "drop"
    )

head(player_summary)

# Add random effects to player summary
player_summary <- player_summary |>
    dplyr::mutate(
        player_effect = player_effects[as.character(player_id), "(Intercept)"],
        conf_effect = conference_effects[as.character(conference_id), "(Intercept)"]
    )
head(player_summary)
# Calculate estimated true talent
# Include only player effect (pure individual talent) or player + conference effect
# For evaluating individual talent for personnel decisions, we isolate player ability from conference environment

player_summary <- player_summary |>
    dplyr::mutate(
        # Estimated logit with player effect only (pure talent)
        logit_talent_pure = intercept + player_effect,
        # Estimated logit with player + conference effect (contextual talent)
        logit_talent_context = intercept + player_effect + conf_effect,
        # Convert to probability scale
        estimated_talent_pure = plogis(logit_talent_pure),
        estimated_talent_context = plogis(logit_talent_context)
    )

# Filter to players with minimum sample size for reliable estimates
# Requiring at least 20 three-point attempts
player_summary_filtered <- player_summary |>
    dplyr::filter(total_fg3_att >= 20) |>
    dplyr::arrange(dplyr::desc(estimated_talent_pure))

# =============================================================================
# 5. RESULTS
# =============================================================================

# TABLE: Top 10 Shooters by Estimated True Talent
top_10 <- player_summary_filtered |>
    head(10) |>
    dplyr::select(
        Player = player_name,
        Team = team_name,
        Games = games,
        `3PA` = total_fg3_att,
        `3PM` = total_fg3_made,
        `Obs %` = observed_pct,
        `Est True Talent %` = estimated_talent_pure
    ) |>
    dplyr::mutate(
        `Obs %` = sprintf("%.1f%%", `Obs %` * 100),
        `Est True Talent %` = sprintf("%.1f%%", `Est True Talent %` * 100)
    )

print("Top 10 Three-Point Shooters by Estimated True Talent:")
print(gt::gt(top_10))
# Create table for the report
top_10_report <- player_summary_filtered |>
    head(10) |>
    dplyr::select(
        Rank = player_name,
        Player = player_name,
        Team = team_name,
        Games = games,
        `3PA` = total_fg3_att,
        `Observed %` = observed_pct,
        `True Talent %` = estimated_talent_pure
    ) |>
    dplyr::mutate(
        Rank = dplyr::row_number(),
        `Observed %` = round(`Observed %` * 100, 1),
        `True Talent %` = round(`True Talent %` * 100, 1)
    )

top_10_table <- top_10_report |>
    gt::gt() |>
    gt::tab_header(
        title = "Top 10 Three-Point Shooters",
        subtitle = "NCAA Women's Basketball 2025-26 Season"
    ) |>
    gt::cols_label(
        Rank = "Rank",
        Player = "Player",
        Team = "Team",
        Games = "Games",
        `3PA` = "3PA",
        `Observed %` = "Observed %",
        `True Talent %` = "True Talent %"
    )

gt::gtsave(top_10_table, "top_10_shooters.png") 
# FIGURE: Estimated True Talent vs Observed Shooting Percentage
plot_data <- player_summary_filtered |>
    dplyr::mutate(
        observed_pct_100 = observed_pct * 100,
        estimated_pct_100 = estimated_talent_pure * 100, # We can use estimated_talent_context if we want to show the effect of conference context, but for pure talent evaluation, we use estimated_talent_pure.
        # Create a flag to identify the Top 10 players based on true talent
        is_top_10 = ifelse(dplyr::row_number() <= 10, "Top 10 True Talent", "Rest of NCAA")
    )

shrinkage_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = observed_pct_100, y = estimated_pct_100)) +
    # Map color, size, and transparency to our new Top 10 flag
    ggplot2::geom_point(ggplot2::aes(color = is_top_10, size = is_top_10, alpha = is_top_10)) +

    # Manually set the colors (Orange for Top 10, Steelblue for everyone else)
    ggplot2::scale_color_manual(values = c("Top 10 True Talent" = "darkorange", "Rest of NCAA" = "steelblue")) +
    ggplot2::scale_size_manual(values = c("Top 10 True Talent" = 3, "Rest of NCAA" = 1.5)) +
    ggplot2::scale_alpha_manual(values = c("Top 10 True Talent" = 1, "Rest of NCAA" = 0.5)) +

    # Add the reference lines
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "darkblue", linewidth = 0.8) +
    ggplot2::labs(
        title = "Estimated True Talent vs. Observed Three-Point Percentage",
        subtitle = "NCAA Women's Basketball 2025-26 Season",
        x = "Observed Three-Point Percentage (%)",
        y = "Estimated True Talent (%)",
        color = "Player Category" 
    ) +
    ggplot2::theme_minimal() +

    ggplot2::guides(size = "none", alpha = "none") +

    ggplot2::coord_cartesian(xlim = c(15, 60), ylim = c(20, 50))

print(shrinkage_plot)

# Save the plot
ggplot2::ggsave("true_talent_vs_observed.png", shrinkage_plot, width = 8, height = 6, dpi = 300)

# =============================================================================
# 6. MODEL DIAGNOSTICS AND VARIANCE COMPONENTS
# =============================================================================

# Extract variance components
var_components <- as.data.frame(lme4::VarCorr(model_full))
print("Variance Components:")
print(var_components)

# Calculate ICC-like measures
total_var <- sum(var_components$vcov)
player_var_pct <- var_components$vcov[var_components$grp == "player_id"] / total_var * 100
conf_var_pct <- var_components$vcov[var_components$grp == "conference_id"] / total_var * 100
opp_conf_var_pct <- var_components$vcov[var_components$grp == "opp_conference_id"] / total_var * 100
opp_team_var_pct <- var_components$vcov[var_components$grp == "opp_team_id"] / total_var * 100

cat("\nVariance Component Breakdown:\n")
cat(sprintf("Player: %.1f%%\n", player_var_pct))
cat(sprintf("Conference: %.1f%%\n", conf_var_pct))
cat(sprintf("Opponent Conference: %.1f%%\n", opp_conf_var_pct))
cat(sprintf("Opponent Team: %.1f%%\n", opp_team_var_pct))

# Population mean shooting percentage
pop_mean <- plogis(intercept)
cat(sprintf("\nPopulation mean 3PT%%: %.1f%%\n", pop_mean * 100))

# =============================================================================
# 7. EXTRA CREDIT: Reduced Model (Player + Opponent Team Only)
# =============================================================================

model_reduced <- lme4::glmer(
    cbind(fg3_made, fg3_att - fg3_made) ~ 1 +
        (1 | player_id) +
        (1 | opp_team_id),
    data = model_data,
    family = binomial(link = "logit"),
)

summary(model_reduced)

# Compare variance components
var_reduced <- as.data.frame(lme4::VarCorr(model_reduced))
print("\nReduced Model Variance Components:")
print(var_reduced)

# Compare opponent team variance between models
opp_team_var_full <- var_components$vcov[var_components$grp == "opp_team_id"]
opp_team_var_reduced <- var_reduced$vcov[var_reduced$grp == "opp_team_id"]

cat("\nComparison of Opponent Team Variance:\n")
cat(sprintf("Full Model: %.4f\n", opp_team_var_full))
cat(sprintf("Reduced Model: %.4f\n", opp_team_var_reduced))
cat(sprintf("Difference: %.4f\n", opp_team_var_reduced - opp_team_var_full))

# =============================================================================
# 8. ADDITIONAL ANALYSIS FOR REPORT
# =============================================================================

# Not used, but still in here for reference.
# In the future this could be used to predict which players to pick as the shooters who will make the most 3s, if we include a ranking mechanism for teams who are likel yto play a lot of games in the tournament.
player_summary_filtered <- player_summary_filtered |>
    dplyr::mutate(
        shrinkage = observed_pct - estimated_talent_pure,
        shrinkage_pct = (observed_pct - estimated_talent_pure) / observed_pct * 100
    )

# Players most overestimated by model (estimated > observed)
overestimated <- player_summary_filtered |>
    dplyr::filter(shrinkage < 0) |>
    dplyr::arrange(shrinkage) |>
    head(10)

# Players most underestimated by model (observed > estimated by large amount)
underestimated <- player_summary_filtered |>
    dplyr::filter(shrinkage > 0) |>
    dplyr::arrange(dplyr::desc(shrinkage)) |>
    head(10)

cat("\nPlayers potentially overestimated (model estimate > observed):\n")
print(overestimated |> dplyr::select(player_name, team_name, total_fg3_att, observed_pct, estimated_talent_pure, shrinkage))

cat("\nPlayers potentially underestimated (observed > model estimate):\n")
print(underestimated |> dplyr::select(player_name, team_name, total_fg3_att, observed_pct, estimated_talent_pure, shrinkage))

# =============================================================================
# 9. SAVE OUTPUTS
# =============================================================================

utils::write.csv(top_10_report, "top_10_shooters.csv", row.names = FALSE)

# Save full player estimates for reference
utils::write.csv(
    player_summary_filtered |>
        dplyr::select(
            player_name, team_name, games, total_fg3_att, total_fg3_made,
            observed_pct, estimated_talent_pure, shrinkage
        ),
    "all_player_estimates.csv",
    row.names = FALSE
)

cat("\n=== Analysis Complete ===\n")
cat("Outputs saved:\n")
cat("- true_talent_vs_observed.png (figure)\n")
cat("- top_10_shooters.csv (table)\n")
cat("- all_player_estimates.csv (full data)\n")
