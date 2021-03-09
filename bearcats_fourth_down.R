library(tidyverse)

plays <- read_csv("fourth_down_plays.csv")

#View(plays)
skimr::skim(plays)

# wrangling ---------------------------------------------------------------

score_split <- str_split(plays$score, " - ") #splitting to get Cincinnati's score and opponent score for each play
plays$score1 <- score_split %>% map(1) %>% unlist() %>% as.numeric()
plays$score2 <- score_split %>% map(2) %>% unlist() %>% as.numeric()

plays <- plays %>% 
  mutate(
    uc_score = ifelse(defteam == "Georgia", score2, score1), #only in a different order for Georgia game
    opp_score = ifelse(defteam == "Georgia", score1, score2),
    uc_lead = uc_score - opp_score
  ) %>% 
  mutate(
    success = case_when(
      choice == "Go" & str_detect(desc, "for a 1ST down") ~ TRUE,
      choice == "Go" & str_detect(desc, "for a TD") ~ TRUE,
      choice == "FG" & str_detect(desc, "GOOD") ~ TRUE,
      choice == "Punt" ~ NA,
      TRUE ~ FALSE
    )
  )

# eda ---------------------------------------------------------------------

bearcats_colors <- c("#404040", "#E00122", "#C0C0C0") # FG, GO, PUNT
legend_text <- "(<span style = 'color: red;'>Go</span> - <span style = 'color: black;'>FG</span> - <span style = 'color: grey;'>Punt</span>)"
#Note: details of plots, especially sizes, were based on exporting from RStudio with width of 600 and height of 400

## Lost value by not going for it
plays %>% 
  filter(recommendation == "Go" & choice != "Go") %>% 
  .$decision_value %>% mean()


## Success when being aggressive
plays %>% 
  filter(recommendation != "Go" & choice == "Go") %>% 
  .$success %>% mean()


## Go, No-Go Pie Charts
#generalizing Go vs Kick decision to see decisions at a high level

go_no_go <- plays %>% 
  mutate(
    reco = ifelse(recommendation == "Go", "Go", "No-Go"), 
    did_go = ifelse(choice == "Go", 1, 0)
  ) %>%
  group_by(reco) %>% 
  summarise(
    go = mean(did_go),
    no_go = mean(!did_go)
  ) %>% 
  ungroup()

gonogo_long <- go_no_go %>% 
  pivot_longer(-reco, names_to = "decision", values_to = "percent") %>% 
  mutate(decision = ifelse(decision=="go", "Go", "No-Go"))

gonogo_legendtext <- "(<span style = 'color: red;'>Go</span> - <span style = 'color: black;'>No-Go</span>)"

#using 2 pie charts (model recommending Go or Kick) to show 2 categories each (Cincinnati choosing to Go or Kick)
gonogo_pie <- function(x) {
  
  stopifnot(x %in% c("Go", "No-Go"))
  
  gonogo_long %>% 
    filter(reco==x)  %>% 
    ggplot(mapping = aes(x = "", y = percent, fill = decision)) +
    geom_bar(
      stat = "identity", 
      width = 1, 
      color = "white",
      alpha = 0.9
    ) +
    # geom_text(
    #   mapping = aes(label = paste(paste0(decision, "   "), scales::percent_format()(percent), sep = "\n")), 
    #   size = 5.5,
    #   position = position_stack(vjust = 0.5), 
    #   color = c(bearcats_colors[1], bearcats_colors[2]),
    #   alpha = 0.9
    # ) +
    coord_polar("y") +
    theme_void() +
    scale_fill_manual(values = c(bearcats_colors[2], bearcats_colors[1])) +
    # labs(
    #   title = paste("Choices when", x, "Recommendation"),
    #   color = "UC's Choice",
    #   x = "Bearcats Lead",
    #   y = "Decision Value"
    # ) +
    theme(
      # plot.title = ggtext::element_markdown(size = 12),
      # axis.text.x = element_text(size = 10),
      # axis.title.x = element_text(size = 10),
      legend.position = "none"
    )
  
}

gonogo_pie("Go")
gonogo_pie("No-Go")


## Choice by Reco
#breaking down further using stacked bars - 3 bars with 3 categories each is still digestible

plays %>% 
  count(recommendation, choice) %>% 
  mutate(
    recommendation = case_when(
      recommendation=="FG" ~ "Field Goal",
      recommendation=="Go" ~ "Go For It",
      TRUE ~ recommendation
    ), 
    choice = case_when(
      choice=="FG" ~ "Field Goal",
      choice=="Go" ~ "Go For It",
      TRUE ~ choice
    )
  ) %>% 
  ggplot(aes(x=recommendation, y=n, fill=choice)) + 
  geom_bar(position="fill", stat="identity", alpha=0.8) +
  theme_minimal() +
  scale_fill_manual(values = bearcats_colors) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(
    title = paste("Cincinnati's Choices for a Given Model Recommendation", legend_text),
    fill = "Coach Decision",
    x = "Model Recommendation",
    y = ""
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  )


## Yards to endzone

#hist - reco and choice
ggplot(plays, aes(x=yards_to_goal, fill=recommendation)) + geom_histogram(alpha=0.2, position="identity")
ggplot(plays, aes(x=yards_to_goal, fill=choice)) + geom_histogram(alpha=0.2, position="identity")

#density - reco and choice
ggplot(plays, aes(x=yards_to_goal, fill=recommendation)) + geom_density(alpha=0.2, position="identity")
ggplot(plays, aes(x=yards_to_goal, fill=choice)) + geom_density(alpha=0.2, position="identity")

ann_text_size <- 3.1

#2 density plots - one for Cincinnati's choice and one for model recommendations -
#that overlay frequency of 4th down play options across yards to endzone
decision_density <- function(x) {
  
  stopifnot(x %in% c("choice", "recommendation"))
  
  plays %>% 
    mutate(fg = !!as.name(x) == "FG") %>% 
    ggplot(aes(x=yards_to_goal, fill=!!as.name(x), alpha = fg)) + 
    #geom_density(alpha=0.2, position="identity") +
    geom_density(position="identity") +
    #scale_y_continuous(breaks = seq(0, 0.06, 0.02)) +
    ylim(0,0.06) +
    scale_fill_manual(values = bearcats_colors) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    theme_bw() +
    labs(
      title = paste("Frequency of 4th Down", paste0(str_to_title(x),"s"), "by Yards to Endzone", legend_text),
      fill = x,
      x = "Yards to Endzone",
      y = "Density"
    ) +
    theme(
      plot.title = ggtext::element_markdown(size = 11),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      legend.position = "none"#,
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank()
    )
}

decision_density("choice")
dp_reco <- decision_density("recommendation")

long_fg_reco <- plays %>% filter(recommendation == "FG") %>% filter(yards_to_goal == max(yards_to_goal)) %>% .$yards_to_goal
num_fg_recos <- plays %>% filter(recommendation == "FG") %>% nrow()

#adding explanation of unusual model recommendation in case plot stands alone
dp_reco +
  annotate(
    "text", size = ann_text_size, color = bearcats_colors[1],
    x = long_fg_reco, y = 0.026, 
    label = paste(strwrap(paste("Cincinnati was up big late in the 4th - FG happened to have a 0.003% higher expected win probability than a punt"), 62), collapse = "\n")
  ) +
  annotate("segment", x = long_fg_reco, xend = long_fg_reco, y = 0.018, yend = 0.0225, colour = "black")


## Decisions based on lead
#scatter plot view of the decision value and Cincinnati's choices given their lead

worst_play <- plays %>% filter(decision_value == min(decision_value))

rect1_xmin <- min(plays$uc_lead)-0.5
rect1_xmax <- 0.5
rect1_y_subset <- plays %>% filter(uc_lead <= rect1_xmax) %>% select(decision_value)
rect1_ymin <- min(rect1_y_subset)-1
rect1_ymax <- max(rect1_y_subset)+1

rect2_xmin <- 10.5 #10 point lead
rect2_xmax <- max(plays$uc_lead)+1
rect2_y_subset <- plays %>% filter(uc_lead >= rect2_xmin) %>% select(decision_value)
rect2_ymin <- min(rect2_y_subset)-1
rect2_ymax <- max(rect2_y_subset)+1

ggplot(plays, aes(x = uc_lead, y = decision_value, color = choice)) + 
  geom_point(size = 2) +
  theme_bw() +
  scale_color_manual(values = bearcats_colors) +
  scale_y_continuous(breaks = seq(-25, 25, by = 5)) +
  labs(
    title = paste("Value of Cincinnati's Choices by Point Margin", legend_text),
    color = "Cincinnati's Choice",
    x = "Cincinnati Lead",
    y = "Decision Value"
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) + 
  #adding insights onto plot for when it stands alone without more context
  annotate("rect", xmin = rect1_xmin, xmax = rect1_xmax, ymin = rect1_ymin, ymax = rect1_ymax, alpha = 0.2) +
  annotate(
    "text", size = ann_text_size, x = mean(c(rect1_xmin, rect1_xmax)), y = rect1_ymin-1,
    label = "Only kicks when tied or losing"
  ) +
  annotate("rect", xmin = rect2_xmin, xmax = rect2_xmax, ymin = rect2_ymin, ymax = rect2_ymax, alpha = 0.2) +
  annotate(
    "text", size = ann_text_size, x = mean(c(rect2_xmin, rect2_xmax)), y = rect2_ymax+1,
    label = "Smaller decision impact as a lead increases"
  ) +
  annotate(
    "text", size = ann_text_size, color = bearcats_colors[1],
    x = worst_play$uc_lead + 15, y = worst_play$decision_value + 0.25, 
    label = paste(strwrap("Worst Decision - Punting back to Georgia which led to their game winning field goal in the Peach Bowl", 55), collapse = "\n")
  ) +
  annotate("segment", x = worst_play$uc_lead+0.5, xend = worst_play$uc_lead + 1.7, y = worst_play$decision_value + 0.2, yend = worst_play$decision_value + 0.7, colour = "black")


# not used ----------------------------------------------------------------

## Go for it Choices by Lead
ggplot(filter(plays, choice == "Go"), aes(x = uc_lead, y = decision_value, color = recommendation)) + geom_point()


## Choice FG Range vs Reco FG Range
plays %>% 
  filter(choice == "FG") %>% 
  .$yards_to_goal %>% summary()

plays %>% 
  filter(recommendation == "FG") %>% 
  .$yards_to_goal %>% summary()

ggplot(plays, aes(x = yards_to_goal)) +
  geom_histogram(data = filter(plays, recommendation == "FG"), aes(x = yards_to_goal), fill = "red", alpha = 0.2) +
  geom_histogram(data = filter(plays, choice == "FG"), aes(x = yards_to_goal), fill = "black", alpha = 0.2)


## Choice WP vs Reco WP
plays <- plays %>% 
  mutate(
    model_wp = case_when(
      recommendation == "FG" ~ fg_wp,
      recommendation == "Punt" ~ punt_wp,
      recommendation == "Go" ~ go_wp
    ),
    choice_wp = case_when(
      choice == "FG" ~ fg_wp,
      choice == "Punt" ~ punt_wp,
      choice == "Go" ~ go_wp
    ),
    wp_diff = model_wp - choice_wp
  )

ggplot(plays, aes(x = model_wp, y = choice_wp, color = model_wp == choice_wp)) + 
  geom_point() + geom_abline()
