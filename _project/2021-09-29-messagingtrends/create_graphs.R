#----------------------------------------------------
# Graph messaging data
#
# Created By: Francine Stephens
# Date Created: 09/10/21
# Date Last Updated: 09/29/21
#----------------------------------------------------

## Set-up-----------------------------------------------------------------------
packages <- c("readr", 
              "tidyverse", 
              "lubridate",
              "ggplot2",
              "cowplot"
)
lapply(packages, library, character.only = TRUE)

# Paths
wd <- getwd()

slack_data_file <- "slack_weekly_pct_chg.csv"
email_data_file <- "email_weekly_pct_chg.csv"
zoom_data_file <-  "zoom_weekly_pct_chg.csv"

# Set Functions & Parameters
zoom_bl <- 2815756


# Import Data
slack <- read_csv(paste0(wd, "/", slack_data_file))
email <- read_csv(paste0(wd, "/", email_data_file))
zoom <- read_csv(paste0(wd, "/", zoom_data_file),
                 )


# JOIN DATASETS-----------------------------------------------------------------
slack_for_graph <- slack %>% 
  mutate(Type = "Slack messages") %>%
  relocate(Type, .before = "pchg_from_bl") %>%
  add_row(Week = as.Date("2020-03-01"), 
          week_post_pandemic = 0,
          Type = "Slack messages", 
          Total = 0,
          pchg_from_bl = 0, .before = 1) %>%
  mutate(
         week_label = as.character(week_post_pandemic),
         week_label = paste0("+", week_label, " Weeks"),
         week_label = case_when(week_label == "+1 Weeks" ~ "+1 Week",
                                week_label == "+0 Weeks" ~ "Baseline",
                                TRUE ~ week_label)
         ) 

email_for_graph <- email %>% 
  filter(Week >= as.Date("2020-03-01")) %>%
  left_join(., slack %>% 
              select(Week, week_post_pandemic),
            by = "Week") %>%
  relocate(week_post_pandemic, .after = "Week") %>%
  mutate(week_post_pandemic = replace_na(week_post_pandemic, 0),
        week_label = as.character(week_post_pandemic),
        week_label = paste0("+", week_label, " Weeks"),
        week_label = case_when(week_label == "+1 Weeks" ~ "+1 Week",
                               week_label == "+0 Weeks" ~ "Baseline",
                               TRUE ~ week_label)
) 

missing_email_dates <- slack_for_graph %>%
  filter(!Week %in% email_for_graph$Week) %>% 
  select(-Total, -pchg_from_bl) %>%
  mutate(Type = "Email messages", 
         Total = NA,
         pchg_from_bl = NA
         ) %>%
  relocate(Week, week_post_pandemic, Total, Type, pchg_from_bl, week_label)

email_for_graph <- bind_rows(email_for_graph,
                             missing_email_dates
)


zoom_for_graph <- zoom %>% 
  mutate(Week = as.Date(Week, "%m/%d/%Y"),
         pchg_from_bl = (
           (Total_min - zoom_bl)/zoom_bl) * 100) %>%
  arrange(Week) %>%
  left_join(., slack %>% 
            select(Week, week_post_pandemic),
          by = "Week") %>%
  relocate(week_post_pandemic, .after = "Week") %>% 
  add_row(Week = as.Date("2020-03-01"), 
          week_post_pandemic = 0,
          Total_min = zoom_bl,
          Type = "Meeting minutes",
          pchg_from_bl = 0, .before = 1) %>%
  mutate(
         week_label = as.character(week_post_pandemic),
         week_label = paste0("+", week_label, " Weeks"),
         week_label = case_when(week_label == "+1 Weeks" ~ "+1 Week",
                                week_label == "+0 Weeks" ~ "Baseline",
                                TRUE ~ week_label)
  ) %>%
  select(-Total_min)
  

messages <- bind_rows(slack_for_graph,
                      email_for_graph, 
                      zoom_for_graph
                      )

# Create specific labels
x_labs <- messages %>% 
  distinct(week_label) %>% 
  mutate(week_label = as.factor(week_label)) %>%
  pull(.)

# GRAPHS------------------------------------------------------------------------
jpeg(file="voe_messaging_line_replicate_annotate.jpg", res=600, width=9600, height=4800,
     pointsize=10, type="windows", antialias="cleartype")
ggplot(messages %>%
         filter(Week <= as.Date("2021-08-31")),
       aes(x = Week, y = pchg_from_bl, color = Type)) + 
  geom_line(aes(linetype = Type), size = 1) + 
  geom_point(size = 3) + 
  annotate("rect",xmin = as.Date("2020-02-28"), xmax = as.Date("2021-09-01"),
           ymin = 50, ymax = Inf, alpha = .1,
           fill = "red") +
  annotate("rect", xmin = as.Date("2020-02-28"), xmax = as.Date("2021-09-01"),
           ymin = -50, ymax = -Inf, alpha = .1,
           fill = "red") + 
  geom_vline(xintercept = as.Date("2021-01-01"),
             color = "black",
             linetype = "dashed",
             size = 1.2
             ) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = "dotted") + 
  annotate("segment", x = as.Date("2021-01-01"), xend = as.Date("2021-01-01"),
             y = 550, yend = 575, linetype = "dashed", size = 1.2) +
  annotate(
    geom = "curve", x = as.Date("2020-03-09"), y = 220,
    xend = as.Date("2020-03-07"), yend = 50,
    curvature = 0.2,
    arrow = arrow(length = unit(2, "mm")),
    size = 1.2
  ) + 
  annotate(geom = "text", x = as.Date("2020-03-10"), y = 220,
           label = "Start of WFH", hjust = "left", size = 5) + 
  annotate(
    geom = "segment", x = as.Date("2020-09-03"), y = 529.86136,
    xend = as.Date("2020-08-26"), yend = 529.86136,
    arrow = arrow(length = unit(2, "mm")),
    size = 1.2
  ) + 
  annotate(geom = "text", x = as.Date("2020-09-05"), y = 529.86136,
           label = "Week of Aug. 23", hjust = "left", size = 5) + 
  annotate(
    geom = "curve", x = as.Date("2021-01-09"), y = -55,
    xend = as.Date("2020-12-27"), yend = -42,
    curvature = -0.3,
    arrow = arrow(length = unit(2, "mm")),
    size = 1.2
  ) + 
  annotate(
    geom = "curve", x = as.Date("2021-03-03"), y = -55,
    xend = as.Date("2021-07-02"), yend = -35,
    curvature = 0.1,
    arrow = arrow(length = unit(2, "mm")),
    size = 1.2
  ) + 
  annotate(geom = "text", x = as.Date("2021-01-10"), y = -55,
           label = "Recharge Days", hjust = "left", size = 5) + 
  annotate(geom = "text", x = as.Date("2020-07-15"), y = Inf,
           label = "2020", vjust = -0.8, size = 5) + 
  annotate(geom = "text", x = as.Date("2021-05-15"), y = Inf,
           label = "2021", vjust = -0.8, size = 5) +  
  scale_x_date(date_breaks = "1 month",
               date_minor_breaks = "1 week",
               date_labels = "%b",
               expand = c(0, 4)
                 ) +
  scale_y_continuous(breaks = seq(-150, 550, 50),
                     labels = scales::percent_format(accuracy = 1, scale = 1),
                     expand = c(0,0)
                     ) +
  coord_cartesian(ylim = c(-100, 550), clip = "off") +
  scale_color_manual(values = c("#0096FF","#A020F0", "#E97451")) + 
  scale_linetype_manual(values=c("dashed", "solid", "twodash"))+
  labs(title = "WoW Messaging Trends in WFH",
       y = "Percent Change from Baseline",
       x = "", 
       caption = "Note: Baseline represents the three month weekly average prior to WFH.") + 
  theme_classic() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        text = element_text(family = "AvenirNext forINTUIT"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 13),
        #axis.line.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#d3d3d3"),
        panel.grid.major.y = element_line(colour = "#d3d3d3"),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 13)
        )
dev.off()
  
