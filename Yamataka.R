require(dplyr)
require(data.table)

# ************************************************************************* ----
# Packages ----
# ************************************************************************* ----

library("ggplot2")
library("dplyr")

# ************************************************************************* ----
# Original data ----
# ************************************************************************* ----

df <- 
  data.frame(
    x.axis.Var = rep(c("Widgets", "Gridgets", "Groms", "Wobs"), 4),
    cat.Var = rep(c("High End", "Mid Range", "Low End","another"), each = 4),
    values = c(600, 500, 300, 200, # high end
               300, 200, 300, 250, # mid range
               100, 80, 200, 150,  # low end
               500, 200, 0 , 50   # Another
    )
  )

# ************************************************************************* ----
# Data for Waterfall Chart ----
# ************************************************************************* ----

df.tmp <- df %>%
  # \_Set the factor levels in the order you want ----
mutate(
  x.axis.Var = factor(x.axis.Var,
                      levels = c("Widgets", "Gridgets", "Groms", "Wobs")),
  cat.Var = factor(cat.Var,
                   levels = c("Low End", "Mid Range", "High End","another"))
) %>%
  # \_Sort by Group and Category ----
arrange(x.axis.Var, desc(cat.Var)) %>%
  # \_Get the start and end points of the bars ----
mutate(end.Bar = cumsum(values),
       start.Bar = c(0, head(end.Bar, -1))) %>%
  # \_Add a new Group called 'Total' with total by category ----
rbind(
  df %>%
    # \___Sum by Categories ----
  group_by(cat.Var) %>% 
    summarise(values = sum(values)) %>%
    # \___Create new Group: 'Total' ----
  mutate(
    x.axis.Var = "Total",
    cat.Var = factor(cat.Var,
                     levels = c("Low End", "Mid Range", "High End", "another"))
  ) %>%
    # \___Sort by Group and Category ----
  arrange(x.axis.Var, desc(cat.Var)) %>%
    # \___Get the start and end points of the bars ----
  mutate(end.Bar = cumsum(values),
         start.Bar = c(0, head(end.Bar, -1))) %>%
    # \___Put variables in the same order ----
  select(names(df),end.Bar,start.Bar)
) %>%
  # \_Get numeric index for the groups ----
mutate(group.id = group_indices(., x.axis.Var)) %>%
  # \_Create new variable with total by group ----
group_by(x.axis.Var) %>%
  mutate(total.by.x = sum(values)) %>%
  # \_Order the columns ----
select(x.axis.Var, cat.Var, group.id, start.Bar, values, end.Bar, total.by.x)

# ************************************************************************* ----
# Plot ----
# ************************************************************************* ----

ggplot(df.tmp, aes(x = group.id, fill = cat.Var)) + 
  # \_Simple Waterfall Chart ----
geom_rect(aes(x = group.id,
              xmin = group.id - 0.25, # control bar gap width
              xmax = group.id + 0.25, 
              ymin = end.Bar,
              ymax = start.Bar),
          color="black", 
          alpha=0.95) + 
  # \_Lines Between Bars ----
geom_segment(aes(x=ifelse(group.id == last(group.id),
                          last(group.id),
                          group.id+0.25), 
                 xend=ifelse(group.id == last(group.id),
                             last(group.id),
                             group.id+0.75), 
                 y=ifelse(cat.Var == "Low End",
                          end.Bar,
                          # these will be removed once we set the y limits
                          max(end.Bar)*2), 
                 yend=ifelse(cat.Var == "Low End",
                             end.Bar,
                             # these will be removed once we set the y limits
                             max(end.Bar)*2)), 
             colour="black") +
  # \_Numbers inside bars (each category) ----
geom_text(
  mapping = 
    aes(
      label = ifelse(values < 150, 
                     "",
                     ifelse(nchar(values) == 3,
                            as.character(values),
                            sub("(.{1})(.*)", "\\1.\\2", 
                                as.character(values)
                            )
                     )
      ),
      y = rowSums(cbind(start.Bar,values/2))
    ),
  color = "white",
  fontface = "bold"
) + 
  # \_Total for each category above bars ----
geom_text(
  mapping = 
    aes(
      label = ifelse(cat.Var != "Low End", 
                     "",
                     ifelse(nchar(total.by.x) == 3,
                            as.character(total.by.x),
                            sub("(.{1})(.*)", "\\1.\\2", 
                                as.character(total.by.x)
                            )
                     )
      ),
      y = end.Bar+200
    ),
  color = "#4e4d47",
  fontface = "bold"
) + 
  # \_Change colors ----
scale_fill_manual(values=c('#c8f464','#ff6969','#55646e','#171ed8')) +
  # \_Change y axis to same scale as original ----
scale_y_continuous(
  expand=c(0,0),
  limits = c(0, 5000),
  breaks = seq(0, 5000, 500),
  labels = ifelse(nchar(seq(0, 5000, 500)) < 3,
                  as.character(seq(0, 5000, 500)),
                  sub("(.{1})(.*)", "\\1.\\2", 
                      as.character(seq(0, 5000, 500))
                  )
  )
) +
  # \_Add tick marks on x axis to look like the original plot ----
scale_x_continuous(
  expand=c(0,0),
  limits = c(min(df.tmp$group.id)-0.5,max(df.tmp$group.id)+0.5),
  breaks = c(min(df.tmp$group.id)-0.5,
             unique(df.tmp$group.id), 
             unique(df.tmp$group.id) + 0.5
  ),
  labels = 
    c("", 
      as.character(unique(df.tmp$x.axis.Var)), 
      rep(c(""), length(unique(df.tmp$x.axis.Var)))
    )
) +
  # \_Theme options to make it look like the original plot ----
theme(
  text = element_text(size = 14, color = "#4e4d47"),
  axis.text = element_text(size = 10, color = "#4e4d47", face = "bold"),
  axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm")),
  axis.ticks.x =
    element_line(color =
                   c("black",
                     rep(NA, length(unique(df.tmp$x.axis.Var))),
                     rep("black", length(unique(df.tmp$x.axis.Var))-1)
                   )
    ),
  axis.line = element_line(colour = "#4e4d47", size = 0.5),
  axis.ticks.length = unit(.15, "cm"),
  axis.title.x =       element_blank(),
  axis.title.y =       element_blank(),
  panel.background =   element_blank(),
  plot.margin =        unit(c(1, 1, 1, 1), "lines"),
  legend.text =        element_text(size = 10, 
                                    color = "#4e4d47",
                                    face = "bold",
                                    margin = margin(l = 0.25, unit = "cm")
  ),
  legend.title =       element_blank()
)