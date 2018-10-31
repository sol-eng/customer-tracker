#* @apiTitle Customer Tracker
#* @apiDescription See the percentage breakdown of KPI's by week.
#* @apiVersion 1.0.1

library(readr)
library(tidyverse)
library(plumber)

curr <- read_csv("data/trackerCalcCurr.csv")
pre <- read_csv("data/trackerCalcPre.csv")
users <- read_csv("data/users.csv")

f <- function(x, y) {100 * (y / x - 1)}

dat <- function(seg, grp, per){
  bind_cols(
    curr %>%
      filter(segment == seg) %>%
      select(ends_with(paste0(grp, per))) %>%
      rename_at(1:3, ~c("purchasesCurr", "itemsCurr", "dollarsCurr")),
    pre %>%
      filter(segment == seg) %>%
      select(ends_with(paste0(grp, per))) %>%
      rename_at(1:3, ~c("purchasesPre", "itemsPre", "dollarsPre"))
  ) %>%
    mutate(
      week = 1:52,
      dollarsPct = f(dollarsPre, dollarsCurr),
      usersPre = filter(users, segment == seg) %>% .$pre,
      usersCurr = filter(users, segment == seg) %>% .$curr,
      usersPct = f(usersPre, usersCurr),
      purUserPre = purchasesPre / usersPre,
      purUserCurr = purchasesCurr / usersCurr,
      purUserPct = f(purUserPre, purUserCurr),
      itemsPurPre = itemsPre / purchasesPre,
      itemsPurCurr = itemsCurr / purchasesCurr,
      itemsPurPct = f(itemsPurPre, itemsPurCurr),
      dollItemsPre = dollarsPre / itemsPre,
      dollItemsCurr = dollarsCurr / itemsCurr,
      dollItemsPct = f(dollItemsPre, dollItemsCurr)
    ) %>%
    filter(week <= 22) %>%
    select(
      week, dollarsPre, dollarsCurr, dollarsPct,
      usersPre, usersCurr, usersPct,
      purUserPre, purUserCurr, purUserPct,
      itemsPurPre, itemsPurCurr, itemsPurPct,
      dollItemsPre, dollItemsCurr, dollItemsPct
    )
}

#* @get /data
#* @param per Period (Week, YTD)
#* @param grp Group (Total, Core, Extra)
#* @param seg Segment (Total, Heavy, Mainstream, Focus1, Focus2, Specialty, Diverse1, Diverse2, Other, New)
out <- function(seg = "Total", grp = "Total", per = "Week"){
  dat(seg, grp, per) %>%
    select(week, dollarsPct, usersPct, purUserPct, itemsPurPct, dollItemsPct) %>%
    mutate_at(vars(dollarsPct:dollItemsPct), round, 2)
}

#* @get /plot
#* @param per Period (Week, YTD)
#* @param grp Group (Total, Core, Extra)
#* @param seg Segment (Total, Heavy, Mainstream, Focus1, Focus2, Specialty, Diverse1, Diverse2, Other, New)
#* @png
plot <- function(seg = "Total", grp = "Total", per = "Week"){
  
  pdat <- dat(seg, grp, per) %>%
    select(week, dollarsPct, usersPct, purUserPct, itemsPurPct, dollItemsPct) %>%
    gather(seg, metric, -week) %>%
    mutate(metric = round(metric, 2))
  
  p1 <- ggplot(data = filter(pdat, seg != "dollarsPct"), aes(week, metric, fill = seg)) +
    geom_bar(stat = "Identity") + 
    geom_line(data = filter(pdat, seg == "dollarsPct"), aes(week, metric), col = "darkgrey") +
    scale_fill_manual(values = alpha(c("darkgrey", "lightgreen", "salmon", "lightblue", "orange"), 0.5)) +
    labs(x = "Week", y = "Percent", title = "Percentage change by Week") +
    theme_minimal()
  
  print(p1)
}

