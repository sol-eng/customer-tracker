#* @apiTitle Customer Tracker
#* @apiDescription See the percentage breakdown of KPI's by week.
#* @apiVersion 1.0.1

library(readr)
library(tidyverse)
library(plumber)

curr <- read_csv("data/trackerCalcCurr.csv")
pre <- read_csv("data/trackerCalcPre.csv")
users <- read_csv("data/users.csv")

f <- function(x, y) {(y / x - 1)}

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
    Week = 1:52,
    RevenuePre = dollarsPre,
    RevenueCurr = dollarsCurr,
    Revenue = f(dollarsPre, dollarsCurr),
    CustomersPre = filter(users, segment == seg) %>% .$pre,
    CustomersCurr = filter(users, segment == seg) %>% .$curr,
    Customers = f(CustomersPre, CustomersCurr),
    VisitsPre = purchasesPre / CustomersPre,
    VisitsCurr = purchasesCurr / CustomersCurr,
    Visits = f(VisitsPre, VisitsCurr),
    ItemsPre = itemsPre / purchasesPre,
    ItemsCurr = itemsCurr / purchasesCurr,
    Items = f(ItemsPre, ItemsCurr),
    SpendPre = dollarsPre / itemsPre,
    SpendCurr = dollarsCurr / itemsCurr,
    Spend = f(SpendPre, SpendCurr)
  ) %>%
  filter(Week <= 22) %>%
  #arrange(desc(Week)) %>%
  select(
    Week, RevenuePre, RevenueCurr, Revenue,
    CustomersPre, CustomersCurr, Customers,
    VisitsPre, VisitsCurr, Visits,
    ItemsPre, ItemsCurr, Items,
    SpendPre, SpendCurr, Spend
  )
}

#* @get /data
#* @param per Period (Week, YTD)
#* @param grp Group (Total, Core, Extra)
#* @param seg Segment (Total, Heavy, Mainstream, Focus1, Focus2, Specialty, Diverse1, Diverse2, Other, New)
out <- function(seg = "Total", grp = "Total", per = "Week"){
  dat(seg, grp, per) %>%
    select(Week, RevenuePre, RevenueCurr, Revenue, Customers, Visits, Items, Spend)
}

#* @get /plot
#* @param per Period (Week, YTD)
#* @param grp Group (Total, Core, Extra)
#* @param seg Segment (Total, Heavy, Mainstream, Focus1, Focus2, Specialty, Diverse1, Diverse2, Other, New)
#* @png
plot <- function(seg = "Total", grp = "Total", per = "Week"){

  pdat <- dat(seg, grp, per) %>%
    select(Week, Revenue, Customers, Visits, Items, Spend) %>%
    gather(seg, metric, -Week) %>%
    mutate(metric = round(100 * metric, 2)) %>%
    mutate(seg = factor(seg, levels = c("Spend", "Items", "Visits", "Customers", "Revenue")))
  
  p1 <- ggplot(filter(pdat, seg != "Revenue"), aes(Week, metric, fill = seg)) +
    geom_bar(stat = "Identity") + 
    geom_line(data = filter(pdat, seg == "Revenue"), aes(Week, metric), col = "darkgrey") +
    scale_fill_manual(values = alpha(c("orange", "salmon", "darkgrey", "lightgreen", "lightblue"), 0.5)) +
    labs(x = "Week", y = "Percent", title = "Percentage change by Week") +
    theme_minimal() +
    theme(legend.title=element_blank())

  print(p1)
}
