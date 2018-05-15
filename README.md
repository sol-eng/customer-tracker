# Replacing Excel with R

Creating and managing complex Excel dashboards leads to issues with accuracy and compliance. Creating your dashboards with reproducible code improves accuracy and opens the door for greater collaboration. You can use parameterized R Markdown reports to create complex, interactive dashboards in R. Hosting these dashboards securely in RStudio Connect gives you control over who has access and what operations are run. By using the R programming language along with R Markdown and RStudio Connect you avoid mainy of the issues that come with Excel.

## Comparison

Solution              | Author  | Render  | Audience       | Snapshot   | Capabilities
----------------------|---------|---------|----------------|------------|----------------
0. Excel Workbook     | Anyone  | Dynamic | Push (email)   | Save as... | Excel functions
1. R Markdown Report  | R users | Batch   | Push (email)   | Yes        | Full power of R
2. Shiny Application  | R users | Dynamic | Pull (website) | No*        | Full power of R
3. R Markdown + Shiny | R users | Dynamic | Pull (website) | No*        | Full power of R

*\* Snapshots with Shiny can be rendered by the user, but typically require an R Markdown file as input. For more details, see [Generating Downloadable Reports]() and this [example](https://shiny.rstudio.com/gallery/download-knitr-reports.html).*

## Documents

These assets are published to [RStudio Connect](https://www.rstudio.com/products/connect/). Click [here](https://beta.rstudioconnect.com/connect/) to try out RStudio Connect.

* [R Markdown Report](http://colorado.rstudio.com:3939/content/1095/tracker-report.nb.html). If you log into RStudio Connect you can create custom report versions, email yourself a copy, and download the associated report in Excel.
* [Shiny application](). If you log into R Studio Connect you can adjust the runtime settings and inspect the logs.
* [Flexdashboard](http://colorado.rstudio.com:3939/content/1094/). If you log into R Studio Connect you can adjust the runtime settings and inspect the logs.

## RStudio Connect

[RStudio Connect](https://beta.rstudioconnect.com/connect/) is a server product from RStudio for secure sharing of R content. It is on-premises software you run behind your firewall. You keep control of your data and who has access. With RStudio Connect you can see all your content, decide who should be able to view and collaborate on it, tune performance, schedule updates, and view logs. You can schedule your R Markdown reports to run automatically or even distribute the latest version by email.
