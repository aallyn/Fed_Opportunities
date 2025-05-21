args <- commandArgs(trailingOnly = TRUE)
today <- args[1]

library(Microsoft365R)
library(glue)

outlook <- get_business_outlook()

html_body <- paste0(
  "<h2>GMRI Grants Summary – ", format(Sys.Date(), "%B %d, %Y"), "</h2>",
  "<p>Another money-mission Monday :)</p>",
  "<p>Attached is the latest <strong>HTML</strong> version of the GMRI Federal Opportunities report.</p>",
  "<p>Happy proposaling,</p>",
  "<p>Andrew</p>"
)

msg <- outlook$create_email(
  subject = sprintf("GMRI Grants Summary – %s", format(Sys.Date(), "%B %d, %Y"))
)

msg$set_body(body = html_body, content_type = "html")

# Define full paths
html_path <- file.path(here::here("docs", "Opportunity_Report.html"))
# pdf_path <- file.path(here::here("docs", "Opportunity_Report.pdf"))

# Confirm files exist
stopifnot(file.exists(html_path))
# stopifnot(file.exists(html_path), file.exists(pdf_path))

# Add attachments using full paths
msg$add_attachment(html_path)
# msg$add_attachment(pdf_path)

# Add recipients
msg$add_recipients(to = c("aallyn@gmri.org"))

# Send
msg$send()



