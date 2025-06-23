# library(blastula)
# library(glue)
# library(here)

# # Today's date
# today <- Sys.Date()
# report_url <- "https://aallyn.github.io/Fed_Opportunities/Opportunity_Report.html"

# # Compose the HTML body
# html_body <- paste0(
#   "<h2>GMRI Grants Summary – ", format(today, "%B %d, %Y"), "</h2>",
#   "<p>Another money-mission Monday :)</p>",
#   "<p>Click below to view the latest <strong>interactive HTML</strong> version of the GMRI Federal Opportunities report:</p>",
#   "<p><a href='", report_url, "'>", report_url, "</a></p>",
#   "<p>Happy proposaling,</p>",
#   "<p>Andrew</p>"
# )

# # Compose the email using blastula
# email <- compose_email(
#   body = md(html_body),
#   footer = md("Sent via GitHub Actions using blastula.")
# )

# # File paths to attachments
# html_path <- here("docs", "Opportunity_Report.html")
# # pdf_path <- here("docs", "Opportunity_Report.pdf")

# # Optional: Check that attachments exist before sending
# stopifnot(file.exists(html_path))
# # stopifnot(file.exists(pdf_path))

# # Send the email
# smtp_send(
#   email = email,
#   from = "aallyn@gmri.org",
#   to = "aallyn@gmri.org",
#   subject = glue("GMRI Grants Summary – {format(today, '%B %d, %Y')}"),
#   credentials = creds(
#     host = Sys.getenv("SMTP_HOST"),
#     port = as.integer(Sys.getenv("SMTP_PORT")),
#     user = Sys.getenv("SMTP_USER"),
#     password = Sys.getenv("SMTP_PASS"),
#     use_ssl = TRUE
#   ),
#   attachments = c(html_path)  # , pdf_path
# )

args <- commandArgs(trailingOnly = TRUE)
today <- args[1]

library(Microsoft365R)
library(glue)

outlook <- get_business_outlook()

report_url <- "https://aallyn.github.io/Fed_Opportunities/Opportunity_Report.html"

html_body <- paste0(
  "<h2>GMRI Grants Summary – ", format(Sys.Date(), "%B %d, %Y"), "</h2>",
  "<p>Another money-mission Monday :)</p>",
  "<p>Click below to view the latest <strong>interactive HTML</strong> version of the GMRI Federal Opportunities report:</p>",
  "<p><a href='", report_url, "'>", report_url, "</a></p>",
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
# msg$add_attachment(html_path)
# msg$add_attachment(pdf_path)

# Add recipients
msg$add_recipients(to = c("aallyn@gmri.org"))

# Send
msg$send()

