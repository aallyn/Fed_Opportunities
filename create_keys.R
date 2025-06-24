library(Microsoft365R)

# login once and save token to cache
outlook <- get_business_outlook(auth_type = "device_code") # or get_personal_outlook()

# Compose message
msg <- outlook$create_email(
  subject = "Your subject",
  body = "Body text"
)

# Add attachment(s)
msg$add_attachment(here::here("Opportunity_Report.html"))
msg$add_attachment("Opportunity_Report.pdf")

# Send email
msg$send()


library(blastula)
create_smtp_creds_key(
  id = "outlook",
  user = "aallyn@gmri.org",
  host = "smtp.office365.com",
  port = 587,
  use_ssl = FALSE,
  overwrite = TRUE
)
