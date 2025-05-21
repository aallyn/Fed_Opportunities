#!/bin/zsh
export PATH="/usr/sbin:/usr/local/bin:/usr/bin:/bin"

# Start the ssh-agent if it's not already running
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

cd /Users/aallyn/GitHub/Fed_Opportunities || exit 1

# Get today's date in YYYY-MM-DD format
today=$(date "+%Y-%m-%d")

# Run your R script to download pages and prepare data
/usr/local/bin/Rscript download_pages_and_render.R

# Render HTML and PDF (Quarto auto-detects formats in your .qmd)
/Applications/quarto/bin/quarto render Opportunity_Report.qmd

# Send the email report using Microsoft365R OAuth script
/usr/local/bin/Rscript email_summary_report.R "${today}"

# Auto-publish to GitHub Pages
git add docs/
git commit -m "Auto-update report for ${today}"
git push origin main