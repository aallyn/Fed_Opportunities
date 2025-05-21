#!/bin/zsh
export PATH="/usr/sbin:/usr/local/bin:/usr/bin:/bin"

# Use the passwordless SSH key
export GIT_SSH_COMMAND="ssh -i ~/.ssh/id_ed25519_nopass -o IdentitiesOnly=yes"

# Move into the project directory
cd /Users/aallyn/GitHub/Fed_Opportunities || exit 1

# Get today's date in YYYY-MM-DD format
today=$(date "+%Y-%m-%d")

# Step 1: Run R script to fetch web pages and prep data
/usr/local/bin/Rscript download_pages_and_render.R

# Step 2: Render HTML and PDF (Quarto auto-detects formats)
/Applications/quarto/bin/quarto render /Users/aallyn/GitHub/Fed_Opportunities/Opportunity_Report.qmd --no-cache --output-dir docs

# Step 3: Send the report email using Microsoft365R
/usr/local/bin/Rscript email_summary_report.R "${today}"

# Step 4: Publish the new report to GitHub Pages
git add docs/
git commit -m "Auto-update report for ${today}" || echo "No changes to commit"
git push origin main