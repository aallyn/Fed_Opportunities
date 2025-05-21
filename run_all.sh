#!/bin/zsh
export PATH="/usr/sbin:/usr/local/bin:/usr/bin:/bin"

cd /Users/aallyn/GitHub/Fed_Opportunities || exit 1

# Run the R script
/usr/local/bin/Rscript download_pages_and_render.R

# Then run Quarto manually (with correct path)
"/Applications/quarto/bin/quarto" render Opportunity_Report.qmd --output index.html

git add .
git commit -m "Automated update on $(date)"
git push
