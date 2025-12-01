📈 Growth & Maturation App
A Shiny Application for Maturity Offset (Mirwald), PHV, and PAH (Khamis–Roche) Maturity Estimation 
<img src="logo.png" width="130" align="right" />

Created by Matija Pavlovic

🔗 Live App

👉 Use the app here:
https://matijapavlovic.shinyapps.io/g_mapp/


📥 Download Template

The Excel input template is included inside this repository:
➡️ g_mApp_template.xlsx

📌 Overview

This Shiny application allows coaches, practitioners, and sport scientists to:

Calculate maturity offset using the Mirwald (2002) method

Estimate Age at Peak Height Velocity (PHV)

Estimate Adult Height using Khamis & Roche

Visualize:

Change of Height over Time

Maturity offset curve

The actual moment and predicted development

Generate automatic PDF reports

The app was built completely with R, Shiny, ggplot2, and rmarkdown.

🧠 Methods Included

The Mirwald equations estimate maturity offset (years from PHV) using anthropometric predictors:

Age

Standing height

Sitting height

Body mass

PHV is then calculated as:

PHV_age = Chronological_Age - Maturity_Offset

Khamis & Roche Method (1994)

This method estimates final adult height without skeletal age, using:

Current standing height

Current weight

Mid-parent height

Percentile-based multipliers

Used widely in pediatric and sport growth research.

Note: the method used is most accurate for children of white ancestry and may be less precise for other ethnic groups.

</details>
🗂 File Structure
growth-maturation-app/
│
├── app.R
├── g_mApp_template.xlsx
└── README.md

🖥 Running Locally
# install packages
install.packages(c(
  "shiny","ggplot2","readxl","dplyr","bslib",
  "rmarkdown","knitr","lubridate"
))

# run the app
shiny::runApp("path/to/app/folder")

📝 PDF Report Requirements

If PDF rendering fails on your machine, install TinyTeX:

tinytex::install_tinytex()


(Users running the cloud version do not need this.)

🚀 There is a lot of room for Future Improvements


👤 About the Author

Matija Pavlovic
Strength & Conditioning Coach

🔗 Links:

GitHub: https://github.com/matijapavlovic

LinkedIn: http://linkedin.com/in/matija-pavlovic

Twitter (X): https://x.com/MatijaPavlovic

Linktree: https://linktr.ee/matijapav

🏋️‍♂️ Why This App Exists

To provide coaches with a simple, reliable, and scientifically-grounded tool to track growth and maturation in youth athletes without needing to code, calculate by hand, or rely on spreadsheets.

📣 Contribute

Pull requests and suggestions are welcome.
