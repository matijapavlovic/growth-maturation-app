# Growth & Maturation App  
<img src="logo.png" width="220" align="right" />

A Shiny Application for Maturity Offset (Mirwald), PHV Prediction, and Predicted Adult Height (Khamis–Roche) estimation.


---

## 🔗 **Live App**
👉 Use the app here:  
https://matijapavlovic.shinyapps.io/g_mapp/

---

## 📥 **Download Template**
The Excel input template is included in this repository:  
**➡️ g_mApp_template.xlsx**

---

## 📌 **Overview**

This Shiny application allows coaches, practitioners, and sport scientists to:

- Calculate **maturity offset** using Mirwald (2002)  
- Estimate **Age at Peak Height Velocity (PHV)**  
- Estimate **Predicted Adult Height (PAH)** using Khamis–Roche  
- Visualize:
  - Growth curve over time  
  - % Predicted Adult Height zone  
  - Maturity offset position relative to PHV  
- Automatically generate **PDF reports**

Built entirely with **R, Shiny, ggplot2, tidyverse, plotly, and rmarkdown**.

---

## 🧠 **Methods Implemented**

### **Mirwald et al. (2002) — Maturity Offset**
Predicts years from PHV using anthropometric variables:

- Standing height  
- Sitting height  
- Body mass  
- Chronological age  

PHV age is calculated as:
PHV Age = Chronological Age − Maturity Offset


---

### **Khamis & Roche (1994) — Predicted Adult Height**
Estimates adult height **without skeletal age**, using:

- Child height  
- Child body mass  
- Mid-parent height  
- Age- and sex-specific proportional multipliers  

Best validated for children of **White/European ancestry** — less accurate for others.

---

## 📊 **Visualizations in the App**

- **Height growth curve** with PHV marker  
- **% of Predicted Adult Height gauge**
- **Maturity offset bar**
- **Growth tempo** gauge (if multiple time points provided)

All visuals are optimized for dark mode and export cleanly into the PDF report.

---

## 📄 **PDF Report Generation**

The app automatically generates a **professional PDF report** summarizing:

- Athlete information  
- Chronological & biological age  
- Offset and Age at PHV  
- % Predicted Adult Height  
- Growth curve  
- PHV zone chart  
- Maturity offset chart  

### ⚠️ For local use:
If PDF rendering fails, install TinyTeX:

```r
tinytex::install_tinytex()

📂 Repository Structure
growth-maturation-app/
│
├── app.R
├── g_mApp_template.xlsx
├── logo.png
└── README.md
```

🖥 Run the App Locally

Install required packages:
```
install.packages(c(
  "shiny","ggplot2","readxl","dplyr","bslib",
  "rmarkdown","knitr","lubridate","plotly","tibble"
))
```

Run the app:
```
shiny::runApp("path/to/app/folder")
```

📚 **Related Work & Inspiration**

The design, structure, and validation were inspired by excellent existing R packages:

matuR

Fernandez J. (2020). matuR: Athlete Maturation and Biobanding.
https://github.com/josedv82/matuR

@Manual{
  title = {matuR: Athlete Maturation and Biobanding},
  author = {Jose Fernandez},
  year = {2020},
  note = {R package version 0.0.0.9000},
  url = {https://github.com/josedv82/matuR},
}

ageR

Kikhia A. (2023). ageR: Athlete Growth & Maturation.
https://github.com/a-kikhia11/ageR

👤 About the Author

Matija Pavlovic
Strength & Conditioning Coach

🔗 Links

GitHub: https://github.com/matijapavlovic

LinkedIn: http://linkedin.com/in/matija-pavlovic

Twitter (X): https://x.com/MatijaPavlovic

Linktree: https://linktr.ee/matijapav

🏋️‍♂️ Purpose of the App

To give coaches a simple, reliable, scientifically-grounded tool for assessing growth & maturation in youth athletes — without spreadsheets, manual calculations, or coding.

🤝 Contribute

Feedback, issues, and pull requests are welcome!
