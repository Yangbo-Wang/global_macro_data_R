# The Global Macro Database (R Package)
<a href="https://www.globalmacrodata.com" target="_blank" rel="noopener noreferrer">
    <img src="https://img.shields.io/badge/Website-Visit-blue?style=flat&logo=google-chrome" alt="Website Badge">
</a>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

[Link to paper 📄](https://www.globalmacrodata.com/research-paper.html)



This repository complements paper, **Müller, Xu, Lehbib, and Chen (2025)**, which introduces a panel dataset of **46 macroeconomic variables across 243 countries** from historical records beginning in the year **1086** until **2024**, including projections through the year **2030**.




## Features

- **Unparalleled Coverage**: Combines data from **32 contemporary sources** (e.g., IMF, World Bank, OECD) with **78 historical datasets**.
- **Extensive Variables**: GDP, inflation, government finance, trade, employment, interest rates, and more.
- **Harmonized Data**: Resolves inconsistencies and splices all available data together.
- **Scheduled Updates**: Regular releases ensure data reliability.
- **Full Transparency**: All code is open source and available in this repository.
- **Accessible Formats**: Provided in `.dta`, `.csv` and as **<a href="https://github.com/KMueller-Lab/Global-Macro-Database" target="_blank" rel="noopener noreferrer">Stata</a>
/<a href="https://github.com/Yangbo-Wang/global_macro_data_python" target="_blank" rel="noopener noreferrer">Python</a>/<a href="https://github.com/Yangbo-Wang/global_macro_data_R" target="_blank" rel="noopener noreferrer">R</a> package**.



## Data access

<a href="https://www.globalmacrodata.com/data.html" target="_blank" rel="noopener noreferrer">Download via website</a>

**R package:**
```
# Install devtools if not already installed
install.packages("devtools")
# Install the package from GitHub
devtools::install_github("Yangbo-Wang/global_macro_data_R")
```

**How to use (an example)**
```
# Load the Package
library(globalmacrodata)

# Download the Latest Available Data
data <- GMD()

# Download Data for a Specific Version (2025_01)
data <- GMD(year = 2025, quarter = 1)

# Filter Data by Country
data <- GMD(year = 2025, quarter = 1, country = "USA")
```

## Parameters
- **`year` (numeric)**: Representing the desired year (e.g., 2025). If NULL, the latest available dataset is used.
- **`quarter` (numeric)**: Eepresenting the quarter (1, 3, 6, or 9). If NULL, the latest available dataset is used.
- **`country` (character string)**: Specifying the ISO3 country code (e.g., "USA"). If NULL, returns all countries.

## Release schedule 

| Release Date | Details          |
|--------------|------------------|
| 2025-01-30   | Initial release: v2025-01 |
| 2025-04-01   | v2025-04         |
| 2025-07-01   | v2025-09         |
| 2025-10-01   | v2025-12         |
| 2026-01-01   | v2026-03         |



## Citation

To cite this dataset, please use the following reference:

```bibtex
@techreport{mueller2025global, 
    title = {The Global Macro Database: A New International Macroeconomic Dataset}, 
    author = {Müller, Karsten and Xu, Chenzi and Lehbib, Mohamed and Chen, Ziliang}, 
    year = {2025}, 
    type = {Working Paper}
}
```

## Acknowledgments

The development of the Global Macro Database would not have been possible without the generous funding provided by the Singapore Ministry of Education (MOE) through the PYP grants (WBS A-0003319-01-00 and A-0003319-02-00), a Tier 1 grant (A-8001749- 00-00), and the NUS Risk Management Institute (A-8002360-00-00). This financial support laid the foundation for the successful completion of this extensive project.
