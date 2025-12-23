# Ukrainian vs Russian Language Search Trends in Ukraine: analyzing language preference shifts through search behavior

## Overview
This project analyzes the temporal and geographic distribution of Ukrainian vs Russian language searches in Ukraine from 2010-2025, using Google Trends data. The goal is to understand whether Ukrainians are shifting their digital language habits, especially in response to major political events like the 2014 Euromaidan Revolution and the 2022 Russian invasion.

## Key Findings (Preliminary)

- **Sharp increase in Ukrainian searches after 2022**: The Ukrainian-to-Russian search ratio shows a clear upward trend across almost all regions following the full-scale invasion
- **Regional divide persists**: Western oblasts (Ternopil, Ivano-Frankivsk, Lviv) heavily favor Ukrainian searches, while eastern/southern regions (Kharkiv, Odesa, Dnipro) still predominantly search in Russian
- **Central Ukraine shows largest growth**: Regions like Khmelnytska and Poltavska oblasts show the highest proportional increases in Ukrainian search ratios

## Methodology

- **Data Source**: Google Trends API via `gtrendsR` package
- **Search Terms**: Common queries in both Ukrainian and Russian (e.g., "що" vs "что" for "what", "новини" vs "новости" for "news")
- **Metric**: Ukrainian-to-Russian ratio = Ukrainian search score / Russian search score
- **Time Period**: Weekly data from 2010-2025 (national), full period (regional)
- **Geographic Coverage**: All Ukrainian oblasts using ISO subregion codes

### Limitations
- Search behavior ≠ actual language use (digital proxy only)
- Historical Russian content bias may overstate Russian usage
- Reflects internet users only
- Google Trends uses relative (0-100) rather than absolute measures
- Occupied territories (Crimea, parts of Donbas) have incomplete/biased data after 2014

## Repository Structure

```
├── Analysis.qmd              # Main analysis document (Quarto)
├── Analysis.html             # Rendered report
├── Main Code/                # R scripts for data collection & analysis
├── Maps/                     # Generated geographic visualizations
├── Figures/                  # Time series plots and charts
├── Shapefiles/               # Ukrainian administrative boundaries
└── Wikimedia/                # Wikipedia pageview data (planned addition)
```

## Planned Additions

- [ ] Expand to more search terms (currently limited by API caps)
- [ ] Incorporate Wikimedia pageview statistics
- [ ] Statistical breakpoint analysis around key events
- [ ] More sophisticated regional clustering
- [ ] Interactive visualizations
