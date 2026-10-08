**Welcome to my portfolio!**


Analyzing Global Volcanic Hazards Data (1500–2026)

**Author:** Nayonika Choudhury  
**Course:** EVR 628 Intro to Data Mgmt. & Visualization for Environmental 
Scientists 

**Repository:** `my_portfolio`  

---

My project examines global historical volcanic hazards (1500–2026) to analyze
the relationships between Volcanic Explosivity Index (VEI), volcano morphology,
and human fatalities. It is important to study and establish these patterns in 
order to better manage populations living in active volcano zones. Using cleaned
eruption data from NOAA NCEI Significant Volcanic Eruptions Database, this 
workflow generates plots & visuals exploring extent of fatality across the 
different explosivity categories, cumulative mortality by volcano type, 
and high-impact spatial eruption distributions across the world. 

---

**Data Source**

- **Dataset Name:** NOAA NCEI Significant Volcanic Eruptions Database
- **Raw File:** `data/raw/volcanoes.tsv`
- **Source URL:** https://www.ncei.noaa.gov/products/natural-hazards/tsunamis-earthquakes-volcanoes/volcanoes)

---

**Repository Structure**

```r

my_portfolio/
├── data/
│   ├── raw/
│   │   └── volcanoes.tsv                 
│   ├── processed/
│   │   └── clean_volcanoes.rds                 
│   └── output/
│       ├── plot1_vei_vs_fatalities.png
│       ├── plot2_volcano_type_deaths.png
│       └── plot3_spatial_impact_map.png        
├── scripts/
│   ├── 01_processing/
│   │   └── data_processing.R                   
│   └── 03_content/
│       └── data_visualization.R                
├── my_portfolio.Rproj                          
└── README.md               

```
