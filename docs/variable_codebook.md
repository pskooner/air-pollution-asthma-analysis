# Variable Codebook

This codebook describes the variables contained in the combined air quality and asthma emergency department analytical dataset used in this project.

## Geographic and Temporal Variables

| Variable | Type | Coding / Values | Description | Role |
|---|---|---|---|---|
| `State` | Categorical (nominal) | U.S. state names | U.S. state in which the county is located | Geographic adjustment variable |
| `County` | Categorical (nominal) | County names | County or county-equivalent geographic unit | Geographic identifier |
| `Year` | Numeric (integer) | 2023 | Year of observation | Temporal identifier |

## Air Quality Variables

| Variable | Type | Coding / Values | Description | Role |
|---|---|---|---|---|
| `Days with AQI` | Numeric | 0–366 | Total number of days for which AQI was recorded | Denominator for AQI-based proportions |
| `Good Days` | Numeric | Count | Number of days with AQI 0–50 | Air-quality measure |
| `Moderate Days` | Numeric | Count | Number of days with AQI 51–100 | Air-quality measure |
| `Unhealthy for Sensitive Groups Days` | Numeric | Count | Number of days with AQI 101–150 | Elevated-risk air-quality measure |
| `Unhealthy Days` | Numeric | Count | Number of days with AQI 151–200 | High-risk air-quality measure |
| `Very Unhealthy Days` | Numeric | Count | Number of days with AQI 201–300 | Very-high-risk air-quality measure |
| `Hazardous Days` | Numeric | Count | Number of days with AQI 301–500 | Hazardous air-quality measure |
| `Days CO` | Numeric | Count | Number of days in which carbon monoxide was the primary pollutant | Pollutant-specific measure |
| `Days NO2` | Numeric | Count | Number of days in which nitrogen dioxide was the primary pollutant | Pollutant-specific measure |
| `Days Ozone` | Numeric | Count | Number of days in which ozone was the primary pollutant | Pollutant-specific measure |
| `Days PM2.5` | Numeric | Count | Number of days in which PM2.5 was the primary pollutant | Pollutant-specific measure |
| `Days PM10` | Numeric | Count | Number of days in which PM10 was the primary pollutant | Pollutant-specific measure |

## Derived Air Pollution Exposure Variables

| Variable | Type | Coding / Values | Description | Role |
|---|---|---|---|---|
| `Annual_Weighted_AQI` | Continuous | Approx. 0–500 | Weighted annual AQI calculated using representative AQI category midpoint values | Primary continuous exposure |
| `prop_unhealthy` | Continuous | 0–1 | Proportion of AQI days classified as Unhealthy for Sensitive Groups or worse | Supporting continuous exposure |
| `AQI_Category` | Categorical (ordinal) | Low, Moderate, High | Exposure category based on tertiles of `prop_unhealthy` | Primary categorical exposure |
| `Pollutant_Category` | Categorical (nominal) | CO, NO2, Ozone, PM2.5, PM10, Mixed | Dominant pollutant based on the pollutant with the greatest number of AQI days; ties classified as Mixed | Exploratory exposure |

## Asthma Variables

| Variable | Type | Coding / Values | Description | Role |
|---|---|---|---|---|
| `Asthma_ED_Rate` | Continuous | Rate per 10,000 | Age-adjusted asthma emergency department visit rate | Supporting outcome measure |
| `Sex` | Categorical (nominal) | Male, Female | Sex group associated with the asthma ED rate | Used in outcome construction |
| `Asthma_ED_Status` | Categorical (binary) | Not Elevated, Elevated | Indicates whether the asthma ED visit rate exceeds the sex-specific benchmark | Primary outcome |

## Derived Variable Construction

### Annual Weighted AQI

`Annual_Weighted_AQI` summarizes annual air-quality exposure using representative midpoint values for each AQI category:

- Good = 25
- Moderate = 75
- Unhealthy for Sensitive Groups = 125
- Unhealthy = 175
- Very Unhealthy = 250
- Hazardous = 400

The midpoint values are weighted by the number of days observed in each AQI category and divided by the total number of days with AQI data.

Higher values indicate worse overall annual air quality.

### Proportion of Unhealthy Days

`prop_unhealthy` represents the proportion of monitored AQI days classified as Unhealthy for Sensitive Groups or worse.

It includes:

- Unhealthy for Sensitive Groups Days
- Unhealthy Days
- Very Unhealthy Days
- Hazardous Days

Higher values indicate more frequent exposure to harmful air-quality conditions.

### AQI Category

`AQI_Category` was derived from the empirical distribution of `prop_unhealthy`.

Categories were defined using the 33rd and 66th percentiles:

- **Low** — lower third of the distribution
- **Moderate** — middle third of the distribution
- **High** — upper third of the distribution

Higher categories indicate a greater frequency of unhealthy air-quality days.

### Pollutant Category

`Pollutant_Category` identifies the pollutant associated with the greatest number of AQI days within each county-year.

Possible categories are:

- CO
- NO2
- Ozone
- PM2.5
- PM10
- Mixed

`Mixed` is assigned when more than one pollutant is tied for the greatest number of AQI days.

### Asthma ED Status

`Asthma_ED_Status` is the primary binary outcome.

Sex-specific thresholds were used:

- **Male:** Elevated if `Asthma_ED_Rate > 27.1` per 10,000
- **Female:** Elevated if `Asthma_ED_Rate > 32.3` per 10,000

The two categories are:

- **Not Elevated**
- **Elevated**

## Primary Analytical Roles

| Variable | Analytical Role |
|---|---|
| `Asthma_ED_Status` | Primary outcome |
| `Annual_Weighted_AQI` | Primary continuous exposure |
| `AQI_Category` | Primary categorical exposure |
| `prop_unhealthy` | Supporting continuous exposure |
| `Pollutant_Category` | Exploratory exposure |
| `State` | Geographic adjustment variable |
| `County` | Geographic identifier |
| `Sex` | Used in construction of the primary outcome |
