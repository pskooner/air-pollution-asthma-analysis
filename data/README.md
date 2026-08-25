# Data

## Air Quality Data

Source: U.S. Environmental Protection Agency

County-level annual Air Quality Index (AQI) data for 2023 were used to characterize air-pollution exposure.

The AQI dataset was used to derive several analytical variables, including annual weighted AQI, the proportion of unhealthy AQI days, AQI exposure categories, and the dominant pollutant category.

## Asthma Emergency Department Data

Source: Centers for Disease Control and Prevention

County-level age-adjusted asthma emergency department visit rates per 10,000 population were used as the health outcome data.

The asthma data were stratified by sex.

A binary asthma emergency department status variable was created using sex-specific thresholds:

- Male: Elevated if Asthma ED visit rate > 27.1 per 10,000 population
- Female: Elevated if Asthma ED visit rate > 32.3 per 10,000 population

## Analytical Dataset

The air-quality and asthma datasets were matched using:

- State
- County
- Year

Only observations with matching geographic and year information in both datasets were retained in the analytical dataset.

## Main Analytical Variables

| Variable | Role |
|---|---|
| `Asthma_ED_Status` | Outcome |
| `Annual_Weighted_AQI` | Primary continuous exposure |
| `AQI_Category` | Primary categorical exposure |
| `prop_unhealthy` | Supporting continuous exposure |
| `Pollutant_Category` | Exploratory exposure |
| `State` | Geographic adjustment variable |

## Variable Definitions

### `Asthma_ED_Status`

Binary indicator of whether the county-level asthma emergency department visit rate was elevated based on sex-specific thresholds.

Categories:

- Not Elevated
- Elevated

### `Annual_Weighted_AQI`

Continuous measure of annual air-pollution exposure calculated by weighting the number of days in each AQI category using representative AQI values.

### `AQI_Category`

Categorical exposure based on the distribution of the proportion of unhealthy AQI days.

Categories:

- Low
- Moderate
- High

### `prop_unhealthy`

Proportion of monitored AQI days classified as:

- Unhealthy for Sensitive Groups
- Unhealthy
- Very Unhealthy
- Hazardous

### `Pollutant_Category`

Dominant pollutant category based on the pollutant responsible for the greatest number of AQI days.

Categories observed in the analytical dataset included:

- Ozone
- PM2.5
- PM10

### `State`

State-level geographic variable used for stratification and adjustment in regression analyses.
