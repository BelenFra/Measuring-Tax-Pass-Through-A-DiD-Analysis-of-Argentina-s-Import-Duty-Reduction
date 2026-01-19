# Measuring-Tax-Pass-Through-A-DiD-Analysis-of-Argentina-s-Import-Duty-Reduction
Analyzed the -2.11% price impact of Argentina’s tax reduction using DiD in R and Python. I moved beyond "fancy charts" to isolate causal effects, providing data-driven insights for strategic retail decisions
# 1. Project Overview
In September 2024, the Argentine government reduced the Impuesto PAIS on imported goods from %60 to 30%. This project investigates the causal effect of this fiscal policy on consumer prices within the Food & Beverage sector. Using a Difference-in-Differences (DiD) approach, I analyzed whether this tax cut actually reached the end consumer or was absorbed by other links in the supply chain.
# 2. The Business Problem
Primary Question: Did imported products see a significantly higher price reduction compared to domestic products after the tax cut?

* *Granularity*: Was the impact uniform across different product categories?
* *Reliability*: Are the observed changes truly causal, or are they artifacts of market trends?

# 3. Dataset & Methodology
* *Data Source*: This project utilizes a dataset originally curated and hosted by [Unidad de Impacto](https://github.com/Unidad-de-Impacto) in the following GitHub repository: [Impuesto_PAIS_2024
](https://github.com/Unidad-de-Impacto/Impuesto_PAIS_2024/tree/main/Archivos). I am grateful to the contributors for making this data available for analysis.

* *Panel Structure*: 26 product categories (13 imported, 13 national) tracked over 5 weeks (4 days pre-event, 37 days post-event).

* *Strategy*: Difference-in-Differences (DiD).

* *Treatment Group*: Imported products affected by the tax.

* *Control Group*: National products (unaffected).

* *Tools*: Python (EDA and Data Cleaning) and R (Fixed Effects Linear Models and Robustness Tests).

# 4. Key Findings
The analysis reveals a significant Tax Pass-Through:

* *Average Impact*: On average, prices for imported products dropped by 2.11% more than national products following the policy change.
* *Heterogeneity*: The impact was not uniform. While some categories showed sharp declines, others remained stagnant, suggesting that other reasons, such as market competition and inventory turnover, play a critical role in price adjustments.

# 5. Robustness & Validation
To move beyond "fancy charts" and ensure the results were not coincidental, I performed several rigorous tests:

* *Parallel Trends Validation*: Confirmed that both groups shared a common price trajectory before the intervention.
* *Placebo Date Test*: Simulated the tax cut on a fake date; the model found no significant effect, confirming the timing of our original result.
* *Placebo Group Test*: Assigned the "imported" label to national products at random; no effect was found, proving the impact was specific to the treated group.

#  6. Conclusion
This study provides empirical evidence that fiscal policy can influence retail prices even in high-inflation environments. However, the pass-through rate depends heavily on product category and supply conditions
