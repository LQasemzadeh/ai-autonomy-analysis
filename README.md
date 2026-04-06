# AI Execution Autonomy Analysis

This project investigates how different levels of AI execution autonomy (Manual, Assistance, Execution) affect user performance and intervention behavior in a controlled task environment.

## Research Focus
- How does AI autonomy impact task completion time and error rates?
- How does autonomy influence user intervention behavior?

## Methodology
- Experimental setup with three autonomy conditions
- Interaction logging via a custom-built system
- Data analysis in R

## Analysis
- Non-parametric statistical tests:
  - Kruskal-Wallis test
  - Pairwise Wilcoxon test
- Metrics:
  - Completion Time
  - Error Rate
  - Intervention Behavior

## Tech Stack
- R (data analysis)
- Quarto (report generation)
- Supabase (logging backend)
- Next.js (prototype interface)

## Reproducibility
The full analysis can be reproduced by rendering the Quarto document:

```r
quarto render ai-execution-autonomy-analysis.qmd