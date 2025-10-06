-Study of Songbird Visitation at a Feeder near Almsee-

Across the EU, ~600 million songbirds were lost between 1980–2017 (Burns et al., 2021). 
Tits (Paridae)—notably Great, Blue, and Marsh tits—are widespread, easily identified residents and useful ecological indicators (Kajanus et al., 2022). 
This study monitored feeder visitation across time-of-day in a human-inhabited setting at Almsee to identify preferred feeding periods and align feeding with birds’ daily energetic cycles. 
We hypothesized peak visitation at sunrise and sunset, with a midday lull. Using both continuous and scan sampling, continuous sampling showed no significant differences in visit numbers by time-of-day, 
whereas scan sampling indicated significant time-of-day effects (by p-values). 
Given scan sampling’s inherent limitations (snapshot bias, interval dependence), the robustness of those p-values is uncertain, so evidence for strong diel peaks is mixed and method-dependent.

Research question & data layout
	•	Goal: Test whether time-of-day (Tagesabschnitt: Früh, Mittag, Nachmittag) affects counts (species-specific *_scan, *_number, and All_Individuen_*).
	•	Design: Each observation is mapped to a time-of-day and a specific clock time (Uhrzeit). Clock time is used for coloring (exploratory layering), not as a factor in the test.
	•	Outcomes: Integer counts—often non-normal, heteroscedastic, and zero-inflated—so a nonparametric primary test is appropriate.

Preprocessing & plotting
	•	Factor levels for Tagesabschnitt and Uhrzeit are explicitly fixed to ensure reproducible ordering and stable plots.
	•	Plots:
	◦	geom_boxplot (+ optional Uhrzeit color) to visualize distribution, median, spread, outliers.
	◦	ggpubr::ggboxplot + stat_compare_means(method = "kruskal.test") to annotate p-values directly on panels.
	◦	Panels are organized by species and by continuous vs scan sampling for side-by-side comparison.

Why Kruskal–Wallis?
	•	Count data are prone to non-normality and heteroscedasticity; ANOVA can be sensitive to these violations.
	•	Kruskal–Wallis tests differences in central tendency nonparametrically and is robust to non-normality.
	•	Assumptions:
	1	Independence of groups/observations
	2	At least ordinal measurement
	3	Similar distributional shapes across groups for clean interpretation
	•	If the omnibus test is significant, follow with Dunn post hoc with multiple-testing correction (e.g., BH/FDR).

Normality & variance checks (good practice to report)
	•	Even with nonparametric inference, diagnostics support the choice:
	◦	Group-wise or residual normality (Shapiro–Wilk, QQ-plots)
	◦	Homoscedasticity (Levene/Brown–Forsythe)
	◦	Strong skewness or floor/ceiling effects motivate the nonparametric approach.

Interpretation
	•	Boxplots summarize median and spread; outliers and shape guide ecological interpretation.
	•	Kruskal–Wallis p indicates whether time-of-day affects counts; pairwise differences require post hoc tests.
	•	Comparing scan vs continuous panels reveals measurement-mode consistency or divergence.

Caveats & extensions
	•	If observations are not independent (e.g., repeated measures per site or individual), Kruskal–Wallis assumptions are strained:
	◦	Prefer GLMMs with Poisson/negative binomial, fixed effect = time-of-day, random effects = site/individual/date.
	◦	Treat Uhrzeit as a continuous covariate (e.g., GAM with s(Uhrzeit)) to estimate diurnal curves.
	•	Report an effect size: ε² for Kruskal–Wallis (or η² under parametric assumptions).
