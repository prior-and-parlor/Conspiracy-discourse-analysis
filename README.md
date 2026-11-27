# Conspiracy Discourse Analysis

This repository contains the research code, analytical workflows, and associated outputs accompanying a manuscript submitted for peer review.

## Repository Contents

1. **Topic Modelling:** Code for LDA (200 topics).
2. **LDAvis Visualisation:** Code for interactive visualisation of the 200-topic LDA model (Sievert & Shirley, 2014).
   - **Inspect topic quality:** Examine whether the words associated with each topic form a coherent combination.
   - **Identify overlapping topics:** Determine whether some topics contain similar vocabularies.
   - **Interpret topics:** Examine the most relevant and distinctive terms before assigning descriptive topic labels.
   - **Assess topic differentiation:** Identify topics that are relatively distinct from the rest of the corpus.
   - **Support topic interpretation:** Use the interactive term rankings alongside the existing topic keyword and frequency analyses.
3. **COVID-19 Topic Analysis:** OLS regression models examining COVID-19-related topics, including variable transformations.
4. **Political Dictionary Analyses:** 4 Lexicon-based political dictionary analyses (Puschmann & Haim, n.d.; original source: https://github.com/MarHai/content-analysis-with-r.com/blob/master/4-dictionaries.Rmd).
5. **Shannon Entropy, Segmented Regression, MannKendall and Sens slope:** 3 political dictionaries (i.e., policy_agendas_dict, lavergarry.lexicon & maerz.lexicon)
6. **Extended Moral Foundations Dictionary Analysis:** Lexicon-based psycholinguistic analysis (Hopp et al., 2021; original source: https://github.com/medianeuroscience/emfd).
7. **Sensitivity and Stability Checks for morality polarity:** Time-series regression of moral polarity (eMFD vice minus virtue) across 18 months, with HC3/Newey-West errors, wild bootstrap, placebo breakpoints, TOST equivalence tests, Bayes factors, and Sen's slope.
8. **LIWC-22 Pre-COVID vs COVID Comparison:** Mann-Whitney U tests on monthly mean
   LIWC-22 scores (20 variables), with effect sizes and BH correction.

## References

   - Hopp, F. R., Fisher, J. T., Cornell, D., Huskey, R., & Weber, R. (2021). The extended Moral Foundations Dictionary (eMFD): Development and applications of a crowd-sourced approach to extracting moral intuitions from text. *Behavior Research Methods, 53*(1), 232–246. https://doi.org/10.3758/s13428-020-01433-0
   - medianeuroscience. (n.d.). *emfd: The Extended Moral Foundations Dictionary (eMFD)* [Computer software]. GitHub. Retrieved November 26, 2026, from https://github.com/medianeuroscience/emfd
   - Puschmann, C., & Haim, M. (n.d.). *Topic-specific dictionaries*. In *Automated content analysis with R*. https://content-analysis-with-r.com/4-dictionaries.html
   - Sievert, C., & Shirley, K. E. (2014). LDAvis: A method for visualizing and interpreting topics. In *Proceedings of the Workshop on Interactive Language Learning, Visualization, and Interfaces* (pp. 63–70). Association for Computational Linguistics. https://doi.org/10.3115/v1/W14-3110

## Reproducibility

Code and analytical outputs are provided to support methodological transparency, independent assessment, and reproducibility in a large conspiracy corpus of >1.02 million comments in a conspiracy theory forum.
