# Conspiracy Discourse Analysis

This repository contains the research code, analytical workflows, and associated outputs accompanying a manuscript submitted for peer review.

## Repository Contents

1. **Topic Modelling:** Code for LDA (200 topics).
2. **LDAvis Visualisation:** Code for interactive visualisation of the 200-topic LDA model.
   - **Inspect topic quality:** Examine whether the words associated with each topic form a coherent combination.
   - **Identify overlapping topics:** Determine whether some topics contain similar vocabularies.
   - **Interpret topics:** Examine the most relevant and distinctive terms before assigning descriptive topic labels.
   - **Assess topic differentiation:** Identify topics that are relatively distinct from the rest of the corpus.
   - **Support topic interpretation:** Use the interactive term rankings alongside the existing topic keyword and frequency analyses.
3. **COVID-19 Topic Analysis:** OLS regression models examining COVID-19-related topics, including variable transformations.
4. **Political Dictionary Analyses:** 4 Lexicon-based political dictionary analyses (original source: https://github.com/MarHai/content-analysis-with-r.com/blob/master/4-dictionaries.Rmd)
5. **Extended Moral Foundations Dictionary Analysis:** Lexicon-based psycholinguistic analysis (original source: https://github.com/medianeuroscience/emfd)
6. **Robustness Checks for morality polarity:** Additional analyses assessing the stability and sensitivity of the findings.
## Reproducibility

Code and analytical outputs are provided to support methodological transparency, independent assessment, and reproducibility in a large conspiracy corpus of > 1.02 million comments in a conspiracy theory forum.
