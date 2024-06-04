# Word Sense Discrimination in French Transformer Models
This repository contains the research code used for my master thesis.


The notebooks are labelled for each different experiment.
Experiment 1.1 and 1.2 concern the clustering of contextualized embeddings, using an approach inspired by Yamada et al.'s (2021) method for semantic frame induction.
Experiment 2.1 and 2.2 implement a variation upon Amrami & Goldberg's (2018) method of dynamic-pattern-based substitute clustering.
Experiment 3.1 and 3.2 implement Zhou et al.'s (2019) approach to lexical substitution to obtain higher-quality substitute-based clustering.

All clustering experiments apply hard-clustering to the data points.
The experiments concern two datasets obtained from the ELEXIS FrTenTen (2020) corpus, which cannot be disclosed in this repository due to copyright reasons.
Experiments 1.1, 2.1  and 3.1 perform word sense discrimination upon a curated corpus, for which manually labelled sense annotations were provided as a gold standard.
Experiments 2.1, 2.2 and 3.2 perform the same methods upon a larger corpus, using the same gold standard for evaluation.


Sources:
Amrami, A., & Goldberg, Y. (2018). 
  Word Sense Induction with Neural biLM and Symmetric Patterns. In E. Riloff, D. Chiang, J. Hockenmaier, & J. Tsujii (Eds.), 
  Proceedings of the 2018 Conference on Empirical Methods in Natural Language Processing (pp. 4860–4867). Association for Computational Linguistics. 
  https://doi.org/10.18653/v1/D18-1523
Yamada, K., Sasano, R., & Takeda, K. (2021). 
  Verb Sense Clustering using Contextualized Word Representations for Semantic Frame Induction. 
  Findings of the Association for Computational Linguistics: ACL-IJCNLP 2021, 4353–4362. 
  https://doi.org/10.18653/v1/2021.findings-acl.381
Zhou, W., Ge, T., Xu, K., Wei, F., & Zhou, M. (2019). 
  BERT-based Lexical Substitution. 
  Proceedings of the 57th Annual Meeting of the Association for Computational Linguistics, 3368–3373. 
  https://doi.org/10.18653/v1/P19-1328
