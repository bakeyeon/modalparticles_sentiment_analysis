# How Much Sentiment Can Be Carried by Modal Particles?

This research was developed for the MA course "Deep Learning in NLP" (SS24) and was conducted under the supervision and evaluation of Dr. Yulia Zinova. This study serves as a technical extension of my previous research on modal particles (detailed in Appendix A). I appreciate your understanding regarding its preliminary nature and welcome any inquiries or academic discussions related to this work.

- Author: Kai H. Park (Ms)
- Supervised by: Dr. Yulia Zinova

## Abstract
 This paper explores the emotional tendencies of German modal particles based on real-world sentence data. Building on a previous study that compared modal particles with expressive emojis through a survey, this research uses sentiment analysis to calculate average emotional values for selected particles (ja, doch, schon, etc.). Three models were tested to compare their performance. The goal of this study is to quantify the sentiment carried by German modal particles and examine to what extent they encode affective meaning. By comparing three sentiment analysis models(FastText, Hugging Face pipeline, and BERT), this research investigates whether similar sentimental patterns are detected across models, which model responds most sensitively to subtle sentiment shifts, and which model’s output aligns most closely with human evaluation from a previous study. The findings are intended to inform future strategies for mapping sentiment-driven features in multilingual translation contexts.


 ## Limitations
- Subjectivity of Survey Data: As the study relies on surveys of proficient language users, the data is inherently subject to individual interpretation and personal perception of emotional nuances.
- Small Sample Size and Robustness: With $N=35$ for Survey 1 and $N=112$ for Survey 2 (previous study), the relatively small sample sizes may limit the statistical robustness and the generalizability of the findings to a broader population.
- Cross-Cultural and Linguistic Variables: Emotional perception is deeply tied to cultural background and language proficiency. These factors influence how both modal particles and emojis are interpreted, potentially limiting the universal applicability of the results.
- Contextual Ambiguity and Polysemy: Certain particles, such as ja or schon, can convey opposing sentiments (e.g., friendliness vs. annoyance) depending on the context. Assigning a fixed sentiment score to these particles involves a degree of arbitrary simplification of their pragmatic complexity.
- Syntactic Position and Pragmatic Variance: While it is standard linguistic practice to account for subtle shifts in meaning based on the exact position of an element within a sentence, this study did not strictly control for syntactic variables. Consequently, the findings may have inherent limitations in capturing the full range of pragmatic nuances associated with word order variations.
- Sample Size and Data Scaling: The study is limited by a relatively small sample size (N ≈ 100). Additionally, human sentiment scores were manually scaled to facilitate a clearer comparison with the models' output ranges. While this adjustment might affect the absolute numerical precision, the primary focus of this research was to observe the relative trends and emotional trajectories. Since the relative rankings and directions of sentiment shifts remain consistent, the comparative analysis between human intuition and AI models remains valid and insightful.


## Related Study & Projects 
Modalpartikeln': Can Sentiments Survive Translation with Emojis?
[https://github.com/bakeyeon/modalparticles_sentiment_analysis](https://github.com/bakeyeon/Modalparticles_Emojis)

Sentiment Translator
https://github.com/bakeyeon/sentiment_translator

Medium Post: Can We Translate Our Sentiments? 
https://medium.com/gopenai/can-we-translate-our-sentiments-140c55822ead?sharedUserId=kaiwritesornot



## Acknowledgments
I would like to thank Prof. Jacopo Romoli and Dr. Yulia Zinova for providing the insightful lectures and the academic environment that inspired this research. Their courses were instrumental in shaping the foundational ideas. 
