# How Much Sentiment Can Be Carried by Modal Particles?

This is a term paper for the MA program in SS24, titled "Deep Learning in NLP" at Heinrich-Heine University. This study is based on amy previous paper about modal particles(in Appendix A). I appreciate your understanding regarding its preliminary nature. I welcome any inquiries or discussions related to this research.

Author: Kai H. Park (Ms)

Abstract:
 This paper explores the emotional tendencies of German modal particles based on real-world sentence data. Building on a previous study that compared modal particles with expressive emojis through a survey, this research uses sentiment analysis to calculate average emotional values for selected particles (ja, doch, schon, etc.). Three models were tested to compare their performance. The goal of this study is to quantify the sentiment carried by German modal particles and examine to what extent they encode affective meaning. By comparing three sentiment analysis models(FastText, Hugging Face pipeline, and BERT), this research investigates whether similar sentimental patterns are detected across models, which model responds most sensitively to subtle sentiment shifts, and which model’s output aligns most closely with human evaluation from a previous study. The findings are intended to inform future strategies for mapping sentiment-driven features in multilingual translation contexts.


 ## Limitations
- Subjectivity of Survey Data: As the study relies on surveys of proficient language users, the data is inherently subject to individual interpretation and personal perception of emotional nuances.
- Small Sample Size and Robustness: With $N=35$ for Survey 1 and $N=112$ for Survey 2, the relatively small sample sizes may limit the statistical robustness and the generalizability of the findings to a broader population.
- Cross-Cultural and Linguistic Variables: Emotional perception is deeply tied to cultural background and language proficiency. These factors influence how both modal particles and emojis are interpreted, potentially limiting the universal applicability of the results.
- Contextual Ambiguity and Polysemy: Certain particles, such as ja or schon, can convey opposing sentiments (e.g., friendliness vs. annoyance) depending on the context. Assigning a fixed sentiment score to these particles involves a degree of arbitrary simplification of their pragmatic complexity.
- Syntactic Position and Pragmatic Variance: While it is standard linguistic practice to account for subtle shifts in meaning based on the exact position of an element within a sentence, this study did not strictly control for syntactic variables. Consequently, the findings may have inherent limitations in capturing the full range of pragmatic nuances associated with word order variations.

## Related Study & Projects 
Modalpartikeln': Can Sentiments Survive Translation with Emojis?
[https://github.com/bakeyeon/modalparticles_sentiment_analysis](https://github.com/bakeyeon/Modalparticles_Emojis)

Sentiment Translator
https://github.com/bakeyeon/sentiment_translator
