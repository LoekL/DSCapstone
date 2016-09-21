

Word Prediction Using N-Grams and a Backoff Model
========================================================
author: Lukas Landzaat
date: 2016-09-21
autosize: true

## JHU - Data Science Specialisation
### Capstone Project

Overview
========================================================

1. [N-Grams][3] used in the model were created using the data provided to us by [SwiftKey][1]. It comprised a number of blogs, news articles & tweets.
2. The [tm package][2] was used to manage the corpora, and apply several transformations to it before generating the ngrams (lowercase, removing punctuation, etc.).
3. N-Grams up to the fifth order were included (5-Grams).
4. A "Backoff" model has been used to retrieve predicitons, in particular the "[Stupid Backoff][4]" model.

[1]: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
[2]: https://cran.r-project.org/web/packages/tm/index.html
[3]: https://en.wikipedia.org/wiki/N-gram
[4]: https://web.stanford.edu/~jurafsky/slp3/4.pdf

<small>
Example - Backoff Model
========================================================

1. You want to predict the next word in the sequence "*hello how are you*".
2. This 4-Gram occurred 100 times in the corpus.
3. You search for possible 5-Grams that start with the same 4-Gram "*hello how are you*".
4. Say you find the 5-Gram "*hello how are you* **doing**", which occurred 20 times in your text corpus.
5. The final score for the word '**doing**' will be **20/100 = 0.2**.
6. However, perhaps we also find the 4-Gram "*how are you* **today**" which occurred 100 times, whereas "*how are you*" 200 times.
7. Because a 4-Gram is a lower order N-Gram than a 5-Gram, we *backoff* one order. Therefore we penalize the score with Lambda, which is usually set to 0.4.
8. Finally, the score for 'today' will be **0.4 * (100 / 200) = 0.2** as well, tied with 'doing'.

</small>

Shiny Application: WordPredictor
========================================================

#### Application
[WordPredictor][1] implements the previously described model in a data product.

#### Model Usage

1. Open the application.
2. Type in one or more words.
3. Hit the 'Go!' button.
4. Look at the top 3 predictions on the right.

[1]: https://loekl.shinyapps.io/DataScienceCapstone/

<small>
References
========================================================


- Ferriere, P. (2016). Word Prediction Using Stupid Backoff With a 5-gram Language Model. Available: [https://rpubs.com/pferriere/dscapreport](https://rpubs.com/pferriere/dscapreport). Last accessed 2016-09-21.
- “Speech and Language Processing”, by D. Jurafsky & al, Chapter 4, Draft of January 9, 2015 @ https://web.stanford.edu/~jurafsky/slp3/.
- [JHU DS Capstone Swiftkey Dataset](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
- Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English (COCA). Downloaded from http://www.ngrams.info on September 18, 2016.
- FEINERER, Ingo; HORNIK, Kurt; MEYER, David. Text Mining Infrastructure in R. Journal of Statistical Software, [S.l.], v. 25, Issue 5, p. 1 - 54, mar. 2008. ISSN 1548-7660. Available at: <https://www.jstatsoft.org/index.php/jss/article/view/v025i05>. Date accessed: 21 sep. 2016. doi:http://dx.doi.org/10.18637/jss.v025.i05.
- All code for this assignment can be found on GitHub: https://github.com/LoekL/DSCapstone.

</small>
