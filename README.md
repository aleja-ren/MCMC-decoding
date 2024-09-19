# MCMC-decoding
This project implements a decoding algorithm using MCMC (Markov Chain Monte Carlo) methods in R. The approach leverages probabilistic sampling to estimate hidden states in a sequence, commonly used in applications like hidden Markov models and Bayesian inference. The code includes data preprocessing, model setup, and result.

# About MCMC-decoding
![Wikipedia](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo)

# Explanation of functions in the code
## Score 1
The score 1 is given by the length of the word. This function will have as input a vector of words, for each one we calculate its length and if it has a length between 1 and 23 (the last length we have in the given file) we calculate the logarithm of the frequency for that length, if it is not in the document we will assign the value $log(10^{-8})$.

## Score 2
Score 2 is given by the product of the observed frequencies of the bigraphs in the word. Like score 1, this function will have as input a vector of words, with the function given in the document we calculate the bigrams of each word, only if it has a length strictly greater than 1 (if it is of length less than 2 we cannot calculate the bigrams). Once calculated the bigrams, we will have them all stored in a vector, we look for each one if it is in the document of the bigrams and if it is, we calculate the logarithm of its frequency, if not, as with the score 1 we return the value $log(10^{-8})$.

## Score 3
The score 3 is given by the product of the observed frequencies of the trigrams in the word. Like the previous scores, this function will have as input a vector of words, with the function given in the document we calculate the trigrams of each word, only if it has a length strictly greater than 2 (if it is of length less than 3 we cannot calculate the trigrams). Once calculated the trigrams, we will have them all stored in a vector, we look for each one if it is in the document of the trigrams and if it is, we calculate the logarithm of its frequency, if not, as with the previous scores, we return the value $log(10^{-8})$.

## Score
The function that defines the score is the following: $score=score1^3∗score2∗score3^{0.5}$, as we are operating with logarithms, applying the properties of logarithms we will have the following function: $score=3∗score1+score2+0.5∗score3$.
# Conclusions

The code seeks to maximize the score based on the results of scores 1, 2 and 3, it does not mean that we will always obtain the best result with the real decoding.
As we can see in the output example of the algorithm, the highest score is obtained with this text:

#### ciphered text
HTSVBPX QTIVBHRXIXBSX XLNTBVBGVBGREX HVSBSXBPXIQVYRXIHTBSXBLTILRXILRVBCBSXB XGRJRTIBXQHXBSX XLNTBRILGACXBGVBGREX HVSBSXBLVYERV BSXB XGRJRTIBTBSXBL XXILRVBVQRBLTYTBGVBGREX HVSBSXBYVIRWXQHV BQAB XGRJRTIBTBQABL XXILRVBRISRZRSAVGBCBLTGXLHRZVYXIHXBHVIHTBXIBPAEGRLTBLTYTBXIBP RZVSTBPT BGVBXIQXMVIDVBGVBP VLHRLVBXGBLAGHTBCBGVBTEQX ZVILRVBHTSTBRISRZRSATBHRXIXBSX XLNTBVBGVBGREX HVSBSXBTPRIRTIBCBSXBXKP XQRTIBXQHXBSX XLNTBRILGACXBXGBSXBITBQX BYTGXQHVSTBVBLVAQVBSXBQAQBTPRIRTIXQBXGBSXBRIZXQHRJV BCB XLRER BRIWT YVLRTIXQBCBTPRIRTIXQBCBXGBSXBSRWAISR GVQBQRIBGRYRHVLRTIBSXBW TIHX VQBPT BLAVGUARX BYXSRTBSXBXKP XQRTI

#### result after 2100 iterations
TODA PERSONA TIENE DERECHO A LA LIBERTAD DE PENSAMIENTO DE CONCIENCIA Ñ DE RELIGION ESTE DERECHO INCLUÑE LA LIBERTAD DE CAMBIAR DE RELIGION O DE CREENCIA ASI COMO LA LIBERTAD DE MANIFESTAR SU RELIGION O SU CREENCIA INDIVIDUAL Ñ COLECTIVAMENTE TANTO EN PUBLICO COMO EN PRIVADO POR LA ENSEJANKA LA PRACTICA EL CULTO Ñ LA OBSERVANCIA TODO INDIVIDUO TIENE DERECHO A LA LIBERTAD DE OPINION Ñ DE EYPRESION ESTE DERECHO INCLUÑE EL DE NO SER MOLESTADO A CAUSA DE SUS OPINIONES EL DE INVESTIGAR Ñ RECIBIR INFORMACIONES Ñ OPINIONES Ñ EL DE DIFUNDIRLAS SIN LIMITACION DE FRONTERAS POR CUALQUIER MEDIO DE EYPRESION <br>

We see that the text can be understood and we can interpret it but we do not have the actual decoding.
The algorithm as I have put it is somewhat inefficient, having nested loops and conditions within them. We could improve it by using data structures like a hash table, so that we don't have to calculate the score constantly and we can access a hash table to use it if it has already been calculated previously.
The algorithm avoids that we have to use brute force to decode the text, we want it to stay “close” to the most probable states, i.e. the ones with the highest score and avoid going down a path that leads to a low score, looking only at the previous state.
To improve the decoding results of this algorithm, we could add another score that with a dictionary returns a score if the word exists and penalizes if the proposed word does not exist in the dictionary.
