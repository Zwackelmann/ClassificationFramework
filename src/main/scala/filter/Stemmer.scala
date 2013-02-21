package filter

import org.tartarus.snowball.ext.{PorterStemmer => ExternalPorterStemmer} 

trait Stemmer {
    def stem(word: String): String
}

class PorterStemmer extends Stemmer {
    def stem(word: String) = {
        val stemmer = new ExternalPorterStemmer
        
        stemmer.setCurrent(word)
        stemmer.stem
        stemmer.getCurrent
    }
}

class NullStemmer extends Stemmer {
    def stem(word: String) = word
}