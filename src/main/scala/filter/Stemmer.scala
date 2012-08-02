package filter

import org.tartarus.snowball.ext.{PorterStemmer => ExternalPorterStemmer} 

trait Stemmer {
    def stem(word: String): String
}

class PorterStemmer extends Stemmer {
    val stemmer = new ExternalPorterStemmer
        
    def stem(word: String) = {
        stemmer.setCurrent(word)
        stemmer.stem
        stemmer.getCurrent
    }
}

class NullStemmer extends Stemmer {
    def stem(word: String) = word
}