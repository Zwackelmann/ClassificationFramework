package script
import filter.VectorFromDictFilter.AdvancedTokenizer
import parser.ArffJsonInstancesSource
import script.formulas.Snuggle
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

object ExtractFormulasFromAbstracts {
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource("data_try_train_set_selection/arffJson/corpus.json")
        
        val formulas = corpus.iterator
            .map(inst => AdvancedTokenizer.tokenize(inst.dataAt(1).asInstanceOf[String])
            .filter(t => t.isInstanceOf[AdvancedTokenizer.Formula]))
            .flatten
        
        val documentWriter = new BufferedWriter(new FileWriter(new File("formula_to_cmathml")))
        
        var i = 0
        for(formula <- formulas) {
            if(i % 100 == 0) println(i)
            documentWriter.write("[" + formula.toString + ", " + (try{Snuggle.receiveContentMathML(formula.toString)} catch { case _: Throwable => None }) + "]\n")
            i += 1
        }
        
        documentWriter.close
    }
}