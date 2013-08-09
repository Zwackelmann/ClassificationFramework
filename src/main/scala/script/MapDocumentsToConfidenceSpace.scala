package script

import parser.History
import filter.CategorySelectionFilter
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import classifier.CategoryIsMsc
import classifier.BalancedTrainSetSelection
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance

object MapDocumentsToConfidenceSpace {
    def main(args: Array[String]) {
        common.Path.rootFolder = "data_try_train_set_selection"
        val toolset = AllClassesForDocument(0)
        
        val corpus = ArffJsonInstancesSource(common.Path.rootFolder + "/arffJson/corpus.json")
        
        val numAttributes = if(corpus.iterator.hasNext) // little hack to find out the number of resulting attributes 
            Some(toolset.categoryCertainies(corpus.iterator.next).size)
        else None
        
        var count = 0
        val mappedDocuments = for(inst <- corpus) yield {
            if(count % 100 == 0) println(count)
            count += 1
            
            val results = toolset.categoryCertainies(inst)
            val data = results.sortBy(_._1).map(_._2)
            
            ArffJsonInstance(inst.id, inst.categories, data, false)
        }
        
        val confidenceSpaceCorpus = numAttributes map { numAttributes => {
            ArffJsonInstancesSource(
                mappedDocuments,
                ArffJsonHeader(numAttributes)
            )
        }}
        
        confidenceSpaceCorpus map { c => 
            c.save("confidenceSpaceCorpus.json")
        }
    }
}