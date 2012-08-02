package report

import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.Writer
import java.io.Reader
import model.Classification
import model.TruePositive
import model.FalsePositive
import model.FalseNegative
import model.TrueNegative
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.FileSystems

class ReportGenerator {
    
    def generateReport(classificationResultsFile: File, targetFolder: Path, indexFile: File, categories: List[Category]) {
        val resultsReader = new BufferedReader(new FileReader(classificationResultsFile))
        
        def readClassifications() {
            val line = resultsReader.readLine
            
            if(line != null) {
                val classification = Classification(line)
                
                val category = categories.find(_.predicate(classification)).getOrElse(throw new RuntimeException())
                category.write(classification)
                
                readClassifications
            }
        }
        
        readClassifications
        
        for(category <- categories) {
            category.close
        }
        
        
        val indexWriter = new BufferedWriter(new FileWriter(indexFile))
        indexWriter.write("""
<html>
    <head>
        <title>Report</title>
    </head>
    <body>
        <h1>Report</h1><br>
""")
        
        val total = categories.map(_.num).reduceLeft(_ + _)

        for(category <- categories) {
            indexWriter.write(
                "<a href=\"report_data/" + category.filenamePrefix + ".html\">" + category.metaTitle + " (" + category.num + "; " + math.round((category.num * 10000.0 / total))/100 + "%)<br>\n"
            )
        }
        
        indexWriter.write("""
    </body>
</html>""")
        
        indexWriter.close()
    }
    
}






