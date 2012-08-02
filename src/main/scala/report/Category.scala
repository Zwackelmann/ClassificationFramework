package report

import model._
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import java.nio.file.Path

object Category {
    def simpleCategories(reportDataFolder: Path, certaintyThreshold: Double) = List(
        new Category(
            reportDataFolder,
            "true_positives",
            ((classification: Classification) => classification match {
                case tp: TruePositive if tp.certainty >= certaintyThreshold => true
                case _ => false
            }),
            "True positives",
            "List of true positives"
        ),
        new Category(
            reportDataFolder,
            "false_positives", 
            ((classification: Classification) => classification match {
                case fp: FalsePositive if fp.certainty >= certaintyThreshold => true
                case _ => false
            }),
            "False positives",
            "List of false positives"
        ),
        new Category(
            reportDataFolder,
            "true_negatives", 
            ((classification: Classification) => classification match {
                case tn: TrueNegative if tn.certainty >= certaintyThreshold => true
                case _ => false
            }),
            "True negatives",
            "List of true negatives"
        ),
        new Category(
            reportDataFolder,
            "false_negatives", 
            ((classification: Classification) => classification match {
                case fn: FalseNegative if fn.certainty >= certaintyThreshold => true
                case _ => false
            }),
            "False negatives",
            "List of false negatives"
        ),
        new Category(
            reportDataFolder,
            "uncategorized", 
            ((classification: Classification) => classification.certainty < certaintyThreshold),
            "uncategorized",
            "List of uncategorized"
        )
    )
}

class Category(val rootFolder: Path, val filenamePrefix: String, val predicate: (Classification => Boolean), val metaTitle: String, val pageTitle: String) {
        val file = rootFolder.resolve(filenamePrefix + ".html").toFile
        var num = 0
        var writer = new BufferedWriter(new FileWriter(file))
        
        writeHead(metaTitle, pageTitle)
        
        def writeHead(metaTitle: String, pageTitle: String) {
            writer.write(
"""<html>
    <head>
        <title>""" + metaTitle + """</title>
    </head>
    <body>
        <h1>""" + pageTitle + """</h1>
""")
        }
    
        def writeFooter() {
            writer.write(
"""</body>
</html>
"""
                )
        }
    
        def write(classification: Classification) {
            val parts = classification.paperId.split('.')
            writer.write(("""<a href="http://localhost/masterarbeit/showPaper.php?an1=%s&an2=%s">%s</a><br>%n""").format(parts(0), parts(1), classification.paperId))
            num = num + 1
        }
        
        def close() {
            writeFooter
            writer.close()
        }
    }