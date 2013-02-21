package parser
import format.arff_json.ArffJsonHeader
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

trait ContentDescribable {
    val contentDescription: ContentDescription
}