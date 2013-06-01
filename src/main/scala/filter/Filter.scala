package filter

import parser.ArffJsonInstancesSource
import java.io.File
import parser.ContentDescription
import common.Path.filterPath
import classifier.CategoryIs
import common.Path
import common.FileManager
import parser.ContentDescribable

object Filter {
    val serializeFilters = true
    def filename(trainBase: ContentDescription, filterFactoryAppendix: String) = trainBase.filename + "__" + filterFactoryAppendix
    def fullFilename(trainBase: ContentDescription, filterFactoryAppendix: String) = Path.filterPath / filename(trainBase, filterFactoryAppendix)
    
    def trainingParams(ffAppendix: String, trainBase: ArffJsonInstancesSource) = trainBase match {
        case cd: ContentDescribable => Some((ffAppendix, cd.contentDescription.filename))
        case _ => None
    }
}

trait Filter {
    def applyFilter(source: ArffJsonInstancesSource, categoryIs: CategoryIs): ArffJsonInstancesSource
    
    /**
     * First Param should be the filename appendix of the FilterFactory
     * Second Param the filename appendix of the ArffJsonInstancesSource 
     */
    def trainingParams: Option[(String, String)]
    
}



