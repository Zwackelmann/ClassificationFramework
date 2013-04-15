package filter

import parser.ArffJsonInstancesSource
import java.io.File
import parser.ContentDescription
import common.Path.filterPath
import classifier.CategoryIs
import common.Path
import common.FileManager

object Filter {
    val serializeFilters = true
    def filename(trainBase: ContentDescription, filterFactoryAppendix: String) = trainBase.filename + "__" + filterFactoryAppendix
    def fullFilename(trainBase: ContentDescription, filterFactoryAppendix: String) = Path.filterPath / filename(trainBase, filterFactoryAppendix)
}

trait Filter {
    def applyFilter(source: ArffJsonInstancesSource, categoryIs: CategoryIs): ArffJsonInstancesSource
}



