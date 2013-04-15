package filter
import parser.ArffJsonInstancesSource
import classifier.CategoryIs

trait GlobalFilter extends Filter {
    override def applyFilter(inst: ArffJsonInstancesSource, categoryIs: CategoryIs) = applyFilter(inst)
    def applyFilter(inst: ArffJsonInstancesSource): ArffJsonInstancesSource
}