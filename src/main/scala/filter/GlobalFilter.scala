package filter
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition

trait GlobalFilter extends Filter {
    override def applyFilter(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = applyFilter(inst)
    def applyFilter(inst: ArffJsonInstancesSource): ArffJsonInstancesSource
}