package filter
import java.io.File

trait StorableFilterFactory extends FilterFactory {
    def load(file: File): Filter
}