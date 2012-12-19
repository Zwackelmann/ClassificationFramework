package parser
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import filter.Filter
import common.Path.arffJsonPath
import filter.GlobalFilter
import filter.FilterFactory
import scala.collection.mutable
import format.arff_json.Point
import java.io.BufferedReader
import net.sf.json.JSONSerializer
import java.io.FileReader
import net.sf.json.JSONObject
import net.sf.json.JSONException

object ArffJsonInstancesSource {
    def apply(_source: Iterable[ArffJsonInstance], _header: ArffJsonHeader, _contentDescription: ContentDescription): ArffJsonInstancesSource with ContentDescribable = {
        new ArffJsonInstancesSource with ContentDescribable {
            val header = _header
            val iterator = _source.iterator
            val contentDescription = _contentDescription
        }
    }
    
    def apply(cd: ContentDescription) = new ArffJsonInstancesSource() with ContentDescribable {
        def reader = new BufferedReader(new FileReader(file))
    
        lazy val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                JSONSerializer.toJSON(r.readLine) match {
                    case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                    case _ => throw new RuntimeException("File: " + contentDescription.file + ": The first line in file cannot be interpeted as a JSON object")
                }
            } catch {
                case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e => throw e
            }
            r.close()
            h
        }
        
        def iterator = {
            val r = reader
            r.readLine()
            new ArffJsonInstancesIterator(r, header)
        }
        
        val contentDescription = cd
    }
    
    def apply(base: String, set: ContentDescription.Set, formatHistory: List[FilterFactory]): ArffJsonInstancesSource with ContentDescribable = apply(ContentDescription(base, set, formatHistory))
    
    def apply(_file: File, cd: ContentDescription) = new ArffJsonInstancesSource() with ContentDescribable {
        def reader = new BufferedReader(new FileReader(file))
    
        val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                JSONSerializer.toJSON(r.readLine) match {
                    case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                    case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
                }
            } catch {
                case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e => throw e
            }
            r.close()
            h
        }
        
        def iterator = {
            val r = reader
            r.readLine()
            new ArffJsonInstancesIterator(r, header)
        }
        
        override def file = _file
        
        val contentDescription = cd
    }
    
    def apply(_file: File) = new ArffJsonInstancesSource() {
        def reader = new BufferedReader(new FileReader(file))
    
        val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                JSONSerializer.toJSON(r.readLine) match {
                    case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                    case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
                }
            } catch {
                case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e => throw e
            }
            r.close()
            h
        }
        
        def iterator = {
            val r = reader
            r.readLine()
            new ArffJsonInstancesIterator(r, header)
        }
    }
    
    def centroids(inst: ArffJsonInstancesSource): Map[Int, Point] = {
        def merge(a: Map[Int, Double], b: Map[Int, Double], fun: (Double, Double) => Double) = {
            (for(key <- a.keys ++ b.keys) yield {
                val aValue = a.getOrElse(key, 0.0)
                val bValue = b.getOrElse(key, 0.0)
                key -> fun(aValue, bValue)
            }).toMap
        }
        
        val map = new mutable.HashMap[Int, Pair[Map[Int, Double], Int]] {
            override def default(key: Int) = (Map[Int, Double](), 0)
        }
        
        for(i <- inst) {
            for(group <- i.categories.map(_.substring(0, 2).toInt).distinct) {
                val mapItem = map(group)
                map(group) = Pair(merge(mapItem._1, i.sparseData, (a, b) => a + b), mapItem._2 + 1)
            }
        }
        map.mapValues(m => Point(m._1.mapValues(v => v / m._2), inst.numAttributes)).toMap
    }
}

trait ArffJsonInstancesSource extends Iterable[ArffJsonInstance] {
    def header: ArffJsonHeader
    def iterator: Iterator[ArffJsonInstance]
    def applyFilter(filter: GlobalFilter) = {
        filter.applyFilter(this)
    }
    
    def numAttributes = header.attributes.size
    def numInstances = iterator.size
    
    def flatMap(elemFun: ArffJsonInstance => List[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: FilterFactory): ArffJsonInstancesSource with ContentDescribable = {
        this match {
            case c: ContentDescribable => {
                val h = header
                val it = iterator
                new ArffJsonInstancesSource with ContentDescribable {
                    def header = headerFun(header)
                    def iterator = it.flatMap(elemFun)
                    val contentDescription = c.contentDescription.addFilterFactory(historyAppendix)
                }
            }
        }
    }
    
    def flatMap(elemFun: ArffJsonInstance => List[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader): ArffJsonInstancesSource = {
        def it = iterator
        def h = header
        
        new ArffJsonInstancesSource {
            def header = headerFun(header)
            def iterator = it.flatMap(elemFun)
        }
    }
    
    def map(elemFun: ArffJsonInstance => ArffJsonInstance, headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: FilterFactory): ArffJsonInstancesSource with ContentDescribable = this match {
        case co: ContentDescribable => {
            def it = iterator
            def h = header
            new ArffJsonInstancesSource with ContentDescribable {
                def header = headerFun(h)
                def iterator = it.map(elemFun)
                val contentDescription = co.contentDescription.addFilterFactory(historyAppendix)
            }
        }
        
        case _ => throw new RuntimeException("map(ArffJsonInstance => ArffJsonInstance, ArffJsonHeader => ArffJsonHeader, FilterFactory) cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
    
    def map(elemFun: ArffJsonInstance => ArffJsonInstance, headerFun: ArffJsonHeader => ArffJsonHeader) = {
        def it = iterator
        def h = header
        new ArffJsonInstancesSource {
            def iterator = it.map(elemFun)
            def header = headerFun(h)
        }
    }
    
    def project(ids: List[Int]): ArffJsonInstancesSource = {
        def it = iterator
        def h = header
        
        new ArffJsonInstancesSource {
            def iterator = it.map(inst => inst.project(ids))
            
            val _header = new ArffJsonHeader(
                h.relationName, 
                ids.map(id => h.attributes(id))
            )
            
            def header = _header
        }
    }
        
    def file: File = this match {
        case co: ContentDescribable => co.contentDescription.file
        case _ => throw new RuntimeException("file() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
    
    def save(_file: File) {
        this match {
            case co: ContentDescribable => {
                if(file.exists()) {
                    // do not save the same file you are reading from (doesn't make sence??)
                } else {
                    val writer = new BufferedWriter(new FileWriter(file))
                    writer.write(header.toJson + "\n")
                
                    for(inst <- iterator) {
                        writer.write(inst.toJson + "\n")
                    }
                    writer.close()
                }
            }
            case _ => throw new RuntimeException("file(File) cannot be called if ArffJsonInstancesSource is not ContentDescribable")
        }
    }
    
    def save(): Unit = save(file)
    
    def saved() = this match {
        case co: ContentDescribable => file.exists()
        case _ => throw new RuntimeException("saved() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
}












