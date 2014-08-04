package format.arff_json

import common.Common.escape
import common.Common.jsonToScalaType
import weka.core.Attribute
import scala.collection.JavaConversions._
import parser.ArffJsonParser
import com.google.gson.JsonParser
import com.google.gson.JsonObject
import com.google.gson.JsonArray
import common.Gson

object ArffJsonInstance {
    def main(args: Array[String]) {
        val s = """[["5530038",["70-01","70Hxx","70F10","37N05"]], ["Introduction to Hamiltonian dynamical systems and the $N$-body problem. 2nd ed.", "[For the review of the 1st ed. see (1992; 0743.70006).]", ["New York, NY: Springer"], ["stability"," symmetries"," integrable systems"," perturbation theory"," KAM theory"]]]"""
        val h = Gson.fromJson("""{"relation-name" : "final_format", "attributes" : [{"name" : "title", "type" : "string"}, {"name" : "abstract", "type" : "string"}, {"name" : "journals", "type" : "string"}, {"name" : "terms", "type" : "string"}]}""", classOf[ArffJsonHeader])
        
        val i = ArffJsonInstance(s, h)
        println(i)
    }
    
    def dataToJson(d: Any): String = d match {
        case s: String => "\"" + escape(s) + "\""
        case d: Double => d.toString
        case i: Int => i.toString
        case l: List[_] => "[" + l.map(dataToJson(_)).mkString(",") + "]"
        case _ => throw new RuntimeException("Unsupported Type")
    }
    
    /*def apply(json: JSONArray, numAttributes: Int) = {
        val metadata = jsonToScalaType(json.get(0)).asInstanceOf[List[Any]]
        json.get(1) match {
            case arr: JSONArray => {
                val data = jsonToScalaType(arr).asInstanceOf[List[Any]]
                new ArffJsonInstance(metadata(0).asInstanceOf[String], metadata(1).asInstanceOf[List[String]], data) with DenseData {
                    
                }
            }
            case obj: JSONObject => {
                val data = jsonToScalaType(obj).asInstanceOf[Map[String, Any]].map(x => x._1.toInt -> x._2)
                new SparseArffJsonInstance(metadata(0).asInstanceOf[String], metadata(1).asInstanceOf[List[String]], data, numAttributes)
            }
            case _ => throw new RuntimeException()
        }
    }*/
    
    def apply(id: String, mscClasses: List[String], dataString: String, numAttributes: Int, sparse: Boolean) = {
        if(sparse) {
            new ArffJsonInstance(id, mscClasses) with SparseData {
                private var storedData = dataString
                
                lazy val dataMap = {
                    // val obj = jsonToScalaType(new JsonParser().parse(storedData).asInstanceOf[JsonObject]).asInstanceOf[Map[String, Any]]
                    val obj = new JsonParser().parse(storedData).asInstanceOf[JsonObject]
                    storedData = null
                    obj.entrySet().map(entry => entry.getKey().toInt -> Gson.fromJson(entry.getValue(), classOf[Any])).toMap
                }
                
                override def toJson = {
                    if(storedData != null) {
                        "[" + 
                            "[" + 
                                (List(id, categories).map(dataToJson(_))).mkString(",") + 
                            "]," + dataString + 
                        "]"
                    } else {
                        super.toJson
                    }
                }
                val _numAttributes = numAttributes
            }
        } else {
            new ArffJsonInstance(id, mscClasses) with DenseData {
                private var storedData = dataString
                
                lazy val dataList = {
                    val arr = jsonToScalaType(new JsonParser().parse(storedData).asInstanceOf[JsonArray])
                    storedData = null
                    arr.asInstanceOf[List[Any]]
                }
                
                override def toJson = {
                    if(storedData != null) {
                        "[" + 
                            "[" + 
                                (List(id, categories).map(dataToJson(_))).mkString(",") + 
                            "]," + dataString + 
                        "]"
                    } else {
                        super.toJson
                    }
                }
            }
        }
    }
    
    def apply(id: String, categories: List[String], _dataList: List[Any], sparse: Boolean = true) = {
        if(!sparse) {
            new ArffJsonInstance(id, categories) with DenseData {
                val dataList = _dataList
            }
        } else {
            val _dataMap = (0 until _dataList.size)
                .zip(_dataList)
                .filter(_._2 match {
                    case d: Double => d != 0.0
                    case i: Int => i != 0
                    case _ => true
                })
                .toMap
            new ArffJsonInstance(id, categories) with SparseData {
                val dataMap = _dataMap
                val _numAttributes = _dataList.size
            }
        }
    }
    
    def apply(id: String, mscClasses: List[String], _dataMap: Map[Int, Any], numAttributes: Int) = new ArffJsonInstance(id, mscClasses) with SparseData {
        val dataMap = _dataMap
        val _numAttributes = numAttributes
    }
    
    /*def stringToArffJsonInstance(str: String, header: ArffJsonHeader) = {
        apply(new DefaultJSONParser(str).parse().asInstanceOf[JSONArray], header.attributes.size)
    }*/
    
    // def apply(json: JSONArray, header: ArffJsonHeader): ArffJsonInstance = apply(json, header.attributes.size)
    def apply(line: String, header: ArffJsonHeader, debug: Boolean): ArffJsonInstance = {
        ArffJsonParser.parseLazy(line, header.numAttributes, debug)
    }
    def apply(line: String, header: ArffJsonHeader): ArffJsonInstance = apply(line, header, false)
    
    def addValue(value: Any, att: Attribute) = {
        if(att.isNumeric()) {
            value.asInstanceOf[Double]
        } else if(att.isString()) {
            att.addStringValue(value.asInstanceOf[String])
        } else if(att.isNominal()) {
            value match {
                case s: String => att.indexOfValue(s)
                case i: Int => i
                case d: Double => d
            }
        } else {
            throw new RuntimeException()
        }
    }
}


abstract class ArffJsonInstance(val id: String, val categories: List[String]) extends Point {
    def toJson: String
    def numAttributes(): Int
    def dataAt(index: Int): Any
    def numNonZeroValues: Int
    
    def project(ids: Set[Int]) = {
        val idsWithIndex = ids.zip(0 until ids.size).toMap
        
        this match {
            case sp: SparseData => new ArffJsonInstance(
                sp.id, 
                sp.categories 
            ) with SparseData {
                val dataMap = sp.dataMap
                    .filter(m => ids.contains(m._1))
                    .map(kv => idsWithIndex(kv._1) -> kv._2)
                    
                val _numAttributes = ids.size
            }
            case de: DenseData => 
                new ArffJsonInstance(
                    de.id,
                    de.categories
                ) with DenseData {
                    val dataList = de.dataList
                        .zip(0 until de.dataList.size)
                        .filter(l => ids.contains(l._2))
                        .map(_._1)
                }
            
        }
    }
    
    def union(ids: List[Int], unionFun: (Any, Any) => Any) = {
        val idsWithIndex = ids.zip(0 until ids.length).toMap
        
        this match {
            case sp: SparseData => new ArffJsonInstance(
                sp.id, 
                sp.categories
            ) with SparseData {
                val dataMap = Map(0 -> (sp.dataMap
                    .filter(m => ids.contains(m._1))
                    .map(kv => kv._2)
                    .reduceLeft(unionFun)))
                    
                val _numAttributes = ids.length
            }
            case de: DenseData => 
                new ArffJsonInstance(
                    de.id,
                    de.categories
                ) with DenseData {
                    val dataList = List(de.dataList
                        .zip(0 until de.dataList.size)
                        .filter(l => ids.contains(l._2))
                        .map(_._1)
                        .reduceLeft(unionFun))
                }
            
        }
    }
}



















