package format.arff_json

import net.sf.json.JSONArray
import net.sf.json.JSONObject
import common.Common.escape
import common.Common.jsonToScalaType
import weka.core.Attribute
import scala.collection.JavaConversions._
import parser.ArffJsonParser

object ArffJsonInstance {
    def dataToJson(d: Any): String = d match {
        case s: String => "\"" + escape(s) + "\""
        case d: Double => d.toString
        case i: Int => i.toString
        case l: List[_] => "[" + l.map(dataToJson(_)).mkString(",") + "]"
        case _ => throw new RuntimeException("Unsupported Type")
    }
    
    def apply(json: JSONArray, numAttributes: Int) = {
        val metadata = jsonToScalaType(json.get(0)).asInstanceOf[List[Any]]
        
        json.get(1) match {
            case arr: JSONArray => {
                val data = jsonToScalaType(arr).asInstanceOf[List[Any]]
                new DenseArffJsonInstance(metadata(0).asInstanceOf[String], metadata(1).asInstanceOf[List[String]], data)
            }
            case obj: JSONObject => {
                val data = jsonToScalaType(obj).asInstanceOf[Map[String, Any]].map(x => x._1.toInt -> x._2)
                new SparseArffJsonInstance(metadata(0).asInstanceOf[String], metadata(1).asInstanceOf[List[String]], data, numAttributes)
            }
            case _ => throw new RuntimeException()
        }
    }
    
    def apply(json: JSONArray, header: ArffJsonHeader): ArffJsonInstance = apply(json, header.attributes.size)
    def apply(line: String, header: ArffJsonHeader) = ArffJsonParser.parse(line, header.attributes.length)
    
    def apply(id: String, categories: List[String], data: List[Any], sparse: Boolean) = {
        if(!sparse) {
            new DenseArffJsonInstance(id, categories, data)
        } else {
            val dataMap = (0 until data.size)
                .zip(data)
                .filter(_._2 match {
                    case d: Double => d != 0.0
                    case _ => true
                })
                .toMap
            new SparseArffJsonInstance(id, categories, dataMap, data.size)
        }
    }
    
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
    def project(ids: List[Int]) = {
        val idsWithIndex = ids.zip(0 until ids.length).toMap
        
        this match {
            case sp: SparseArffJsonInstance => new SparseArffJsonInstance(
                sp.id, 
                sp.categories, 
                sp.dataMap
                    .filter(m => ids.contains(m._1))
                    .map(kv => idsWithIndex(kv._1) -> kv._2),
                ids.length
            )
            case de: DenseArffJsonInstance => 
                new DenseArffJsonInstance(
                    de.id,
                    de.categories,
                    de.dataList
                        .zip(0 until de.dataList.size)
                        .filter(l => ids.contains(l._2))
                        .map(_._1)
                )
            
        }
    }
    def union(ids: List[Int], unionFun: (Any, Any) => Any) = {
        val idsWithIndex = ids.zip(0 until ids.length).toMap
        
        this match {
            case sp: SparseArffJsonInstance => new SparseArffJsonInstance(
                sp.id, 
                sp.categories, 
                Map(0 -> (sp.dataMap
                    .filter(m => ids.contains(m._1))
                    .map(kv => kv._2)
                    .reduceLeft(unionFun))),
                ids.length
            )
            case de: DenseArffJsonInstance => 
                new DenseArffJsonInstance(
                    de.id,
                    de.categories,
                    List(de.dataList
                        .zip(0 until de.dataList.size)
                        .filter(l => ids.contains(l._2))
                        .map(_._1)
                        .reduceLeft(unionFun))
                )
            
        }
    }
}




