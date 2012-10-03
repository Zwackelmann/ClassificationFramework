package format.arff_json

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import scala.collection.mutable.HashMap
import weka.core.Instance
import weka.core.Attribute
import weka.core.SparseInstance
import weka.core.Instances
import weka.core.FastVector
import java.io.Writer
import parser.ArffJsonInstancesReader
import parser.ArffJsonInstancesSource

object ArffJsonInstances {
    val serializeInstances = true
}

class ArffJsonInstances(inst: ArffJsonInstancesSource, virtualAttributes: List[Pair[ArffJsonAttribute, ArffJsonInstance => Any]] = List()) {
    val header = inst.header
    
    private var _instances: Instances = {
        val arffAttributes = new FastVector()
        
        var attributeIndex = 0
        
        for(vAttribute <- virtualAttributes) {
            arffAttributes.addElement(vAttribute._1.toArffAttribute(attributeIndex))
            attributeIndex = attributeIndex + 1
        }
        
        for(att <- header.attributes) yield {
            arffAttributes.addElement(att.toArffAttribute(attributeIndex))
            attributeIndex = attributeIndex + 1
        }
        
        new Instances(header.relationName, arffAttributes, 1000)
    }
    
    def numVAttributes = virtualAttributes.size
    
    val instancesMetadata = new HashMap[Int, Pair[String, List[String]]]
    
    def instances = _instances
    def instances_=(newInstances: Instances) {
        if(newInstances.numInstances() != instancesMetadata.size) {
            throw new RuntimeException()
        } else {
            _instances = newInstances
            header.adeptToInstances(newInstances)
        }
    }
    
    def numInstances = instances.numInstances()
    def numAttributes = instances.numAttributes() + numVAttributes
    
    def arffJsonInstance(index: Int) = {
        val metadata = instancesMetadata(index)
        val instance = instances.instance(index)
        
        instance match {
            case sparseInst: SparseInstance => {
                val dataMap = (for(i <- 0 until sparseInst.numValues()) yield {
                    val index = sparseInst.index(i)
                    val att = sparseInst.attributeSparse(i)
                    val value = sparseInst.value(att)
                    
                    if(att.isString)
                        index -> att.value(value.toInt)
                    else 
                        index -> value
                }).toMap
                
                new SparseArffJsonInstance(metadata._1, metadata._2, dataMap, instances.numAttributes())
            }
            
            case denseInst: Instance => {
                val dataList = (for(i <- 0 until denseInst.numAttributes()) yield {
                    val att = denseInst.attribute(i)
                    val value = denseInst.value(i)
                    
                    if(att.isString)
                        att.value(value.toInt)
                    else 
                        value
                }).toList
                
                new DenseArffJsonInstance(metadata._1, metadata._2, dataList)
            }
        }
    }
    
    def write(out: Writer, sparse: Boolean) {
        out.write(header.toJson + "\n")
        
        for(i <- 0 until numInstances) {
            val instanceData = {
                val inst = instances.instance(i)
                (for(j <- numVAttributes until inst.numAttributes()) yield {
                    if(inst.attribute(j).isString()) inst.stringValue(j)
                    else if(inst.attribute(j).isNumeric()) inst.value(j)
                    else throw new RuntimeException()
                }).toList
            }
            val meta = instancesMetadata(i)
            
            out.write(ArffJsonInstance(meta._1, meta._2, instanceData, sparse).toJson + "\n")
        }
    }
    
    def write(out: Writer, fun: ArffJsonInstance => String, writeHead: Boolean = false) {
        if(writeHead)
            out.write(header.toJson + "\n")
        
        for(i <- 0 until numInstances) {
            val instanceData = {
                val inst = instances.instance(i)
                (for(j <- numVAttributes until inst.numAttributes()) yield {
                    if(inst.attribute(j).isString()) inst.stringValue(j)
                    else if(inst.attribute(j).isNumeric()) inst.value(j)
                    else throw new RuntimeException()
                }).toList
            }
            val meta = instancesMetadata(i)
            
            out.write(fun(ArffJsonInstance(meta._1, meta._2, instanceData, true)) + "\n")
        }
    }
    
    var instancesCounter = 0
    import ArffJsonInstance.addValue
    
    for(arffJsonInstance <- inst.iterator) {
        instancesMetadata(instancesCounter) = Pair(arffJsonInstance.id, arffJsonInstance.mscClasses)
        
        val inst = arffJsonInstance match {
            case denseInst: DenseArffJsonInstance => {                   
                var attributeIndex = 0
                val instanceData = new Array[Double](instances.numAttributes())
                
                for(vAtt <- virtualAttributes) {
                    val att = instances.attribute(attributeIndex)
                    val value = vAtt._2(arffJsonInstance)
                    instanceData(attributeIndex) = addValue(value, att)
                    
                    attributeIndex = attributeIndex + 1
                }
                
                for(value <- denseInst.dataList) {
                    val att = instances.attribute(attributeIndex)
                    instanceData(attributeIndex) = addValue(value, att)
                    
                    attributeIndex = attributeIndex + 1
                }
                new Instance(1.0, instanceData)
            }
            
            case sparseInst: SparseArffJsonInstance => {
                val (indexes, values) = (
                    (for(index <- 0 until virtualAttributes.size) yield {
                        val att = instances.attribute(index)
                        (index, addValue(virtualAttributes(index)._2(sparseInst), att))
                    }) ++ (for((attIndex, value) <- sparseInst.dataMap) yield {
                        val att = instances.attribute(attIndex + virtualAttributes.size)
                        (attIndex + virtualAttributes.size, addValue(value, att))
                    })
                ).sortBy(_._1).unzip
                
                new SparseInstance(1.0, values.toArray, indexes.toArray, header.attributes.size)
            }
        }
        
        instances.add(inst)
        inst.setDataset(instances)
        instancesCounter = instancesCounter + 1
    }
    instances.compactify()
    
    if(instances.numInstances() != instancesMetadata.size) {
        throw new RuntimeException()
    }
}
