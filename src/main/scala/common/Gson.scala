package common

import com.google.gson.GsonBuilder
import com.google.gson.JsonDeserializer
import format.arff_json.ArffJsonAttribute
import com.google.gson.JsonElement
import java.lang.reflect.Type
import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive
import format.arff_json.StringArffJsonAttribute
import format.arff_json.NumericArffJsonAttribute
import format.arff_json.NominalArffJsonAttribute
import com.google.gson.JsonSerializer
import com.google.gson.JsonSerializationContext
import common.Common.escape
import java.lang.reflect.Type
import com.google.gson.stream.JsonReader
import java.io.Reader
import com.google.gson.stream.JsonWriter
import format.arff_json.ArffJsonHeader
import scala.collection.JavaConversions._
import com.google.gson.reflect.TypeToken
import java.util.ArrayList
import java.util.Collection

object Gson {
    class ArffJsonAttributeDeserializer extends JsonDeserializer[ArffJsonAttribute] {
        def deserialize(elem: JsonElement, typeOfT: Type, context: JsonDeserializationContext) = {
            val obj = elem.asInstanceOf[JsonObject]
            val typeDef = obj.get("type").getAsString()
            val name = obj.get("name").getAsString()
            
            typeDef match {
                case "string" => new StringArffJsonAttribute(name)
                case "numeric" => new NumericArffJsonAttribute(name)
                case "nominal" => {
                    val possibleValues = context.deserialize(obj.get("possible-values"), classOf[Array[String]])
                    new NominalArffJsonAttribute(name, possibleValues)
                }
                case _ => throw new RuntimeException("Invalid attribute type")
            }
        }
    }
    
    class NumericArffJsonAttributeSerializer extends JsonSerializer[NumericArffJsonAttribute] {
        def serialize(att: NumericArffJsonAttribute, typeOfSrc: Type, context: JsonSerializationContext) = {
            val o = new JsonObject()
            o.addProperty("name", escape(att.name))
            o.addProperty("type", "numeric")
            o
        }
    }
    
    class StringArffJsonAttributeSerializer extends JsonSerializer[StringArffJsonAttribute] {
        def serialize(att: StringArffJsonAttribute, typeOfSrc: Type, context: JsonSerializationContext) = {
            val o = new JsonObject()
            o.addProperty("name", escape(att.name))
            o.addProperty("type", "string")
            o
        }
    }
    
    class NominalArffJsonAttributeSerializer extends JsonSerializer[NominalArffJsonAttribute] {
        def serialize(att: NominalArffJsonAttribute, typeOfSrc: Type, context: JsonSerializationContext) = {
            val o = new JsonObject()
            o.addProperty("name", att.name)
            o.addProperty("type", "nominal")
            o.add("possible-values", context.serialize(att.possibleValues))
            o
        }
    }
    
    class ArffJsonHeaderDeserializer extends JsonDeserializer[ArffJsonHeader] {
        def deserialize(elem: JsonElement, typeOfT: Type, context: JsonDeserializationContext) = {
            val obj = elem.getAsJsonObject()
            
            val relationName = obj.get("relation-name").getAsString()
            val keySet = obj.entrySet().map(_.getKey())
            
            if(keySet contains "attributes") {
                val attributes = Gson.fromJson(obj.get("attributes"), classOf[Array[ArffJsonAttribute]])
                ArffJsonHeader(relationName, attributes.toIterable)
            } else if(keySet contains "num-attributes") {
                val numAttributes = obj.get("num-attributes").getAsInt()
                ArffJsonHeader(relationName, numAttributes)
            } else {
                throw new RuntimeException("Invalid header")
            }
        }
    }
    
    class ArffJsonHeaderSerializer extends JsonSerializer[ArffJsonHeader] {
        def serialize(header: ArffJsonHeader, typeOfSrc: Type, context: JsonSerializationContext) = {
            val o = new JsonObject()
            o.addProperty("relation-name", escape(header.relationName))
            if(header.explicitAttributes) {
                o.add("attributes", context.serialize(header.attributes))
            } else {
                o.addProperty("num-attributes", header.numAttributes)
            }
            o
        }
    }
    
    private val gsonBuilder = new GsonBuilder()
    
    gsonBuilder.registerTypeAdapter(classOf[ArffJsonAttribute], new ArffJsonAttributeDeserializer())
    gsonBuilder.registerTypeAdapter(classOf[NumericArffJsonAttribute], new NumericArffJsonAttributeSerializer())
    gsonBuilder.registerTypeAdapter(classOf[StringArffJsonAttribute], new StringArffJsonAttributeSerializer())
    gsonBuilder.registerTypeAdapter(classOf[NominalArffJsonAttribute], new NominalArffJsonAttributeSerializer())
    gsonBuilder.registerTypeAdapter(classOf[ArffJsonHeader], new ArffJsonHeaderSerializer())
    gsonBuilder.registerTypeAdapter(classOf[ArffJsonHeader], new ArffJsonHeaderDeserializer())
    
    val gson = gsonBuilder.create()
    
    def fromJson[T](elem: JsonElement, c: Class[T]) = gson.fromJson(elem, c)
    def fromJson[T](elem: JsonElement, t: Type) = gson.fromJson(elem, t)
    def fromJson[T](reader: JsonReader, t: Type) = gson.fromJson(reader, t)
    def fromJson[T](reader: Reader, c: Class[T]) = gson.fromJson(reader, c)
    def fromJson[T](reader: Reader, t: Type) = gson.fromJson(reader, t)
    def fromJson[T](s: String, c: Class[T]) = gson.fromJson(s, c)
    def fromJson[T](s: String, t: Type) = gson.fromJson(s, t)
    
    def toJson(a: Any) = gson.toJson(a)
    def toJson(a: Any, app: Appendable) = gson.toJson(a, app)
    def toJson(a: Any, t: Type) = gson.toJson(a, t)
    def toJson(a: Any, t: Type, app: Appendable) = gson.toJson(a, t, app)
    def toJson(a: Any, t: Type, w: JsonWriter) = gson.toJson(a, t, w)
    def toJson(elem: JsonElement) = gson.toJson(elem)
    def toJson(elem: JsonElement, app: Appendable) = gson.toJson(elem, app)
    def toJson(elem: JsonElement, w: JsonWriter) = gson.toJson(elem, w)
}