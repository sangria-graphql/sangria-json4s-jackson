package sangria.marshalling.json4s

import org.json4s.jackson.JsonMethods.{compact, pretty, parse => parseJson, render => jsonRender}
import org.json4s._
import sangria.marshalling._

import scala.util.Try

object jackson extends Json4sJacksonSupportLowPrioImplicits {
  implicit object Json4sJacksonResultMarshaller extends ResultMarshaller {
    type Node = JValue
    type MapBuilder = ArrayMapBuilder[Node]

    def emptyMapNode(keys: Seq[String]): MapBuilder = new ArrayMapBuilder[Node](keys)
    def addMapNodeElem(
        builder: MapBuilder,
        key: String,
        value: Node,
        optional: Boolean): MapBuilder =
      builder.add(key, value)

    def mapNode(builder: MapBuilder): JObject = JObject(builder.toList)
    def mapNode(keyValues: Seq[(String, JValue)]): JObject = JObject(keyValues.toList)

    def arrayNode(values: Vector[JValue]): JArray = JArray(values.toList)
    def optionalArrayNodeValue(value: Option[JValue]): Node =
      value match {
        case Some(v) => v
        case None => nullNode
      }

    def scalarNode(value: Any, typeName: String, info: Set[ScalarValueInfo]): Node =
      value match {
        case v: String => JString(v)
        case v: Boolean => JBool(v)
        case v: Int => JInt(v)
        case v: Long => JLong(v)
        case v: Float => JDouble(v)
        case v: Double => JDouble(v)
        case v: BigInt => JInt(v)
        case v: BigDecimal => JDecimal(v)
        case v => throw new IllegalArgumentException("Unsupported scalar value: " + v)
      }

    def enumNode(value: String, typeName: String): JString = JString(value)

    val nullNode: JNull.type = JNull

    def renderCompact(node: JValue): String = compact(jsonRender(node))
    def renderPretty(node: JValue): String = pretty(jsonRender(node))
  }

  implicit object Json4sJacksonMarshallerForType extends ResultMarshallerForType[JValue] {
    val marshaller: Json4sJacksonResultMarshaller.type = Json4sJacksonResultMarshaller
  }

  implicit object Json4sJacksonInputUnmarshaller extends InputUnmarshaller[JValue] {
    def getRootMapValue(node: JValue, key: String): Option[JValue] =
      node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    def isMapNode(node: JValue): Boolean = node.isInstanceOf[JObject]
    def getMapValue(node: JValue, key: String): Option[JValue] =
      node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)
    def getMapKeys(node: JValue) = node.asInstanceOf[JObject].obj.map(_._1)

    def isListNode(node: JValue): Boolean = node.isInstanceOf[JArray]
    def getListValue(node: JValue): List[JValue] = node.asInstanceOf[JArray].arr

    def isDefined(node: JValue): Boolean = node != JNull && node != JNothing
    def getScalarValue(node: JValue): Any =
      node match {
        case JBool(b) => b
        case JInt(i) => i
        case JDouble(d) => d
        case JLong(l) => l
        case JDecimal(d) => d
        case JString(s) => s
        case _ => throw new IllegalStateException(s"$node is not a scalar value")
      }

    def getScalaScalarValue(node: JValue): Any = getScalarValue(node)

    def isEnumNode(node: JValue): Boolean = node.isInstanceOf[JString]

    def isScalarNode(node: JValue): Boolean =
      node match {
        case _: JBool | _: JNumber | _: JString => true
        case _ => false
      }

    def isVariableNode(node: JValue) = false
    def getVariableName(node: JValue) =
      throw new IllegalArgumentException("variables are not supported")

    def render(node: JValue): String = compact(jsonRender(node))
  }

  private object Json4sJacksonToInput extends ToInput[JValue, JValue] {
    def toInput(value: JValue): (JValue, Json4sJacksonInputUnmarshaller.type) =
      (value, Json4sJacksonInputUnmarshaller)
  }

  implicit def json4sJacksonToInput[T <: JValue]: ToInput[T, JValue] =
    Json4sJacksonToInput.asInstanceOf[ToInput[T, JValue]]

  private object Json4sJacksonFromInput extends FromInput[JValue] {
    val marshaller: Json4sJacksonResultMarshaller.type = Json4sJacksonResultMarshaller
    def fromResult(node: marshaller.Node): JValue = node
  }

  implicit def json4sJacksonFromInput[T <: JValue]: FromInput[T] =
    Json4sJacksonFromInput.asInstanceOf[FromInput[T]]

  implicit object Json4sJacksonInputParser extends InputParser[JValue] {
    def parse(str: String): Try[JValue] =
      Try(parseJson(str, useBigDecimalForDouble = true, useBigIntForLong = false))
  }
}

trait Json4sJacksonSupportLowPrioImplicits {
  implicit val Json4sJacksonInputUnmarshallerJObject: InputUnmarshaller[JObject] =
    jackson.Json4sJacksonInputUnmarshaller.asInstanceOf[InputUnmarshaller[JObject]]
}
