package sangria.marshalling.json4s

import org.json4s.jackson.JsonMethods.{render ⇒ jsonRender, pretty, compact}
import org.json4s.JsonAST._
import sangria.marshalling._

object jackson extends Json4sJacksonSupportLowPrioImplicits {
  implicit object Json4sJacksonResultMarshaller extends ResultMarshaller {
    type Node = JValue

    def emptyMapNode = JObject(Nil)
    def mapNode(keyValues: Seq[(String, JValue)]) = JObject(keyValues.toList)
    def addMapNodeElem(node: JValue, key: String, value: JValue, optional: Boolean) =
      JObject(node.asInstanceOf[JObject].obj :+ (key → value))

    def arrayNode(values: Vector[JValue]) = JArray(values.toList)
    def optionalArrayNodeValue(value: Option[JValue]) = value match {
      case Some(v) ⇒ v
      case None ⇒ nullNode
    }

    def stringNode(value: String) = JString(value)
    def floatNode(value: Double) = JDouble(value)
    def booleanNode(value: Boolean) = JBool(value)
    def intNode(value: Int) = JInt(value)
    def bigIntNode(value: BigInt) = JInt(value)
    def bigDecimalNode(value: BigDecimal) = JDecimal(value)

    def nullNode = JNull

    def renderCompact(node: JValue) =  compact(jsonRender(node))
    def renderPretty(node: JValue) = pretty(jsonRender(node))
  }

  implicit object Json4sJacksonMarshallerForType extends ResultMarshallerForType[JValue] {
    val marshaller = Json4sJacksonResultMarshaller
  }

  implicit object Json4sJacksonInputUnmarshaller extends InputUnmarshaller[JValue] {
    def getRootMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    def isMapNode(node: JValue) = node.isInstanceOf[JObject]
    def getMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)
    def getMapKeys(node: JValue) = node.asInstanceOf[JObject].obj map (_._1)

    def isListNode(node: JValue) = node.isInstanceOf[JArray]
    def getListValue(node: JValue) = node.asInstanceOf[JArray].arr

    def isDefined(node: JValue) = node != JNull && node != JNothing
    def getScalarValue(node: JValue) = node match {
      case JBool(b) ⇒ b
      case JInt(i) ⇒ i
      case JDouble(d) ⇒ d
      case JLong(l) ⇒ l
      case JDecimal(d) ⇒ d
      case JString(s) ⇒ s
      case _ ⇒ throw new IllegalStateException(s"$node is not a scalar value")
    }

    def getScalaScalarValue(node: JValue) = getScalarValue(node)

    def isEnumNode(node: JValue) = node.isInstanceOf[JString]

    def isScalarNode(node: JValue) = node match {
      case _: JBool | _: JNumber | _: JString ⇒ true
      case _ ⇒ false
    }

    def isVariableNode(node: JValue) = false
    def getVariableName(node: JValue) = throw new IllegalArgumentException("variables are not supported")

    def render(node: JValue) = compact(jsonRender(node))
  }

  private object Json4sJacksonToInput extends ToInput[JValue, JValue] {
    def toInput(value: JValue) = (value, Json4sJacksonInputUnmarshaller)
  }

  implicit def json4sJacksonToInput[T <: JValue]: ToInput[T, JValue] =
    Json4sJacksonToInput.asInstanceOf[ToInput[T, JValue]]

  private object Json4sJacksonFromInput extends FromInput[JValue] {
    val marshaller = Json4sJacksonResultMarshaller
    def fromResult(node: marshaller.Node) = node
  }

  implicit def json4sJacksonFromInput[T <: JValue]: FromInput[T] =
    Json4sJacksonFromInput.asInstanceOf[FromInput[T]]
}

trait Json4sJacksonSupportLowPrioImplicits {
  implicit val Json4sJacksonInputUnmarshallerJObject =
    jackson.Json4sJacksonInputUnmarshaller.asInstanceOf[InputUnmarshaller[JObject]]
}
