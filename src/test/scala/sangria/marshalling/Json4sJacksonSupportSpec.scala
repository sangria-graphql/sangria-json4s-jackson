package sangria.marshalling

import org.scalatest.{Matchers, WordSpec}

import sangria.marshalling.json4s.jackson._
import sangria.marshalling.testkit._

import org.json4s.JsonAST._

class Json4sJacksonSupportSpec extends WordSpec with Matchers with MarshallingBehaviour with InputHandlingBehaviour with ParsingBehaviour {
  "Json4s native integration" should {
    behave like `value (un)marshaller` (Json4sJacksonResultMarshaller)

    behave like `AST-based input unmarshaller` (json4sJacksonFromInput[JValue])
    behave like `AST-based input marshaller` (Json4sJacksonResultMarshaller)

    behave like `input parser` (ParseTestSubjects(
      complex = """{"a": [null, 123, [{"foo": "bar"}]], "b": {"c": true, "d": null}}""",
      simpleString = "\"bar\"",
      simpleInt = "12345",
      simpleNull = "null",
      list = "[\"bar\", 1, null, true, [1, 2, 3]]",
      syntaxError = List("[123, \"FOO\" \"BAR\"")
    ))
  }

  val toRender = JObject(
    "a" -> JArray(List(JNull, JInt(123), JArray(List(JObject("foo" -> JString("bar")))))),
    "b" -> JObject(
      "c" -> JBool(true),
      "d" -> JNull))

  "InputUnmarshaller" should {
    "throw an exception on invalid scalar values" in {
      an [IllegalStateException] should be thrownBy
          Json4sJacksonInputUnmarshaller.getScalarValue(JObject())
    }

    "throw an exception on variable names" in {
      an [IllegalArgumentException] should be thrownBy
          Json4sJacksonInputUnmarshaller.getVariableName(JString("$foo"))
    }

    "render JSON values" in {
      val rendered = Json4sJacksonInputUnmarshaller.render(toRender)

      rendered should be ("""{"a":[null,123,[{"foo":"bar"}]],"b":{"c":true,"d":null}}""")
    }
  }

  "ResultMarshaller" should {
    "render pretty JSON values" in {
      val rendered = Json4sJacksonResultMarshaller.renderPretty(toRender)

      rendered.replaceAll("\r", "") should be (
        """{
          |  "a" : [ null, 123, [ {
          |    "foo" : "bar"
          |  } ] ],
          |  "b" : {
          |    "c" : true,
          |    "d" : null
          |  }
          |}""".stripMargin.replaceAll("\r", ""))
    }

    "render compact JSON values" in {
      val rendered = Json4sJacksonResultMarshaller.renderCompact(toRender)

      rendered should be ("""{"a":[null,123,[{"foo":"bar"}]],"b":{"c":true,"d":null}}""")
    }
  }
}
