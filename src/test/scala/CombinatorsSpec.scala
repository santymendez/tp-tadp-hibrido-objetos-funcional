import parsers._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}

class CombinatorsSpec extends AnyFunSpec with Matchers {

  describe("<|> combinator") {

    // Definimos los parsers básicos ya existentes
    val p1: Parser[Char] = char('a')
    val p2: Parser[Char] = char('b')

    it("should parse 'a' using the first parser") {
      val aob = p1 <|> p2  // Se usa <|> para combinar los parsers
      val result = aob("arbol")

      result shouldBe Success(Resultado('a', "rbol"))
      result.get.elementoParseado shouldBe 'a'
      result.get.cadenaRestante shouldBe "rbol"
    }

    it("should parse 'b' using the second parser when the first fails") {
      val aob = p1 <|> p2  // Se usa <|> para combinar los parsers
      val result = aob("bort")

      result shouldBe Success(Resultado('b', "ort"))
      result.get.elementoParseado shouldBe 'b'
      result.get.cadenaRestante shouldBe "ort"
    }

    it("should fail if both parsers fail") {
      val aob = p1 <|> p2  // Se usa <|> para combinar los parsers
      val result = aob("casa")

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no coincide con el esperado")
    }

    it("should parse 'a' even if the second parser is different") {
      val p3: Parser[Char] = char('c')

      val aoc = p1 <|> p3  // Cambiamos el segundo parser
      val result = aoc("casa")

      result shouldBe Success(Resultado('c', "asa"))
      result.get.elementoParseado shouldBe 'c'
      result.get.cadenaRestante shouldBe "asa"
    }

  }

  describe("<> combinator") {

    val holaParser = string("hola")
    val mundoParser = string("mundo")
    val parserCombinado = holaParser <> mundoParser

    it("should parse correctly two strings sequentially") {
      parserCombinado("holamundo") match {
        case Success(resultado) =>
          resultado.elementoParseado shouldEqual("hola", "mundo")
          resultado.cadenaRestante shouldEqual ""
        case Failure(_) =>
          fail("Se esperaba un resultado exitoso")
      }
    }

    it("should fail if the first parser doesn't match") {
      // Probamos con una cadena que no pasa el primer parser
      parserCombinado("adiosmundo") match {
        case Success(_) =>
          fail("Se esperaba un error porque el primer parser no coincide")
        case Failure(exception) =>
          exception shouldBe a[RuntimeException]
          exception.getMessage should include("La cadena obtenida no coincide con la esperada")
      }
    }

    it("should fail if the second parser doesn't match") {
      // Probamos con una cadena que pasa el primer parser, pero no el segundo
      parserCombinado("holachau") match {
        case Success(_) =>
          fail("Se esperaba un error porque el segundo parser no coincide")
        case Failure(exception) =>
          exception shouldBe a[RuntimeException]
          exception.getMessage should include("La cadena obtenida no coincide con la esperada")
      }
    }

  }

  describe("~> combinator") {

    // Definimos los parsers básicos
    val p1: Parser[Char] = char('a')
    val p2: Parser[String] = string("hello")

    it("should parse 'a' followed by 'hello' and return only the result of 'hello'") {
      val parser = p1 ~> p2
      val result = parser("ahello")

      result shouldBe Success(Resultado("hello", ""))
      result.get.elementoParseado shouldBe "hello"
      result.get.cadenaRestante shouldBe ""
    }

    it("should parse 'a' followed by 'hello' with remaining text") {
      val parser = p1 ~> p2
      val result = parser("ahelloworld")

      result shouldBe Success(Resultado("hello", "world"))
      result.get.elementoParseado shouldBe "hello"
      result.get.cadenaRestante shouldBe "world"
    }

    it("should fail if first parser fails") {
      val parser = p1 ~> p2
      val result = parser("bhello") // 'b' does not match 'a'

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no coincide con el esperado")
    }

    it("should fail if second parser fails") {
      val parser = p1 ~> p2
      val result = parser("aworld") // 'world' does not match 'hello'

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("La cadena obtenida no coincide con la esperada")
    }

    it("should fail if the input is empty") {
      val parser = p1 ~> p2
      val result = parser("")

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("La cadena está vacia")
    }
  }


  describe("<~ combinator") {

    // Definimos los parsers básicos
    val p1: Parser[Char] = char('a')
    val p2: Parser[String] = string("hello")

    it("should parse 'a' followed by 'hello' and return only the result of 'a'") {
      val parser = p1 <~ p2
      val result = parser("ahello")

      result shouldBe Success(Resultado('a', ""))
      result.get.elementoParseado shouldBe 'a'
      result.get.cadenaRestante shouldBe ""
    }

    it("should parse 'a' followed by 'hello' with remaining text") {
      val parser = p1 <~ p2
      val result = parser("ahelloworld")

      result shouldBe Success(Resultado('a', "world"))
      result.get.elementoParseado shouldBe 'a'
      result.get.cadenaRestante shouldBe "world"
    }

    it("should fail if first parser fails") {
      val parser = p1 <~ p2
      val result = parser("bhello") // 'b' does not match 'a'

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no coincide con el esperado")
    }

    it("should fail if second parser fails") {
      val parser = p1 <~ p2
      val result = parser("aworld") // 'world' does not match 'hello'

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("La cadena obtenida no coincide con la esperada")
    }

    it("should fail if the input is empty") {
      val parser = p1 <~ p2
      val result = parser("")

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("La cadena está vacia")
    }
  }

  describe("sepBy combinator") {

    // Definimos el parser de contenido y el parser separador
    val integerParser: Parser[Int] = integer
    val hyphenParser: Parser[Char] = char('-')

    it("should parse integers separated by hyphens") {
      val numeroDeTelefono = integerParser.sepBy(hyphenParser)
      val result = numeroDeTelefono("4356-1234")

      result shouldBe Success(Resultado(List(4356, 1234), ""))
      result.get.elementoParseado shouldBe List(4356, 1234)
      result.get.cadenaRestante shouldBe ""
    }

    it("should parse a single integer without separator") {
      val numeroDeTelefono = integerParser.sepBy(hyphenParser)
      val result = numeroDeTelefono("5678")

      result shouldBe Success(Resultado(List(5678), ""))
      result.get.elementoParseado shouldBe List(5678)
      result.get.cadenaRestante shouldBe ""
    }

    it("should parse multiple integers separated by hyphens with remaining text") {
      val numeroDeTelefono = integerParser.sepBy(hyphenParser)
      val result = numeroDeTelefono("1234-5678 rest")

      result shouldBe Success(Resultado(List(1234, 5678), " rest"))
      result.get.elementoParseado shouldBe List(1234, 5678)
      result.get.cadenaRestante shouldBe " rest"
    }

    it("should fail if content is not an integer") {
      val numeroDeTelefono = integerParser.sepBy(hyphenParser)
      val result = numeroDeTelefono("hola-chau")

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no es un dígito")
    }

    it("should parse up to the end when no separator is found") {
      val numeroDeTelefono = integerParser.sepBy(hyphenParser)
      val result = numeroDeTelefono("1234 5678")

      result shouldBe Success(Resultado(List(1234), " 5678"))
      result.get.elementoParseado shouldBe List(1234)
      result.get.cadenaRestante shouldBe " 5678"
    }

    it("should not parse a second hola if it's not separated") {
      val holahola = string("hola").sepBy(char('-'))
      val result = holahola("holahola")

      result shouldBe Success(Resultado(List("hola"), "hola"))
    }
  }
}
