
import parsers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class OperacionesSpec extends AnyFunSpec with Matchers {

  describe("satisfies combinator") {

    // Definimos parsers básicos para los tests
    val parserDigito: Parser[Char] = digit
    val parserCaracter: Parser[Char] = char('a')

    it("should return success if the condition is met") {
      val parserDigitoPar = parserDigito.satisfies(d => d.asDigit % 2 == 0)
      val result = parserDigitoPar("8")

      result shouldBe Success(Resultado('8', ""))
      result.get.elementoParseado shouldBe '8'
      result.get.cadenaRestante shouldBe ""
    }

    it("should fail if the condition is not met") {
      val parserDigitoPar = parserDigito.satisfies(d => d.asDigit % 2 == 0)
      val result = parserDigitoPar("3")

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("Predicate does not hold")
    }

    it("should fail if the parser does not match the input") {
      val parserDigitoPar = parserDigito.satisfies(d => d.asDigit % 2 == 0)
      val result = parserDigitoPar("a") // "a" no es un dígito

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no es un dígito")
    }

    it("should return success with a specific character when condition is met") {
      val parserCaracterA = parserCaracter.satisfies(_ == 'a')
      val result = parserCaracterA("a")

      result shouldBe Success(Resultado('a', ""))
    }

    it("should fail if the input does not match the specific character condition") {
      val parserCaracterA = parserCaracter.satisfies(_ == 'a')
      val result = parserCaracterA("b") // "b" no es 'a'

      result.isFailure shouldBe true
    }
  }

  describe("opt combinator") {

    // Definimos los parsers básicos ya existentes
    val parserStringIn = string("in")

    it("should succeed with Some value if the parser matches") {
      val talVezIn = parserStringIn.opt
      val result = talVezIn("infija")

      result shouldBe Success(Resultado(Some("in"), "fija"))
      result.get.elementoParseado shouldBe Some("in")
      result.get.cadenaRestante shouldBe "fija"
    }

    it("should succeed with None and no character consumed if the parser does not match") {
      val talVezIn = parserStringIn.opt
      val result = talVezIn("fija")

      result shouldBe Success(Resultado(None, "fija"))
      result.get.elementoParseado shouldBe None
      result.get.cadenaRestante shouldBe "fija"
    }

    it("should parse a sequence using opt correctly") {
      val talVezIn = parserStringIn.opt
      val precedencia = talVezIn <> string("fija")
      val result1 = precedencia("infija")
      val result2 = precedencia("fija")

      result1 shouldBe Success(Resultado((Some("in"), "fija"), ""))
      result1.get.elementoParseado shouldBe(Some("in"), "fija")
      result1.get.cadenaRestante shouldBe ""

      result2 shouldBe Success(Resultado((None, "fija"), ""))
      result2.get.elementoParseado shouldBe(None, "fija")
      result2.get.cadenaRestante shouldBe ""
    }

  }

  describe("* combinator") {

    // Definimos un parser básico para el test
    val parserA = char('a')

    it("should succeed with an empty list when the parser is not matched at all") {
      val kleeneA = parserA.*
      val result = kleeneA("bcdef")

      result shouldBe Success(Resultado(List(), "bcdef"))
      result.get.elementoParseado shouldBe List()
      result.get.cadenaRestante shouldBe "bcdef"
    }

    it("should succeed with a list of matched elements when the parser matches multiple times") {
      val kleeneA = parserA.*
      val result = kleeneA("aaabc")

      result shouldBe Success(Resultado(List('a', 'a', 'a'), "bc"))
      result.get.elementoParseado shouldBe List('a', 'a', 'a')
      result.get.cadenaRestante shouldBe "bc"
    }

    it("should succeed with a single match in the list when the parser matches once") {
      val kleeneA = parserA.*
      val result = kleeneA("abc")

      result shouldBe Success(Resultado(List('a'), "bc"))
      result.get.elementoParseado shouldBe List('a')
      result.get.cadenaRestante shouldBe "bc"
    }

    it("should parse an empty string and return an empty list") {
      val kleeneA = parserA.*
      val result = kleeneA("")

      result shouldBe Success(Resultado(List(), ""))
      result.get.elementoParseado shouldBe List()
      result.get.cadenaRestante shouldBe ""
    }

  }

  describe("+ combinator") {

    // Definimos un parser básico para el test
    val parserA = char('a')

    it("should succeed with a single match in the list when the parser matches once") {
      val pKleeneA = parserA.+ // Al menos un match de 'a'
      val result = pKleeneA("a")

      result shouldBe Success(Resultado(List('a'), ""))
      result.get.elementoParseado shouldBe List('a')
      result.get.cadenaRestante shouldBe ""
    }

    it("should succeed with a list of matched elements when the parser matches multiple times") {
      val pKleeneA = parserA.+ // Varias coincidencias de 'a'
      val result = pKleeneA("aaabc")

      result shouldBe Success(Resultado(List('a', 'a', 'a'), "bc"))
      result.get.elementoParseado shouldBe List('a', 'a', 'a')
      result.get.cadenaRestante shouldBe "bc"
    }

    it("should fail if there is no match at the beginning") {
      val pKleeneA = parserA.+ // No hay coincidencias iniciales de 'a'
      val result = pKleeneA("bcdefa")

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage shouldBe "El caracter obtenido no coincide con el esperado"
    }
  }

  describe("map combinator") {

    // Ejemplo de clase simple para usar en el map
    case class Persona(nombre: String, apellido: String)

    // Parsers de ejemplo
    val parserNombre = string("John")
    val parserApellido = string("Doe")
    val parserCompleto = (parserNombre <> (char(' ') ~> parserApellido))
    val personaParser = parserCompleto.map { case (nombre, apellido) => Persona(nombre, apellido) }


    it("should apply the transformation function to the parsed value") {
      val result = personaParser("John Doe")

      result shouldBe Success(Resultado(Persona("John", "Doe"), ""))
    }

    it("should leave the remaining string after parsing") {
      val result = personaParser("John Doe and others")

      result shouldBe Success(Resultado(Persona("John", "Doe"), " and others"))
    }

    it("should fail if the original parser fails") {
      val result = personaParser("Jane Doe") // "Jane" no coincide con "John"

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("La cadena obtenida no coincide con la esperada")
    }
  }

}
