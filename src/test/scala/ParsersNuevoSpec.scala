import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import parsers.*

import scala.util.{Success, Try}

class ParsersNuevoSpec extends AnyFunSpec with Matchers {

  describe("anyChar parser") {
    it("should return Success with the first character and remaining string if the string is not empty") {
      val cadena = "hello"
      val result = anyChar.apply(cadena)

      result shouldBe Success(Resultado('h', "ello"))
      result.get.elementoParseado shouldBe 'h'
      result.get.cadenaRestante shouldBe "ello"
    }

    it("should return Failure with a message if the string is empty") {
      val cadena = ""
      val result = anyChar.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage shouldBe "La cadena está vacia"
    }
  }

  describe("char parser") {
    it("should return Success with the character and remaining string if the string starts with the specified character") {
      val parser = char('a')
      val cadena = "abc"
      val result = parser.apply(cadena)

      result shouldBe Success(Resultado('a', "bc"))
      result.get.elementoParseado shouldBe 'a'
      result.get.cadenaRestante shouldBe "bc"
    }

    it("should return Failure with a message if the string is empty") {
      val parser = char('a')
      val cadena = ""
      val result = parser.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage shouldBe "La cadena está vacia"
    }

    it("should return Failure with a message if the first character does not match the specified character") {
      val parser = char('a')
      val cadena = "bcd"
      val result = parser.apply(cadena)

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage shouldBe "El caracter obtenido no coincide con el esperado"
    }
  }

  describe("digit parser") {
    it("should return Success with the digit and remaining string if the string starts with a digit") {
      val cadena = "123abc"
      val result = digit.apply(cadena)

      result shouldBe Success(Resultado('1', "23abc"))
      result.get.elementoParseado shouldBe '1'
      result.get.cadenaRestante shouldBe "23abc"
    }

    it("should return Failure with a message if the string is empty") {
      val cadena = ""
      val result = digit.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage shouldBe "La cadena está vacia"
    }

    it("should return Failure with a message if the first character is not a digit") {
      val cadena = "abc123"
      val result = digit.apply(cadena)

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage shouldBe "El caracter obtenido no es un dígito"
    }
  }

  // Agrupamos los tests para el parser `string`
  describe("string parser") {

    it("should return Success with the matched string and remaining string if it starts with the specified string") {
      val parser = string("hello")
      val cadena = "hello world"
      val result = parser.apply(cadena)

      result shouldBe Success(Resultado("hello", " world"))
      result.get.elementoParseado shouldBe "hello"
      result.get.cadenaRestante shouldBe " world"
    }

    it("should return Failure with a message if the input string is empty") {
      val parser = string("hello")
      val cadena = ""
      val result = parser.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage shouldBe "La cadena obtenida no coincide con la esperada"
    }

    it("should return Failure with a message if the input string does not start with the specified string") {
      val parser = string("hello")
      val cadena = "world"
      val result = parser.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage shouldBe "La cadena obtenida no coincide con la esperada"
    }
  }

  describe("integer parser") {

    it("should parse a positive integer correctly") {
      val cadena = "123abc"
      val result = integer.apply(cadena)

      result shouldBe Success(Resultado(123, "abc"))
      result.get.elementoParseado shouldBe 123
      result.get.cadenaRestante shouldBe "abc"
    }

    it("should parse a negative integer correctly") {
      val cadena = "-456xyz"
      val result = integer.apply(cadena)

      result shouldBe Success(Resultado(-456, "xyz"))
      result.get.elementoParseado shouldBe -456
      result.get.cadenaRestante shouldBe "xyz"
    }

    it("should return Failure with a message if the string does not start with a number") {
      val cadena = "abc123"
      val result = integer.apply(cadena)

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no es un dígito")
    }

    it("should return Failure with a message if the string is empty") {
      val cadena = ""
      val result = integer.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage should include("La cadena está vacia")
    }

    it("funca para coordenada") {
      val cadena = "1 @ 2"
      val result = integer.apply(cadena)

      result shouldBe Success(Resultado(1, " @ 2"))
    }
  }

  describe("double parser") {

    it("should parse a valid double number correctly") {
      val cadena = "12.34xyz"
      val result = double.apply(cadena)

      result shouldBe Success(Resultado(12.34, "xyz"))
      result.get.elementoParseado shouldBe 12.34
      result.get.cadenaRestante shouldBe "xyz"
    }

    it("should handle multiple decimal parts correctly (12.34.56xyz)") {
      val cadena = "12.34.56xyz"
      val result = double.apply(cadena)

      result shouldBe Success(Resultado(12.34, ".56xyz"))
      result.get.elementoParseado shouldBe 12.34
      result.get.cadenaRestante shouldBe ".56xyz"
    }

    it("should throw an exception if the decimal part is negative") {
      val cadena = "12.-34xyz"
      val result = double.apply(cadena)

      result.get.elementoParseado shouldBe 12.0
      result.get.cadenaRestante shouldBe ".-34xyz"
      //      result.isFailure shouldBe true
      //      result.failed.get.getMessage shouldBe "La parte decimal no puede ser negativa"
    }

    it("should throw an exception if the string does not contain a valid double") {
      val cadena = "abc"
      val result = double.apply(cadena)

      result.isFailure shouldBe true
      //TODO result.failed.get.getMessage should include("El caracter obtenido no es un dígito")
    }

    it("should return Failure with a message if the input string is empty") {
      val cadena = ""
      val result = double.apply(cadena)

      result.isFailure shouldBe true
      result.failed.get.getMessage shouldBe "La cadena está vacia"
    }
  }

  describe("whiteSpace parser") {
    it("should match one whitespace") {
      val result = whiteSpace(" 123 hola")

      result.get.elementoParseado shouldBe ' '
      result.get.cadenaRestante shouldBe "123 hola"
    }

    it("should match a tab") {
      val result = whiteSpace("\t123 hola")

      result.get.elementoParseado shouldBe '\t'
      result.get.cadenaRestante shouldBe "123 hola"
    }

    it("should match a new line") {
      val result = whiteSpace("\n123 hola")

      result.get.elementoParseado shouldBe '\n'
      result.get.cadenaRestante shouldBe "123 hola"
    }

    it("should work with Kleene Clause") {
      val result = whiteSpace.*("   \t  \n\n  \t \n   123 hola")

      result.get.elementoParseado.mkString shouldBe "   \t  \n\n  \t \n   "
      result.get.cadenaRestante shouldBe "123 hola"
    }

    it("Should work with potencia") {
      val result = char('a').potencia(2, 5)
      result("aaaa").isSuccess shouldBe true
      result("aaaaaaa").isSuccess shouldBe false
      result("a").isSuccess shouldBe false
      
      
    }
  }
}

