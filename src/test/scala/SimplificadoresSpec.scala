import dibujadores.{Circulo, ColorDibujable, EscalaDibujable, Grupo, Rectangulo, RotacionDibujable, TraslacionDibujable, Triangulo}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import tadp.poc.drawing.DrawingAST.Cuadrado
import dibujable.*
import dibujadores.*
import parserDibujable.ParserDibujable
import parsers.Resultado

import scala.runtime.Nothing$

class SimplificadoresSpec extends AnyFunSpec with Matchers {
  describe("escala") {
    it("escala nula") {
      val p = EscalaDibujable(1, 1, Triangulo((1, 1), (1, 1), (1, 1)))
      simplificarEscala(p) shouldBe Triangulo((1, 1), (1, 1), (1, 1))
    }

    it("escala recursiva") {
      val p = EscalaDibujable(20, 20, EscalaDibujable(2, 2, Rectangulo((1, 1), (1, 1))))
      simplificarEscala(p) shouldBe EscalaDibujable(40, 40, Rectangulo((1, 1), (1, 1)))
    }

  }
//  describe("color") {

//    it("color recursivo") {
////      val c = ColorDibujable((200, 200, 200), ColorDibujable((100, 100, 100), Circulo((1, 2), 4)))
////      simplificarColor(c) shouldBe ColorDibujable((100, 100, 100), Circulo((1, 2), 4))
////    }
////  }

  describe("rotacion") {
    it("rotacion nula") {
      val p = RotacionDibujable(0, Rectangulo((1, 1), (1, 1)))
      simplificarRotacion(p) shouldBe Rectangulo((1, 1), (1, 1))
    }

    it("rotacion anidada") {
      val p = RotacionDibujable(100, RotacionDibujable(20, Rectangulo((1, 1), (1, 1))))
      simplificarRotacion(p) shouldBe RotacionDibujable(120, Rectangulo((1, 1), (1, 1)))
    }
  }

  describe("traslacion") {
    it("traslacion nula") {
      val p = TraslacionDibujable(0, 0, Circulo((10, 10), 10))
      simplificarTraslacion(p) shouldBe Circulo((10, 10), 10)
    }

    it("traslacion anidada") {
      val p = TraslacionDibujable(100, 5, TraslacionDibujable(20, 10, Circulo((0, 5), 10)))
      simplificarTraslacion(p) shouldBe TraslacionDibujable(120, 15, Circulo((0, 5), 10))
    }
  }

  describe("grupos") {

    it("ejemplo todos con mismo color") {
      val p = Grupo(List(ColorDibujable((200, 200, 200), Rectangulo((100, 100), (100, 100))), ColorDibujable((200, 200, 200), Circulo((100, 300), 150))))
      simplificarAst(p) shouldBe ColorDibujable((200, 200, 200), Grupo(List(Rectangulo((100, 100), (100, 100)), Circulo((100, 300), 150))))
    }

    it("ejemplo todos con la misma escala") {
      val p = Grupo(List(EscalaDibujable(200, 200, Rectangulo((100, 100), (100, 100))), EscalaDibujable(200, 200, Circulo((100, 300), 150))))
      simplificarAst(p) shouldBe EscalaDibujable(200, 200, Grupo(List(Rectangulo((100, 100), (100, 100)), Circulo((100, 300), 150))))
    }

    it("ejemplo todos con la misma rotacion") {
      val p = Grupo(List(RotacionDibujable(200, Rectangulo((100, 100), (100, 100))), RotacionDibujable(200, Circulo((100, 300), 150)), RotacionDibujable(200, Circulo((5, 10), 1))))
      simplificarAst(p) shouldBe RotacionDibujable(200, Grupo(List(Rectangulo((100, 100), (100, 100)), Circulo((100, 300), 150), Circulo((5, 10), 1))))
    }


    it("ejemplo no todos con el mismo color, no simplifica") {
      val p = Grupo(List(ColorDibujable((200, 200, 200), Rectangulo((100, 100), (100, 100))), ColorDibujable((250, 200, 200), Circulo((100, 300), 150))))
      simplificarAst(p) shouldBe Grupo(List(ColorDibujable((200, 200, 200), Rectangulo((100, 100), (100, 100))), ColorDibujable((250, 200, 200), Circulo((100, 300), 150))))
    }

    it("simplifica algunos si y otros no") {
      val p = Grupo(
        List(
          ColorDibujable((200, 200, 200),
            ColorDibujable((200, 200, 200), Rectangulo((100, 100), (100, 100)))
          ),
          ColorDibujable((250, 200, 200), Circulo((100, 300), 150)),
          TraslacionDibujable(0, 0, Circulo((10, 10), 10)),
          RotacionDibujable(45, RotacionDibujable(45, Triangulo((20, 20), (30, 30), (50, 50))))
        )
      )
      simplificarAst(p) shouldBe Grupo(
        List(
          ColorDibujable((200, 200, 200), Rectangulo((100, 100), (100, 100))),
          ColorDibujable((250, 200, 200), Circulo((100, 300), 150)),
          Circulo((10, 10), 10),
          RotacionDibujable(90, Triangulo((20, 20), (30, 30), (50, 50)))
        )
      )
    }

  }

  describe("test de integracion"){
    it("ejemplo del tp"){
      val resultado =  ParserDibujable.map(dibujable => simplificarAst(dibujable))("grupo(\n" +
        "color[200,200,200](color[200,200,200](rectangulo[100 @ 100, 100 @ 100])),\n" +
        "color[250,200,200](circulo[100 @ 300, 150]),\n" +
        "traslacion[0,0](circulo[10 @ 10, 10]),\n" +
        "rotacion[45](rotacion[45](triangulo[20 @ 20, 30 @ 30, 50 @ 50]))\n" +
        ")").getOrElse(Resultado(Triangulo((0,0),(0,0),(0,0)),""))
      resultado.elementoParseado shouldBe Grupo(
        List(
          ColorDibujable((200, 200, 200), Rectangulo((100, 100), (100, 100))),
          ColorDibujable((250, 200, 200), Circulo((100, 300), 150)),
          Circulo((10, 10), 10),
          RotacionDibujable(90, Triangulo((20, 20), (30, 30), (50, 50)))
        )
      )
    }

  }

}
