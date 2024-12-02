import parsers._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}
import dibujable.*
import dibujadores._

class DibujadoresSpec extends AnyFunSpec with Matchers {
  describe("coordenadas"){
    it("una coordenada"){
      val p = coordenadas("1 @ 2")
      p shouldBe Success(Resultado((1, 2), ""))
    }

    it("una coordenada sin espacios") {
      val p = coordenadas("1@2")
      p shouldBe Success(Resultado((1, 2), ""))
    }

    it("una coordenada con espacios de mas") {
      val p = coordenadas("1@  2")
      p shouldBe Success(Resultado((1, 2), ""))
    }
  }

  describe("triangulo") {
    it("un triangulo") {
      val t = parserTriangulo("triangulo[1 @ 1, 3 @ 4, 5 @ 6]")
      t shouldBe Success(Resultado(Triangulo((1, 1), (3, 4), (5, 6)) ,""))
    }
  }

  describe("rectangulo") {
    it("un rectangulo") {
      val t = parserRectangulo("rectangulo[1   @ 1,   3 @ 4]")
      t shouldBe Success(Resultado(Rectangulo((1, 1), (3, 4)), ""))
    }
  }

  describe("circulo") {
    it("un circulo") {
      val t = parserCirculo("circulo[1 @ 1, 34]")
      t shouldBe Success(Resultado(Circulo((1, 1), 34), ""))
    }
  }

  describe("grupo") {
    it("un grupo") {
      val t = parserGrupo("grupo(\ntriangulo[200 @ 50, 101 @ 335, 299 @ 335],\ncirculo[200 @ 350, 100]\n)")
      t shouldBe Success(Resultado(Grupo(List(Triangulo((200,50),(101,335),(299,335)),Circulo((200,350),100))), ""))
    }

    it("grupos anidados") {
      val t = parserGrupo("grupo(\ngrupo(\ntriangulo[250 @ 150, 150 @ 300, 350 @ 300],\ntriangulo[150 @ 300, 50 @ 450, 250 @ 450],\ntriangulo[350 @ 300, 250 @ 450, 450 @ 450]\n),\n    grupo(\n        rectangulo[460 @ 90, 470 @ 100],\n        rectangulo[430 @ 210, 500 @ 220],\n        rectangulo[430 @ 210, 440 @ 230],\n        rectangulo[490 @ 210, 500 @ 230],\n        rectangulo[450 @ 100, 480 @ 260]\n    )\n)")
      t shouldBe Success(Resultado(Grupo(List(Grupo(List(Triangulo((250,150),(150,300),(350,300)),Triangulo((150,300),(50,450),(250,450)),Triangulo((350,300),(250,450),(450,450)))),Grupo(List(Rectangulo((460,90),(470,100)),Rectangulo((430,210),(500,220)),Rectangulo((430,210),(440,230)),Rectangulo((490,210),(500,230)),Rectangulo((450,100),(480,260)))))), ""))
    }
  }

  describe("color") {
    it("ejemplo del tp") {
      val t = parserColor("color[60, 150, 200](\n    grupo(\n   \t triangulo[200 @ 50, 101 @ 335, 299 @ 335],\n   \t circulo[200 @ 350, 100]\n    )\n)")
      t shouldBe Success(Resultado(ColorDibujable((60,150,200),Grupo(List(Triangulo((200,50),(101,335),(299,335)),Circulo((200,350),100)))), ""))
    }
  }


  describe("escala") {
    it("ejemplo del tp") {
      val t = parserEscala("escala[6, 15](\n\tcirculo[0 @ 5, 10]\n)")
      t shouldBe Success(Resultado(EscalaDibujable(6, 15, Circulo((0, 5), 10)), ""))
    }
  }


  describe("rotacion") {
    it("ejemplo del tp") {
      val t = parserRotacion("rotacion[300](\n\trotacion[10](\n\t\trectangulo[100 @ 200, 300 @ 400]\n\t)\n)")
      t shouldBe Success(Resultado(RotacionDibujable(300,RotacionDibujable(10,Rectangulo((100,200),(300,400)))), ""))
    }
  }

  describe("traslacion") {
    it("ejemplo del tp") {
      val t = parserTraslacion("traslacion[100, 5](\n\ttraslacion[20, 10](\n\t\tcirculo[0 @ 5, 10]\n)\n)")
      t shouldBe Success(Resultado(TraslacionDibujable(100,5, TraslacionDibujable(20,10, Circulo((0, 5), 10))), ""))
    }
  }

}
