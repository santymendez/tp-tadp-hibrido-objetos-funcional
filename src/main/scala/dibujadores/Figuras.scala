package dibujadores

import dibujable._
import parsers._
import tadp.drawing.TADPDrawingAdapter

import scala.util.Try

case class Triangulo(punto1: Punto, punto2: Punto, punto3: Punto) extends Dibujable {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    adapter.triangle(punto1, punto2, punto3)
  }
}

case class Rectangulo(punto1: Punto, punto2: Punto) extends Dibujable {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    adapter.rectangle(punto1, punto2)
  }
}

case class Circulo(punto1: Punto, radio: Double) extends Dibujable {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    adapter.circle(punto1, radio)
  }
}

object parserTriangulo extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] = {
    ((string("triangulo[") ~> (coordenadas <~ charWhiteSpaceSeparator(','))) <>
      (coordenadas <~ charWhiteSpaceSeparator(',')) <>
      (coordenadas <~ char(']')))
      .map { case ((p1, p2), p3) => Triangulo(p1, p2, p3) }(cadena)

  }
}

object parserRectangulo extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] = {
    ((string("rectangulo[") ~> (coordenadas <~ charWhiteSpaceSeparator(','))) <>
      (coordenadas <~ char(']')))
      .map { (p1, p2) => Rectangulo(p1, p2) }(cadena)
  }
}

object parserCirculo extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] = {
    ((string("circulo[") ~> (coordenadas <~ charWhiteSpaceSeparator(','))) <>
      (double <~ char(']')))
      .map { (p1, radio) => Circulo(p1, radio) }(cadena)
  }
}

object parserFigura extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] = (parserTriangulo <|> parserRectangulo <|> parserCirculo)(cadena)
}