package dibujadores

import dibujable.*
import parserDibujable.*
import parsers.*
import scalafx.scene.paint.Color
import tadp.drawing.TADPDrawingAdapter

import scala.util.Try

type RGB = (Int, Int, Int)

abstract class Transformacion(dibujable: Dibujable) extends Dibujable { // no se usa para nada ahora, solo semantica
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    dibujable.dibujar(adapter).end()
  }
}

case class ColorDibujable(color: RGB, dibujable: Dibujable) extends Transformacion(dibujable) {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    super.dibujar(adapter.beginColor(Color.rgb(color._1, color._2, color._3)))
  }
}

case class EscalaDibujable(x: Double, y: Double, dibujable: Dibujable) extends Transformacion(dibujable) {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    super.dibujar(adapter.beginScale(x, y))
  }
}

case class RotacionDibujable(grados: Double, dibujable: Dibujable) extends Transformacion(dibujable) {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    super.dibujar(adapter.beginRotate(grados))
  }
}

case class TraslacionDibujable(x: Double, y: Double, dibujable: Dibujable) extends Transformacion(dibujable) {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
    super.dibujar(adapter.beginTranslate(x, y))
  }
}

object parserTransformacion extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] = (parserColor <|> parserEscala <|> parserRotacion <|> parserTraslacion)(cadena)
}

object parserColor extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] =
    (parserNombreColor <>
      ParserDibujable <~ charWhiteSpaceSeparator(')'))
      .map { case (color, dibujable) => ColorDibujable(color, dibujable) }(cadena)
}

object parserNombreColor extends Parser[RGB] {
  def apply(cadena: String): Try[Resultado[RGB]] =
    (string("color[") ~> whiteSpace.* ~>
      ((integer <~ charWhiteSpaceSeparator(',')) <>
        (integer <~ charWhiteSpaceSeparator(',')) <>
        (integer <~ charWhiteSpaceSeparator(']')))
        .map { case ((r, g), b) => (r, g, b) } <~ charWhiteSpaceSeparator('('))(cadena)
}

object parserEscala extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] =
    (parserNombreEscala <>
      ParserDibujable <~ charWhiteSpaceSeparator(')'))
      .map { case ((x, y), dibujable) => EscalaDibujable(x, y, dibujable) }(cadena)
}

object parserNombreEscala extends Parser[Punto] {
  def apply(cadena: String): Try[Resultado[Punto]] =
    ((string("escala[") ~> whiteSpace.* ~>
      (double <~ charWhiteSpaceSeparator(',')) <>
      (double <~ charWhiteSpaceSeparator(']'))) <~ charWhiteSpaceSeparator('('))(cadena)
}

object parserRotacion extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] =
    (parserNombreRotacion <>
      ParserDibujable <~ charWhiteSpaceSeparator(')'))
      .map { case (grados, dibujable) => RotacionDibujable(grados, dibujable) }(cadena)
}

object parserNombreRotacion extends Parser[Double] {
  def apply(cadena: String): Try[Resultado[Double]] =
    (string("rotacion[") ~> whiteSpace.* ~> double <~ charWhiteSpaceSeparator(']') <~ charWhiteSpaceSeparator('('))(cadena)
}

object parserTraslacion extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] =
    (parserNombreTraslacion <>
      ParserDibujable <~ charWhiteSpaceSeparator(')'))
      .map { case ((deltaX, deltaY), dibujable) => TraslacionDibujable(deltaX, deltaY, dibujable) }(cadena)
}

object parserNombreTraslacion extends Parser[Punto] {
  def apply(cadena: String): Try[Resultado[(Double, Double)]] =
    ((string("traslacion[") ~> whiteSpace.* ~>
      (double <~ charWhiteSpaceSeparator(',')) <>
      (double <~ charWhiteSpaceSeparator(']'))) <~ charWhiteSpaceSeparator('('))(cadena)
}