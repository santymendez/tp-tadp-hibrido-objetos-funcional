package dibujable

import parsers.*
import tadp.drawing.TADPDrawingAdapter

import scala.util.Try

type Punto = (Double, Double)

trait Dibujable {
  def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter
}

object coordenadas extends Parser[Punto] {
  def apply(cadena: String): Try[Resultado[Punto]] = {
    ((double <~ charWhiteSpaceSeparator('@')) <> double)(cadena)
  }
}