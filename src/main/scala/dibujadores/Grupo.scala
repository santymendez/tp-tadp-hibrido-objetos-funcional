package dibujadores

import dibujable.Dibujable
import parsers._
import tadp.drawing.TADPDrawingAdapter
import parserDibujable.*

import scala.util.Try

case class Grupo(contenido: List[Dibujable]) extends Dibujable {
  override def dibujar(adapter: TADPDrawingAdapter): TADPDrawingAdapter = contenido.foldLeft(adapter)((acc, d) => d.dibujar(acc))
}

object parserGrupo extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] =
    (string("grupo(") ~> whiteSpace.* ~> ParserDibujable.sepBy(charWhiteSpaceSeparator(',')) <~ charWhiteSpaceSeparator(')'))
      .map(listadoDibujables => Grupo(listadoDibujables))(cadena)
}