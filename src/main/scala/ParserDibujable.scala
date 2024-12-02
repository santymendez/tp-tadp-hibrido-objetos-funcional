package parserDibujable

import dibujable.Dibujable
import dibujadores.{parserFigura, parserGrupo, parserTransformacion}
import parsers.{Parser, Resultado}

import scala.util.Try

object ParserDibujable extends Parser[Dibujable] {
  def apply(cadena: String): Try[Resultado[Dibujable]] = (parserFigura <|> parserGrupo <|> parserTransformacion)(cadena)
}
