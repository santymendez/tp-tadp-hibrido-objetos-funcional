package parsers

import scala.annotation.targetName
import scala.util.Try

abstract class Parser[T] extends (String => Try[Resultado[T]]) {

  def apply(cadena: String): Try[Resultado[T]]

  @targetName("<|>")
  def <|>(otroParser: Parser[T]): Parser[T] = cadena => this (cadena).recoverWith { _ => otroParser(cadena) }

  @targetName("<>")
  def <>[X](otroParser: Parser[X]): Parser[(T, X)] = cadena => {
    for {
      r <- this (cadena) // Ejecuta el primer parser
      r2 <- otroParser(r.cadenaRestante) // Ejecuta el segundo parser en el resto de la cadena
    } yield Resultado((r.elementoParseado, r2.elementoParseado), r2.cadenaRestante)
  }

  @targetName("~>")
  def ~>[X](otroParser: Parser[X]): Parser[X] = (this <> otroParser).map((x, y) => y)

  @targetName("<~")
  def <~[X](otroParser: Parser[X]): Parser[T] = (this <> otroParser).map((x, y) => x)

  def sepBy[X](parserSeparador: Parser[X]): Parser[List[T]] = cadena => (this <> (parserSeparador ~> this).*)(cadena).map {
    r => Resultado(r.elementoParseado._1 :: r.elementoParseado._2, r.cadenaRestante)
  }

  //Operaciones
  def satisfies(condicion: T => Boolean): Parser[T] = cadena =>
    this (cadena).filter(r => condicion(r.elementoParseado))

  def opt: Parser[Option[T]] = cadena => this.map {
    Option(_)
  }(cadena).recoverWith { _ => Try(Resultado(None, cadena)) }

  @targetName("*")
  def * : Parser[List[T]] = ClausuraKleene(this)

  @targetName("+")
  def + : Parser[List[T]] = cadena => (this <> this.*).map { r => List(r._1) ++ r._2 }(cadena)

  def map[X](funcion: T => X): Parser[X] = cadena => this (cadena).map { r => Resultado(funcion(r.elementoParseado), r.cadenaRestante) }

  def flatMap[U](f: T => Parser[U]): Parser[U] = cadena => this (cadena).flatMap { resultado =>
    f(resultado.elementoParseado)(resultado.cadenaRestante)
  }
}



// ---------------------- PARSERS ---------------------- //

case class Resultado[+T](elementoParseado: T, cadenaRestante: String)

object anyChar extends Parser[Char] {

  def apply(cadena: String): Try[Resultado[Char]] =
    Try(
      cadena match {
        case cad if cad.isEmpty => throw new RuntimeException("La cadena está vacia")
        case cad => Resultado(cad.head, cad.tail)
      }
    )

}

case class char(caracter: Char) extends Parser[Char] {
  def apply(cadena: String): Try[Resultado[Char]] = anyChar.satisfies(c => c == caracter)(cadena)
}

object digit extends Parser[Char] {
  def apply(cadena: String): Try[Resultado[Char]] = anyChar.satisfies(d => d.isDigit)(cadena)
}

case class string(cadenaFiltro: String) extends Parser[String] {

  def apply(cadenaAParsear: String): Try[Resultado[String]] =
    Try(
      cadenaAParsear match {
        case c if c startsWith cadenaFiltro => Resultado(cadenaFiltro, cadenaAParsear.drop(cadenaFiltro.length));
        case _ => throw new RuntimeException("La cadena obtenida no coincide con la esperada");
      }
    )

}

object integer extends Parser[Int] {

  def apply(cadena: String): Try[Resultado[Int]] = {

    val optionalSign = char('-').opt // Permite el signo '-' de forma opcional
    val digits = digit.+ // Asegura que haya al menos un dígito

    for {
      sign <- optionalSign(cadena) // Intenta parsear el signo opcional
      digitResult <- digits(sign.cadenaRestante) // Aplica el parser de dígitos obligatorios
      numeroStr = sign.elementoParseado.getOrElse("").toString + digitResult.elementoParseado.mkString // Combina el signo con los dígitos
    } yield Resultado(numeroStr.toInt, digitResult.cadenaRestante)
  }
}

object double extends Parser[Double] {

  def apply(cadena: String): Try[Resultado[Double]] =
    // resultadoCorrecto.getElementoParseado(1) => Parte decimal
    // resultadoCorrecto.getElementoParseado => [elem1, elem2, ...] - Tomo solo los primeros dos

    val combinator = integer <> (char('.') ~> digit.+).opt
    //123 concat ".34252"

    combinator(cadena).map {
      r => Resultado(s"${r.elementoParseado._1}.${r.elementoParseado._2.getOrElse(List('0')).mkString}".toDouble, r.cadenaRestante)
    }
}

object whiteSpace extends Parser[Char] {
  def apply(cadena: String): Try[Resultado[Char]] = anyChar.satisfies(c => c.isWhitespace)(cadena)
}

case class charWhiteSpaceSeparator(caracter: Char) extends Parser[Char] {
  def apply(cadena: String): Try[Resultado[Char]] = ((whiteSpace.* ~> char(caracter)) <~ whiteSpace.*)(cadena)
}

// ---------------------- OPERACIONES ---------------------- //

case class ClausuraKleene[T](parserOriginal: Parser[T]) extends Parser[List[T]] {

  private def parsear(resultadoParcial: Resultado[List[T]]): Resultado[List[T]] =
    parserOriginal(resultadoParcial.cadenaRestante)
      // Exito => agrega el nuevo valor parseado a la lista de elementos y llama recursivamente a parsear con el nuevo Resultado
      .map { r => parsear(Resultado(resultadoParcial.elementoParseado ++ List(r.elementoParseado), r.cadenaRestante)) }
      // Falla => Retorna el Resultado parcial actual, indicando que no se puede parsear más.
      .recoverWith { _ => Try(resultadoParcial) }.get

  // Arranca con una lista vacia como "Resultado(List(), cadena)"
  def apply(cadena: String): Try[Resultado[List[T]]] = Try(parsear(Resultado(List(), cadena)))

}