import dibujable._
import dibujadores.*


def simplificarAst(ast: Dibujable): Dibujable = {
  ast match
    case Grupo(ColorComun(color, listaDibujable)) =>
      val simplificado = simplificarAst(Grupo(listaDibujable))
      ColorDibujable(color, simplificado)

    case Grupo(RotacionComun(grados, listaDibujable)) =>
      val simplificado = simplificarAst(Grupo(listaDibujable))
      RotacionDibujable(grados, simplificado)

    case Grupo(EscalaComun((fx, fy), listaDibujable)) =>
      val simplificado = simplificarAst(Grupo(listaDibujable))
      EscalaDibujable(fx, fy, simplificado)

    case Grupo(TraslacionComun((x, y), listaDibujable)) =>
      val simplificado = simplificarAst(Grupo(listaDibujable))
      TraslacionDibujable(x, y, simplificado)

    case Grupo(listaDibujable) =>
      Grupo(listaDibujable.map(simplificarAst))

    case ColorDibujable(_, ColorDibujable(inner,dibujable)) =>
      ColorDibujable(inner, simplificarAst(dibujable))

    case EscalaDibujable(x, y, dibujable) =>
      simplificarEscala(EscalaDibujable(x, y, simplificarAst(dibujable)))

    case TraslacionDibujable(x, y, dibujable) =>
      simplificarTraslacion(TraslacionDibujable(x, y, simplificarAst(dibujable)))

    case RotacionDibujable(grados, dibujable) =>
      simplificarRotacion(RotacionDibujable(grados, simplificarAst(dibujable)))

    case _ => ast
}

//def simplificarColor(color: ColorDibujable): Dibujable = {
//  color match
//    case ColorDibujable(_, ColorDibujable(inner,dibujable)) => simplificarColor(ColorDibujable(inner,dibujable))
//    case _ => color
//}
def simplificarRotacion(rotacion: RotacionDibujable): Dibujable = {
  rotacion match
    case RotacionDibujable(0,dibujable) => dibujable
    case RotacionDibujable(grados,RotacionDibujable(otrosGrados,dibujable)) => simplificarRotacion(RotacionDibujable(grados + otrosGrados,dibujable))
    case _ => rotacion
}

def simplificarTraslacion(traslacionDibujable: TraslacionDibujable): Dibujable = {
  traslacionDibujable match
    case TraslacionDibujable(0, 0, dibujable) => dibujable
    case TraslacionDibujable(x1, y1, TraslacionDibujable(x2, y2, dibujable)) => simplificarTraslacion(TraslacionDibujable(x1 + x2, y1 + y2, dibujable))
    case _ => traslacionDibujable
}

def simplificarEscala(escalaDibujable: EscalaDibujable): Dibujable = {
  escalaDibujable match
    case EscalaDibujable(1, 1, dibujable) => dibujable
    case EscalaDibujable(a1,b1,EscalaDibujable(a2,b2,dibujable)) => simplificarEscala(EscalaDibujable(a1*a2,b1*b2,dibujable))
    case _ => escalaDibujable
}


// el extractor es el unapply de alguna de las transformacioens por ejemplo ColorComun

object TransformacionComun {
  def unapply[Atributos](hijos: List[Dibujable], extractor: Dibujable => Option[(Atributos, Dibujable)]): Option[(Atributos, List[Dibujable])] = {
    // los NONE no quedan en la lista flatmapeada
    val transformaciones = hijos.flatMap(extractor)

    // Si todos los hijos comparten la misma transformación y no están vacíos, la extraemos
    if (transformaciones.size == hijos.size && transformaciones.nonEmpty && transformaciones.map(_._1).distinct.size == 1) {
      val (atributoComun, _) = transformaciones.head
      // Extraemos solo los elementos transformados para simplificar la estructura
      Some((atributoComun, transformaciones.map(_._2)))
    } else {
      None
    }
  }
}

object ColorComun {
  def unapply(hijos: List[Dibujable]): Option[(RGB, List[Dibujable])] =
    TransformacionComun.unapply(hijos, {
      case ColorDibujable(c, elem) => Some((c, elem))
      case _ => None
    })
}

object RotacionComun {
  def unapply(hijos: List[Dibujable]): Option[(Double, List[Dibujable])] =
    TransformacionComun.unapply(hijos, {
      case RotacionDibujable(g, elem) => Some((g, elem))
      case _ => None
    })
}

object EscalaComun {
  def unapply(hijos: List[Dibujable]): Option[(Punto, List[Dibujable])] =
    TransformacionComun.unapply(hijos, {
      case EscalaDibujable(fx, fy, elem) => Some(((fx, fy), elem))
      case _ => None
    })
}

object TraslacionComun {
  def unapply(hijos: List[Dibujable]): Option[(Punto, List[Dibujable])] =
    TransformacionComun.unapply(hijos, {
      case TraslacionDibujable(dx, dy, elem) => Some((dx, dy), elem)
      case _ => None
    })
}