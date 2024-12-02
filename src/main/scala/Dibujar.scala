import parserDibujable.*
import parsers.*
import scalafx.scene.paint.Color
import tadp.drawing.TADPDrawingAdapter

object Dibujar extends App {
  TADPDrawingAdapter.forScreen { adapter =>
    // Aca es donde iria su codigo que USA el adapter

    // Ejemplo de como dibujar usando directamente la API de dibujo
    adapter
      .circle((300, 100), 50) // un ojo
      .circle((500, 100), 50) // otro ojo
      .beginColor(Color.rgb(255, 0, 0))
      .triangle((400, 200), (350, 250), (450, 250)) // nariz
      .end()
      .rectangle((300, 350), (500, 370)) //
      .rectangle((300, 310), (320, 350)) // La boca
      .rectangle((480, 310), (500, 350)) //
  }
}

object GUIDeTextoADibujo extends App {
  TADPDrawingAdapter.forInteractiveScreen { (texto, adapter) =>
    // Aca es donde iria su codigo que USA el adapter, y convierte de String a envios de mensaje
    // al adapter

    // EJEMPLO de como usar el texto para dibujar (Ustedes van a tener que usar el parser)
    //    var i: Int = 0
    //    texto.lines().forEach { line =>
    //      val offset = 40 * i
    //      line match
    //        case "triangulo" =>
    //          adapter.triangle((20 + offset, 40), (30 + offset, 20), (40 + offset, 40))
    //          i += 1
    //        case "rectangulo" =>
    //          adapter.rectangle((20 + offset, 20), (50 + offset, 40))
    //          i += 1
    //        case "cuadrado" =>
    //          adapter.rectangle((20 + offset, 20), (40 + offset, 40))
    //          i += 1
    //        case "circulo" =>
    //          adapter.circle((30 + offset, 30), 10)
    //          i += 1
    //        case "rojo" => adapter.beginColor(Color.Red)
    //        case "verde" => adapter.beginColor(Color.Green)
    //        case "azul" => adapter.beginColor(Color.Blue)
    //        case _ => ()
    //    }

    //    texto.lines().forEach { line =>
    //      parserTriangulo(line) match
    //        case Success(Resultado(t, _)) => t.dibujar(adapter)
    //        case Failure(_) => parserRectangulo(line) match
    //          case Success(Resultado(r,_)) => r.dibujar(adapter)
    //          case Failure(_) => parserCirculo(line) match
    //            case Success(Resultado(c,_)) => c.dibujar(adapter)
    //            case Failure(_) => ???
    //
    //    }
    val parserPrograma = (whiteSpace.* ~> ParserDibujable <~ whiteSpace.*).map(dibujable => simplificarAst(dibujable))
    parserPrograma(texto).get.elementoParseado.dibujar(adapter)
    
  }
}

object GenerarImagen extends App {
  // La imagen se genera en la carpeta out/
  TADPDrawingAdapter.forImage("imagen.png") { adapter =>
    adapter
      .circle((300, 100), 50) // un ojo
      .circle((500, 100), 50) // otro ojo
      .beginColor(Color.rgb(255, 0, 0))
      .triangle((400, 200), (350, 250), (450, 250)) // nariz
      .end()
      .rectangle((300, 350), (500, 370)) //
      .rectangle((300, 310), (320, 350)) // La boca
      .rectangle((480, 310), (500, 350)) //
  }
}

