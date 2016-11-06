package org.tadp.dragonball

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.tadp.dragonball.DragonBall._

class DragonBallTest extends FreeSpec with Matchers with BeforeAndAfter {
  
  val movimientosGoku : List[Movimiento] = List(dejarseFajar)
  val itemsGoku : List[Item] = List(SemillaDeErmitanio)
  
  val goku = Guerrero(Saiyajin(), movimientosGoku, ki=900, kiMaximo=1000,inventario = itemsGoku, Normal)
  val vegeta = Guerrero(Saiyajin(), movimientosGoku, ki=1000, kiMaximo=1000,inventario = itemsGoku, Normal)
  
  "Torneo Artes Marciales" - {
    
    "Peleas entre guerreros" - {
      
      "Goku se deja fajar" in {
        val (nuevoGoku,nuevoVegeta) = goku.hacerMovimiento(dejarseFajar, vegeta)
        nuevoGoku.ki should be(900)
        nuevoVegeta.ki should be(1000)
      }
      
      "Goku sube su ki" in {
        val (nuevoGoku,nuevoVegeta) = goku.hacerMovimiento(cargarKi, vegeta)
        nuevoGoku.ki should be(1000)
      }
      
      
    }
  }
}