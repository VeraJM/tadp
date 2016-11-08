package org.tadp.dragonball

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.tadp.dragonball.dragonBall._

class DragonBallTest extends FreeSpec with Matchers with BeforeAndAfter {
  
  val movUsarArmaFilosa = usarItem(ArmaFilosa) _
  val movCargarki = cargarKi _
  val movDejarseFajar = dejarseFajar _
  
  val movimientosGoku : List[Movimiento] = List(movDejarseFajar,movCargarki,movUsarArmaFilosa)
  val itemsGoku : List[Item] = List(SemillaDeErmitanio,ArmaFilosa)
  val itemsGohan: List[Item] = List(SemillaDeErmitanio)
  
  val goku = Guerrero("Goku",Saiyajin(), movimientosGoku, ki=900, kiMaximo=2000,inventario = itemsGoku, Normal)
  val vegeta = Guerrero("Vegeta",Saiyajin(), movimientosGoku, ki=2000, kiMaximo=2000,inventario = itemsGoku, Normal)
  val gohan =  Guerrero("Gohan",Saiyajin(cola=false), movimientosGoku, ki=1000, kiMaximo=2000,inventario = itemsGohan, Normal)
  
  "Torneo Artes Marciales" - {
    
    "Peleas entre guerreros" - {
      
      "Goku se deja fajar" in {
        val (nuevoGoku,nuevoVegeta) = goku.hacerMovimiento(dejarseFajar, vegeta)
        nuevoGoku.ki should be(900)
        nuevoVegeta.ki should be(2000)
      }
      
      "Goku sube su ki" in {
        val (nuevoGoku,nuevoVegeta) = goku.hacerMovimiento(cargarKi, vegeta)
        nuevoGoku.ki should be(1000)
      }
    }
    
    "Mejores movimientos" - {
      
      "Mejor movimiento de danio" in {
        val mejor = goku.movimientoMasEfectivoContra(vegeta, mayorDanioAlEnemigo)
        mejor should be(movUsarArmaFilosa)
      }
      
      "Mejor movimiento de ki de atacante" in {
        val mejor = goku.movimientoMasEfectivoContra(vegeta, mayorKi)
        mejor should be(movCargarki)
      }
      
      "Mejor movimiento de diferencia de ki" in {
        val mejor = goku.movimientoMasEfectivoContra(vegeta, diferenciaDeKi)
        mejor should be(movUsarArmaFilosa)
      }
      
    }
    
    "Pelear Round" - {
      
      "Un round de cargar ki atacante saiyajin con cola" in {
        val (nuevoGoku,nuevoVegeta) = goku.pelearRound(movCargarki)(vegeta)
        /* Vegeta tiene un arma filosa, es muy efectiva contra los saiyajins con cola(los deja en 1 de ki) */
        /* Goku tiene cola */ 
        nuevoGoku.ki should be (1)
        nuevoVegeta.ki should be (2000)
      }
      
      "Un round de usar arma filosa contra oponente con cola" in {
        val (nuevoGoku,nuevoVegeta) = goku.pelearRound(movUsarArmaFilosa)(vegeta)
        /* Vegeta tiene cola, las armas filosas lo dejan en 1 de ki */
        nuevoGoku.ki should be (1)
        nuevoVegeta.ki should be (1)
      }
      
      "Un round de usar arma filosa contra oponente sin cola" in {
        /* Gohan es un saiyajin sin cola, recibe tanto daño como el ki de goku */
        /* Gohan contraataca con una espada */
        goku.ki should be (900)
        gohan.ki should be (1000)
        val (nuevoGoku,nuevoGohan) = goku.pelearRound(movUsarArmaFilosa)(gohan)
        nuevoGoku.ki should be (1)
        nuevoGohan.ki should be (100)
      }
      
    }
  }
}
