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
  val movimientosAndroide15 : List[Movimiento] = List(dejarseFajar)
  val movimientosAndroide16 : List[Movimiento] = List(usarItem(SemillaDeErmitanio))
  val movimientosMonstruo : List[Movimiento] = List(comerOponente)
  
  val itemsGoku : List[Item] = List(SemillaDeErmitanio,ArmaFilosa)
  val itemsGohan: List[Item] = List(SemillaDeErmitanio)
  
  val goku = Guerrero("Goku",Saiyajin(), movimientosGoku, ki=900, kiMaximo=2000,inventario = itemsGoku, Normal)
  val vegeta = Guerrero("Vegeta",Saiyajin(), movimientosGoku, ki=2000, kiMaximo=2000,inventario = itemsGoku, Normal)
  val gohan =  Guerrero("Gohan",Saiyajin(cola=false), movimientosGoku, ki=1000, kiMaximo=2000,inventario = itemsGohan, Normal)
  val androide15 = Guerrero("Androide 15", Androide,movimientosAndroide15, ki = 100, kiMaximo = 200, inventario = List(), Normal)
  val androide16 = Guerrero("Androide 16", Androide,movimientosAndroide16, ki = 100, kiMaximo = 200, inventario = List(), Normal)
  val cell = Guerrero("Cell", Monstruo(soloAndroides, List()),movimientosMonstruo, ki = 1500, kiMaximo = 2000, inventario = List(), Normal)
  val majinBuu = Guerrero("Majin Buu", Monstruo(comerTodo, List()),movimientosMonstruo, ki = 1500, kiMaximo = 2000, inventario = List(), Normal)
 

  val kamehameha = ondaDeEnergia(400) _
  val bigBang = ondaDeEnergia(300) _
  val masenko = ondaDeEnergia(200) _
  
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
      
      "El androide no sube su ki cuando lo intenta" in {        
        val resultado = androide16.hacerMovimiento(cargarKi,cell)
        resultado._1.ki shouldBe (androide16.ki)
      }
      
        "Gohan lanza energia y su ki disminuye" in {
         
        val resultado = gohan.hacerMovimiento(masenko,goku)
        resultado._1.ki shouldBe (800)
       }
      
       "Vegeta le lanza energia a Goku y le saca el doble" in {
         
        val resultado = vegeta.hacerMovimiento(bigBang,goku)
        resultado._2.ki shouldBe (300)
       }
      
      
      "Goku le lanza energia a majin bu y solo le saca la mitad" in {
        
        val resultado = goku.hacerMovimiento(kamehameha,majinBuu)
         resultado._2.ki shouldBe (1300)
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
  
   
  "Pruebas comida" - {
    
    "Al comer un guerrero este muere"  in {
      val peleadores = cell.hacerMovimiento(comerOponente, androide15)
      
      peleadores._2.estado should be(Muerto)
    }
    
    "Cell come androides y aprende sus poderes" in {
      val movimientosAprendidos : List[Movimiento] = movimientosAndroide15 ++ movimientosAndroide16
      
      var peleadores = cell.hacerMovimiento(comerOponente, androide15)
      peleadores = peleadores._1.hacerMovimiento(comerOponente, androide16)
      
      peleadores._1.especie.asInstanceOf[Monstruo].movimientosAprendidosPorDigestion should be(movimientosAprendidos)
    }
    
    "Cell no puede comer guerreros no androides" in {
      a [RuntimeException] should be thrownBy {
      cell.hacerMovimiento(comerOponente, goku)
      } 
    }
    
    "Majin Buu come todo tipo de guerreros y aprende los poderes solo del ultimo" in {
      var peleadores = majinBuu.hacerMovimiento(comerOponente, goku)
      peleadores = peleadores._1.hacerMovimiento(comerOponente, androide15)
      
      peleadores._1.especie.asInstanceOf[Monstruo].movimientosAprendidosPorDigestion should be(movimientosAndroide15)
    }
  }
  
  
   "Pruebas del Saiyajin" - {
    
     "Goku no tiene ki suficiente y no se transforma en SSJ" in {
       //ki de goku = 900 < ki maximo/2 = 100 => no se transforma 
      val resultado = goku.hacerMovimiento(convertirseEnSSJ,vegeta)
      
      nivelDelSaiyajin(resultado._1) shouldBe(0)
      }
     
     
        "Goku se transforma en SSJ y no pierde su cola" in {
       //ki de goku = 900 < ki maximo/2 = 1000 => no se transforma 
        val gokuEnojado = goku.kiTo(1500)
      
        val resultado = gokuEnojado.hacerMovimiento(convertirseEnSSJ,vegeta)
      
        resultado._1.especie.asInstanceOf[Saiyajin].cola shouldBe(gokuEnojado.especie.asInstanceOf[Saiyajin].cola)
      }
     
     
      "Goku se transforma en SSJ y multiplica su ki maximo por 5" in {
       //ki de goku = 900 < ki maximo/2 = 1000 => no se transforma 
        val gokuEnojado = goku.kiTo(1500)
      
        val resultado = gokuEnojado.hacerMovimiento(convertirseEnSSJ,vegeta)
      
        resultado._1.kiMaximo shouldBe(10000)
      }
    
     "Goku normal se transforma en SSJ" in {
       
      val gokuEnojado = goku.kiTo(1500)
      
      val resultado = gokuEnojado.hacerMovimiento(convertirseEnSSJ,vegeta)
      
      nivelDelSaiyajin(resultado._1) shouldBe(1)
      }
    
      "Goku se transforma en SSJ y no tiene efecto en el rival" in {
         
      val resultado = goku.hacerMovimiento(convertirseEnSSJ,vegeta)
      
      resultado._2 should be(vegeta)
      }
      
      "Goku SSJ1 se transforma en SSJ2" in {
        
      val gokuSSJ = goku.especie( Saiyajin(false, Some(SSJ(1)))).kiMaximo(2000).ki(2000)
      val resultado = gokuSSJ.hacerMovimiento(convertirseEnSSJ,vegeta)
      
      nivelDelSaiyajin(resultado._1) shouldBe(2)
      }
      
      "Goku SSJ1 se transforma en SSJ2 y multiplica su nivel en 5 por el nivel" in {
        
       val gokuSSJ =  goku.especie( Saiyajin(false, Some(SSJ(1)))).kiMaximo(10000).ki(7000)

        val resultado = gokuSSJ.hacerMovimiento(convertirseEnSSJ,vegeta)
      
        resultado._1.kiMaximo shouldBe(50000)
      }
      
      
       "Goku SSJ pierde su estado y recupera su ki maximo inicial" in {
        
       val gokuSSJ =  goku.especie( Saiyajin(false, Some(SSJ(1)))).kiMaximo(10000)
        val resultado = gokuSSJ.perdeSSJ
      
        resultado.kiMaximo shouldBe(2000)
      }
       
       
        "Goku SSJ2 se destransforma y recupera su ki maximo inicial" in {
        
        val gokuSSJ2 =  goku.especie( Saiyajin(false, Some(SSJ(2)))).kiMaximo(50000)

        val resultado = gokuSSJ2.perdeSSJ
      
        resultado.kiMaximo shouldBe(2000)
      }
        
        
        "Goku SSJ se transforma en mono y pierde estado ssj" in {
          //ki max goku 2000 => ki max en mono = 6000
           val otrosItems : List[Item] = List(SemillaDeErmitanio,ArmaFilosa, FotoDeLaLuna)
           
            val gokuSSJ = Guerrero("Goku",Saiyajin(true, Some(SSJ(1))),
                                      movimientosGoku, ki=7000,
                                      kiMaximo=10000,
                                      inventario = otrosItems, Normal)
          
           val resultado = gokuSSJ.hacerMovimiento(convertirseEnMono,vegeta)

          resultado._1.kiMaximo shouldBe(6000)
           
           
       }
        
  }

}