package org.tadp.dragonball

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.tadp.dragonball.dragonBall._
import org.tadp.dragonball.Movimientos._
import org.tadp.dragonball.Criterios._
import org.tadp.dragonball.Especies._
import org.tadp.dragonball.Items._

class DragonBallTest extends FreeSpec with Matchers with BeforeAndAfter {

  //MOVIMIENTOS
  val movUsarArmaFilosa = usarItem(ArmaFilosa)
  val movCargarki = cargarKi 
  val movDejarseFajar = dejarseFajar

  val movimientosGoku: List[Movimiento] = List(movDejarseFajar, movCargarki, movUsarArmaFilosa)
  val movimientosAndroide15: List[Movimiento] = List(dejarseFajar)
  val movimientosAndroide16: List[Movimiento] = List(usarItem(SemillaDeErmitanio),explotar)
  val movimientosMonstruo: List[Movimiento] = List(comerOponente,explotar)
  val movimientosHumano: List[Movimiento] = List(muchosGolpesNinja)
  val movimientosNamekusein: List[Movimiento] = List(muchosGolpesNinja)
  
  val kamehameha = ondaDeEnergia(400) 
  val bigBang = ondaDeEnergia(300) 
  val masenko = ondaDeEnergia(200) 
    
  val curarse :Movimiento = (atacante,oponente) => (atacante.recuperaKiMaximo, oponente)
  val curaOponente :Movimiento = (atacante,oponente) => (atacante, oponente.recuperaKiMaximo)
  
  // Otra opcion es: 
  /* val curarse = magia((atacante,oponente) => (atacante.recuperaKiMaximo, oponente)) _
   * val curaOponente = magia((atacante,oponente) => (atacante, oponente.recuperaKiMaximo))_
   * 
   * y utilizar asi: atacante.hacerMovimiento(curarese, oponente)
   * */
    
  //LISTA DE ITEMS
  val itemsGoku: List[Item] = List(SemillaDeErmitanio, ArmaFilosa)
  val itemsGohan: List[Item] = List(SemillaDeErmitanio)
  val sieteEsferas: List[Item] = List.fill(7)(EsferaDragon)
  
  //GUERREROS
  val goku = Guerrero("Goku", Saiyajin(), movimientosGoku, ki = 900, kiMaximo = 2000, inventario = itemsGoku, estado = Normal)
  val vegeta = Guerrero("Vegeta", Saiyajin(), movimientosGoku, ki = 2000,potenciadorGenkidama = 0, kiMaximo = 2000, inventario = itemsGoku, Normal)
  val gohan = Guerrero("Gohan", Saiyajin(cola = false), movimientosGoku, ki = 1200,potenciadorGenkidama = 0, kiMaximo = 2000, inventario = itemsGohan, Normal)
  val androide15 = Guerrero("Androide 15", Androide, movimientosAndroide15, ki = 100,potenciadorGenkidama = 0, kiMaximo = 200, inventario = List(), Normal)
  val androide16 = Guerrero("Androide 16", Androide, movimientosAndroide16, ki = 100,potenciadorGenkidama = 0, kiMaximo = 200, inventario = List(), Normal)
  val cell = Guerrero("Cell", Monstruo(soloAndroides, List()), movimientosMonstruo, ki = 1500,potenciadorGenkidama = 0, kiMaximo = 2000, inventario = List(), Normal)
  val majinBuu = Guerrero("Majin Buu", Monstruo(comerTodo, List()), movimientosMonstruo, ki = 1500,potenciadorGenkidama = 0, kiMaximo = 2000, inventario = List(), Normal)
  val mrSatan = Guerrero("Satan", Humano, movimientosHumano, ki = 80,potenciadorGenkidama = 0, kiMaximo = 250, inventario = List(), Normal)
  val piccolo = Guerrero("Piccoro", Namekusein, movimientosNamekusein, ki = 600,potenciadorGenkidama = 0, kiMaximo = 1300, inventario = List(), Normal)
  val krilin = Guerrero("Krilin", Humano, movimientosHumano, ki = 250, potenciadorGenkidama = 0, kiMaximo = 900, inventario = sieteEsferas, Normal)
  
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      ASSERT PARA LAS MONADAS DEL PUNTO 4
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  def assertOnTerminada(resultado:Pelea,ganador:Guerrero) = resultado match{
    case pelea:PeleaTerminada => pelea.get.shouldBe(ganador)
    case _ => fail
  }
  
  def assertOnEnCurso(resultado:Pelea,atc:Guerrero,opo:Guerrero) = resultado match{
    case pelea:PeleaEnCurso => pelea.get.shouldBe((atc,opo)) 
    case _ => fail
  }

  "Torneo Artes Marciales" - {

    "Peleas entre guerreros" - {

      "Goku se deja fajar" in {
        val (nuevoGoku, nuevoVegeta) = goku.hacerMovimiento(dejarseFajar, vegeta)
        nuevoGoku.ki should be(900)
        nuevoVegeta.ki should be(2000)
      }

      "Goku sube su ki" in {
        val (nuevoGoku, nuevoVegeta) = goku.hacerMovimiento(cargarKi, vegeta)
        nuevoGoku.ki should be(1000)
      }

      "El androide no sube su ki cuando lo intenta" in {
        val resultado = androide16.hacerMovimiento(cargarKi, cell)
        resultado._1.ki shouldBe (androide16.ki)
      }

      "Gohan lanza energia y su ki disminuye" in {

        val resultado = gohan.hacerMovimiento(masenko, goku)
        resultado._1.ki shouldBe (1000)
      }

      "Vegeta le lanza energia a Goku y le saca el doble" in {

        val resultado = vegeta.hacerMovimiento(bigBang, goku)
        resultado._2.ki shouldBe (300)
      }

      "Goku le lanza energia a majin bu y solo le saca la mitad" in {

        val resultado = goku.hacerMovimiento(kamehameha, majinBuu)
        resultado._2.ki shouldBe (1300)
      }

      "Goku le lanza una genkidama a Vegeta" in {

        //se deja fajar 2 veces, por lo que el daño tiene que ser 10^2
        val dejarse1 = goku.hacerMovimiento(dejarseFajar, vegeta)
        val dejarse2 = dejarse1._1.hacerMovimiento(dejarseFajar, dejarse1._2)
        val resultadoGenkidama = dejarse2._1.hacerMovimiento(genkidama, dejarse2._2)

        resultadoGenkidama._2.ki shouldBe (vegeta.ki - 100)
      }

      "Goku carga genkidama y la reinicia al usar kamehameha" in {

        val dejarse1 = goku.hacerMovimiento(dejarseFajar, vegeta)
        val dejarse2 = dejarse1._1.hacerMovimiento(dejarseFajar, dejarse1._2)
        val despuesDeKamehameja = dejarse2._1.hacerMovimiento(kamehameha, dejarse2._2)

        despuesDeKamehameja._1.potenciadorGenkidama shouldBe (0)
      }

      "Goku carga genkidama y la reinicia al quedar inconciente" in {

        val dejarse1 = goku.hacerMovimiento(dejarseFajar, vegeta)
        val dejarse2 = dejarse1._1.hacerMovimiento(dejarseFajar, dejarse1._2)
        
        val gokuInconsciente = dejarse2._1.poneteInconsciente

        gokuInconsciente.potenciadorGenkidama shouldBe (0)
      }
      
       "Mr Satan hace muchos golpes ninja contra androide16 y pierde 10 puntos por ser Humano vs Androide" in {

        val atacar = mrSatan.hacerMovimiento(muchosGolpesNinja, androide16)

        atacar._1.ki shouldBe (70)
      }

        "Goku hace muchos golpes ninja contra Cell y pierde 20 de ki por tener menos ki" in {

        val atacar = goku.hacerMovimiento(muchosGolpesNinja, cell)

        atacar._1.ki shouldBe (880)
      }
        
      "Vegeta hace muchos golpes ninja contra Cell y Cell pierde 20 de ki por tener menos ki" in {

        val atacar = vegeta.hacerMovimiento(muchosGolpesNinja, cell)

        atacar._2.ki shouldBe (1480)
      }
      
      "Cell explota y muere" in {

        val atacar = cell.hacerMovimiento(explotar, vegeta)

        atacar._1.estado shouldBe (Muerto)
      }
      
       "Cell explota y pierde todo su ki" in {

        val atacar = cell.hacerMovimiento(explotar, vegeta)

        atacar._1.ki shouldBe (0)
      }
       
      "Cell con 200 de ki explota y hace doble de ki por ser monstruo a vegeta" in {

        val cell2 = cell.kiTo(200)
        val atacar = cell2.hacerMovimiento(explotar, vegeta)

        atacar._2.ki shouldBe (1600)
      }
      
     "Androide16 con 200 de ki explota y hace triple de ki por ser androide a vegeta" in {

        val androide16B = androide16.kiTo(200)
        val atacar = androide16B.hacerMovimiento(explotar, vegeta)

        atacar._2.ki shouldBe (1400)
      }
     
     "Cell explota y daña a piccoro casi matandolo dejandolo en 1 de ki (por ser namekusein)" in {

        val atacar = cell.hacerMovimiento(explotar, piccolo)

        atacar._2.ki shouldBe (1)
      }
   
      "Piccoro intenta explotar y dispara un error" in {

        a[InvalidAttackException] should be thrownBy {
        piccolo.hacerMovimiento(explotar, goku)
        }
      }
      
      "Majin Bu se cura usando un pase de magia" in {
        
        val resultado = majinBuu.hacerMovimiento(magia(curarse), goku)
        
        resultado._1.ki shouldBe(2000)
        
      }
      
      "Piccoro cura al otro usando un pase de magia" in {
        
        val resultado = majinBuu.hacerMovimiento(magia(curaOponente), gohan)
        
        resultado._2.ki shouldBe(2000)        
      }
      
      "Mr Satan realiza un pase pero no pasa nada  por no tener las esferas del dragon" in{
        val resultado = mrSatan.hacerMovimiento(magia(curarse), majinBuu)
        
        resultado._1.ki shouldBe(80) 
      }
      
      
       "Krilin restaura su ki al pedir un deseo con las esferas del dragon" in{
      
        val resultado = krilin.hacerMovimiento(magia(curarse), majinBuu)
        
        resultado._1.ki shouldBe(900) 
      }
       
       "Krilin pide un deseo y pierde las esferas del dragon" in{
      
        val resultado = krilin.hacerMovimiento(magia(curarse), majinBuu)
        
        tieneEsferasDelDragon(resultado._1) shouldBe(false)
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
        val (nuevoGoku, nuevoVegeta) = goku.pelearRound(movCargarki)(vegeta)
        /* Vegeta tiene un arma filosa, es muy efectiva contra los saiyajins con cola(los deja en 1 de ki) */
        /* Goku tiene cola */
        nuevoGoku.ki should be(1)
        nuevoVegeta.ki should be(2000)
      }

      "Un round de usar arma filosa contra oponente con cola" in {
        val (nuevoGoku, nuevoVegeta) = goku.pelearRound(movUsarArmaFilosa)(vegeta)
        /* Vegeta tiene cola, las armas filosas lo dejan en 1 de ki */
        nuevoGoku.ki should be(1)
        nuevoVegeta.ki should be(1)
      }

      "Un round de usar arma filosa contra oponente sin cola" in {
        /* Gohan es un saiyajin sin cola, recibe tanto daño como el ki de goku */
        /* Gohan contraataca con una espada */
        goku.ki should be(900)
        gohan.ki should be(1200)
        val (nuevoGoku, nuevoGohan) = goku.pelearRound(movUsarArmaFilosa)(gohan)
        nuevoGoku.ki should be(1)
        nuevoGohan.ki should be(300)
      }

    }
  }

  "Pruebas comida" - {

    "Al comer un guerrero este muere" in {
      val peleadores = cell.hacerMovimiento(comerOponente, androide15)

      peleadores._2.estado should be(Muerto)
    }

    "Cell come androides y aprende sus poderes" in {
      val movimientosAprendidos: List[Movimiento] = movimientosAndroide15 ++ movimientosAndroide16

      var peleadores = cell.hacerMovimiento(comerOponente, androide15)
      peleadores = peleadores._1.hacerMovimiento(comerOponente, androide16)

      peleadores._1.especie.asInstanceOf[Monstruo].movimientosAprendidosPorDigestion should be(movimientosAprendidos)
    }

    "Cell no puede comer guerreros no androides" in {
      a[RuntimeException] should be thrownBy {
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
      val resultado = goku.hacerMovimiento(convertirseEnSSJ, vegeta)

      nivelDelSaiyajin(resultado._1) shouldBe (0)
    }

    "Goku se transforma en SSJ y no pierde su cola" in {
      //ki de goku = 900 < ki maximo/2 = 1000 => no se transforma 
      val gokuEnojado = goku.kiTo(1500)

      val resultado = gokuEnojado.hacerMovimiento(convertirseEnSSJ, vegeta)

      resultado._1.especie.asInstanceOf[Saiyajin].cola shouldBe (gokuEnojado.especie.asInstanceOf[Saiyajin].cola)
    }

    "Goku se transforma en SSJ y multiplica su ki maximo por 5" in {
      //ki de goku = 900 < ki maximo/2 = 1000 => no se transforma 
      val gokuEnojado = goku.kiTo(1500)

      val resultado = gokuEnojado.hacerMovimiento(convertirseEnSSJ, vegeta)

      resultado._1.kiMaximo shouldBe (10000)
    }

    "Goku normal se transforma en SSJ" in {

      val gokuEnojado = goku.kiTo(1500)

      val resultado = gokuEnojado.hacerMovimiento(convertirseEnSSJ, vegeta)

      nivelDelSaiyajin(resultado._1) shouldBe (1)
    }

    "Goku se transforma en SSJ y no tiene efecto en el rival" in {

      val resultado = goku.hacerMovimiento(convertirseEnSSJ, vegeta)

      resultado._2 should be(vegeta)
    }

    "Goku SSJ1 se transforma en SSJ2" in {

      val gokuSSJ = goku.especie(Saiyajin(false, Some(SSJ(1)))).kiMaximo(2000).ki(2000)
      val resultado = gokuSSJ.hacerMovimiento(convertirseEnSSJ, vegeta)

      nivelDelSaiyajin(resultado._1) shouldBe (2)
    }

    "Goku SSJ1 se transforma en SSJ2 y multiplica su nivel en 5 por el nivel" in {

      val gokuSSJ = goku.especie(Saiyajin(false, Some(SSJ(1)))).kiMaximo(10000).ki(7000)

      val resultado = gokuSSJ.hacerMovimiento(convertirseEnSSJ, vegeta)

      resultado._1.kiMaximo shouldBe (50000)
    }

    "Goku SSJ pierde su estado y recupera su ki maximo inicial" in {

      val gokuSSJ = goku.especie(Saiyajin(false, Some(SSJ(1)))).kiMaximo(10000)
      val resultado = gokuSSJ.perdeSSJ

      resultado.kiMaximo shouldBe (2000)
    }

    "Goku SSJ2 se destransforma y recupera su ki maximo inicial" in {

      val gokuSSJ2 = goku.especie(Saiyajin(false, Some(SSJ(2)))).kiMaximo(50000)

      val resultado = gokuSSJ2.perdeSSJ

      resultado.kiMaximo shouldBe (2000)
     }

    "Goku SSJ se transforma en mono y pierde estado ssj" in {
      //ki max goku 2000 => ki max en mono = 6000
      val otrosItems: List[Item] = List(SemillaDeErmitanio, ArmaFilosa, FotoDeLaLuna)

      val gokuSSJ = Guerrero("Goku", Saiyajin(true, Some(SSJ(1))),
        movimientosGoku, ki = 7000,potenciadorGenkidama = 0,
        kiMaximo = 10000,
        inventario = otrosItems, Normal)

      val resultado = gokuSSJ.hacerMovimiento(convertirseEnMono, vegeta)

      resultado._1.kiMaximo shouldBe (6000)

      }
    
      "Goku SSJ se vuelve inconsciente y pierde su estado" in {

        val gokuSSJ = Guerrero("Goku", Saiyajin(false, Some(SSJ(1))),
                                movimientosGoku, ki = 7000,potenciadorGenkidama = 0,
                                kiMaximo = 10000,
                                inventario = itemsGoku, Normal)

       nivelDelSaiyajin(gokuSSJ.poneteInconsciente) shouldBe (0)
      }

  }
  "Planes de ataque" - {
    
    "Plan de ataque Goku vs Vegeta" in {
      
      val planDeGoku = goku.planDeAtaqueContra(vegeta, 3)(diferenciaDeKi)
//      planDeGoku.foreach { x => Console.println(x) }
//      Console.println(planDeGoku.length)
      /* Goku planea primero atacar, recuperarse por la represalia y volver a atacar */
      planDeGoku.length shouldBe (3)
      planDeGoku(0) shouldBe (movUsarArmaFilosa)
      planDeGoku(1) shouldBe (movCargarki)
      planDeGoku(2) shouldBe (movUsarArmaFilosa)
      
      
    }
    
  }
  
  "Peleas hasta el final" - {
    
    "Goku pelea hasta el final con Vegeta" in {
      
      val planDeAtaque = List(movUsarArmaFilosa,movCargarki,movCargarki)
      val estadoFinal: Pelea = goku.pelearContra(vegeta)(planDeAtaque)
      
      val gokuFinal = Guerrero("Goku",Saiyajin(false,None),movimientosGoku,100,0,2000,List(SemillaDeErmitanio, ArmaFilosa),Normal)
      val vegetaFinal = Guerrero("Vegeta",Saiyajin(false,None),movimientosGoku,101,0,2000,List(SemillaDeErmitanio, ArmaFilosa),Normal)

      assertOnEnCurso(estadoFinal,gokuFinal,vegetaFinal)
      
    }
    
    
    "Goku pelea hasta el final con Gohan" in {
      
      /* Gohan no tiene cola, los ataques de Arma Filosa no son tan efectivos, goku pierde el combate */
      val planDeAtaque = List(movUsarArmaFilosa,movCargarki,movUsarArmaFilosa)
      val estadoFinal: Pelea = goku.pelearContra(gohan)(planDeAtaque)
      
      val gohanFinal = Guerrero("Gohan",Saiyajin(false,None),movimientosGoku,300,0,2000,List(SemillaDeErmitanio),Normal)
      assertOnTerminada(estadoFinal,gohanFinal)
      
      
    }
    
    "Goku pelea hasta el final con Majin Boo" in {
      
      val planDeAtaque = List(movUsarArmaFilosa,movCargarki,movUsarArmaFilosa)
      val estadoFinal: Pelea = goku.pelearContra(majinBuu)(planDeAtaque)
      /* TODO: Bug, Majin Boo explota y se declara ganador a goku porque su ponente esta muerto, a pesar que el tambien murio e
       * explosion. Majin Boo DEBERIA comerse a goku y no explotar */
      val gokuFinal = Guerrero("Goku",Saiyajin(true,None),movimientosGoku,0,0,2000,List(SemillaDeErmitanio, ArmaFilosa),Muerto)
      
      assertOnTerminada(estadoFinal,gokuFinal)
    }
  }

}