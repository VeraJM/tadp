package org.tadp.dragonball

package object dragonBall{
  
  //##########################################
  //               Guerrero
  //##########################################
  
  case class Guerrero(nombre: String,
                      especie: Especie,
                      habilidades: List[Movimiento],
                      ki: Int = 0,
                      kiMaximo: Int,
                      inventario: List[Item],
                      estado: Estado = Normal) {
  
    /* "SETTERS" */
    def ki (delta: Int) : Guerrero = copy(ki = ki + delta min kiMaximo)
    def kiTo(value: Int) : Guerrero = copy(ki=value)
    def estado(nuevoEstado: Estado) : Guerrero = copy (estado=nuevoEstado)
    def kiMaximo(nuevoMaximo: Int) : Guerrero = copy (kiMaximo=nuevoMaximo)
    def especie(especie: Especie) : Guerrero = copy (especie=especie)
    
    def multiplicarKiMaximoEn(valor :Int) :Guerrero = kiMaximo( kiMaximo * valor)
    
    // Magia para crear la lista de inventario con un item modificado, ej: la arma de fuego con una bala menos
    def item(itemViejo: Item, itemNuevo: Item) : Guerrero = copy(inventario = inventario.updated(inventario.indexOf(itemViejo), itemNuevo))
    
    def esDeEspecie(especie : Especie) = {this.especie == especie}
    
    def hacerMovimiento(movimiento : Movimiento, oponente : Guerrero) : (Guerrero,Guerrero) = {
      estado match {
        // En caso de estar muerto no pasa nada
        // Aca es donde podria tirar error por estar muerto y querer hacer algo
        case Muerto => (this,oponente)
        // Hay movimientos que se puede hacer por mas que esten inconcientes. Ej: comer semillas
        case _ => movimiento(this,oponente)
      }
    }
    
    def movimientoMasEfectivoContra(oponente: Guerrero, criterio: Criterio) = {
      val resultados = for {
        movimiento <- this.habilidades
        (nuevoAtacante,nuevoOponente) = movimiento(this,oponente)
        valor = criterio(nuevoAtacante, nuevoOponente)
      }yield (movimiento,valor)
      
      resultados.sortBy(_._2).reverse.head._1
    }
    
    def pelearRound(movimiento: Movimiento)(oponente: Guerrero) = {
      val (nuevoAtacante,nuevoOponente) = this.hacerMovimiento(movimiento, oponente)
      turnoOponente(nuevoAtacante,nuevoOponente)
    }
    
    def planDeAtaqueContra(oponente :Guerrero, cantidadRounds : Int)(criterio :Criterio) :List[Movimiento] = {
      
      //si no hay rounds por pelear no se devuelve ningun movimiento
      if(cantidadRounds < 1) return List()
      
      //seleccionar mejor movimiento
      var mejorMovimiento = this.movimientoMasEfectivoContra(oponente, criterio)
      var planDeAtaque = List(mejorMovimiento)
      //simular el round
      var peleadores = this.pelearRound(mejorMovimiento)(oponente)
      //volver a repetir con cantidad de rounds - 1 y con los peleadores luego de pelear el round
      planDeAtaque :: (peleadores._1.planDeAtaqueContra(peleadores._2, cantidadRounds - 1)(criterio))
      
      planDeAtaque   
     }
    
    def pelearContra(oponente :Guerrero)(planDeAtaque : List[Movimiento]) : ResultadoPelea = {
      val estadoFinalPeleadores : (Guerrero, Guerrero) = null //TODO aca hay q ejecutar el plan de pelea y obtener los peleadores finales
      
      estadoFinalPeleadores._1.estado match{
        case Muerto => tenemosUnGanador(estadoFinalPeleadores._2)
        case _ => estadoFinalPeleadores._2.estado match{
          case Muerto => tenemosUnGanador(estadoFinalPeleadores._1)
          case _ => sinGanador(estadoFinalPeleadores._1, estadoFinalPeleadores._2)
        }
      }
    }
 
    
  } 
  //aca termina el guerrero
  
  def turnoOponente(atacante: Guerrero, oponente:Guerrero) = {
    val (nuevoOponente,nuevoAtacante) = 
      oponente.hacerMovimiento(oponente.movimientoMasEfectivoContra(atacante, diferenciaDeKi), atacante)
    (nuevoAtacante,nuevoOponente)
  }
  
  //##########################################
  //      Criterios de movimientos
  //##########################################
  
  def mayorDanioAlEnemigo(atacante: Guerrero, oponente: Guerrero) : Int = {
    oponente.kiMaximo - oponente.ki
  }
  
  def mayorKi(atacante: Guerrero, oponente: Guerrero) : Int = {
    atacante.ki
  }
  
  def diferenciaDeKi(atacante: Guerrero, oponente: Guerrero) : Int = {
    atacante.ki.compare(oponente.ki)
  }
 
  //##########################################
  //      Movimientos de pelea
  //##########################################  
  def dejarseFajar(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    (atacante, oponente)
  }
  
  def cargarKi(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    atacante.especie match{
      case Androide => (atacante,oponente)
      case Saiyajin (_,transformacion) => transformacion match {
        case Some(SSJ(nivel)) => (atacante.ki(150*nivel),oponente)
        case _ => (atacante.ki(100),oponente)
      }
      case _ => (atacante.ki(100),oponente)
    }
  }
  
  def usarItem(item : Item)(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    // Podria crear los items como objetos que hagan apply con la tupla de guerreros
    // Queda medio incomodo tener que llevar el conteo de las balas de las armas teniendo que 
    // reflejarlo en la lista de items del atacante que devuelvo.
    // La municion seria un atributo en el objeto arma de fuego
    item match{
        case ArmaRoma => oponente.especie match{
          case Androide => (atacante, oponente)
          case _ => if (oponente.ki < 300) (atacante,oponente.estado(Inconsciente)) else (atacante, oponente)
          }
        case ArmaFilosa =>
          val oponenteAtacado = oponente.ki(-1*atacante.ki)
          
          oponenteAtacado.especie match{
            case Saiyajin(true,transformacion) =>
                val nuevoSaiyajin = oponenteAtacado.especie(Saiyajin(cola=false,transformacion)).kiTo(1)
                transformacion match{
                  // Los monos pierden la cola y la transformacion de mono, ademas quedan inconscientes
                  case Some(Mono(anterior)) => (atacante,anterior.especie(Saiyajin(false,None)))
                  case _ => (atacante, nuevoSaiyajin)
                  }
            case _ => (atacante,oponenteAtacado)
        }
        case ArmaDeFuego(municion) => 
          val armaUsada = ArmaDeFuego(municion - 1)
          val atacanteNuevo = atacante.item(item, armaUsada)
          
          oponente.especie match {
          case Humano => (atacanteNuevo, oponente.ki(-20))
          case Namekusein => oponente.estado match {
            case Inconsciente => (atacanteNuevo,oponente.ki(-10))
            case _ => (atacanteNuevo, oponente)
          }
          case _ => (atacanteNuevo, oponente)
        }
      case SemillaDeErmitanio => (atacante.ki(atacante.kiMaximo),oponente)
    }
  }
  
  def comerOponente(atacante :Guerrero, oponente :Guerrero) :(Guerrero, Guerrero) ={
    atacante.especie match {
      case Monstruo(_,_) if atacante.ki > oponente.ki =>  atacante.especie.asInstanceOf[Monstruo].comer(atacante, oponente)
      case _ => (atacante, oponente)
    }
  }
  
  def convertirseEnMono(atacante :Guerrero, oponente:Guerrero) : (Guerrero, Guerrero) = {
    var atacanteTransformado = atacante.especie match{
      case Saiyajin(_,Some(Mono(_))) => throw new RuntimeException("El mono no se puede convertir en mono")
      case Saiyajin(true, _) if atacante.inventario.contains(FotoDeLaLuna) => atacante.kiMaximo(atacante.kiMaximo*3)
                                                                                      .kiTo(atacante.kiMaximo*3)
                                                                                      .especie(atacante.especie.asInstanceOf[Saiyajin].transformacion(Mono(atacante)))
      case _ => atacante
    }
    
    return (atacanteTransformado, oponente)
  }
  
  //##########################################
  //  FORMAS DE DIGESTION DE LOS MONSTRUOS
  //##########################################
  
  def soloAndroides(monstruo :Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
    comida.especie match{
      case Androide => var movimientosDeVictima = comida.habilidades
                       var movimientosMonstruo = monstruo.especie.asInstanceOf[Monstruo].movimientosAprendidosPorDigestion
                       var monstruoLleno = monstruo.especie(monstruo.especie.asInstanceOf[Monstruo]
                                                                .movimientos(movimientosMonstruo ++ movimientosDeVictima))
                       (monstruoLleno, comida)
      case _ => throw new RuntimeException("El monstruo no puede comer guerreros no androides")
    }
  }
  
  def comerTodo(monstruo : Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
    var monstruoLleno = monstruo.especie(monstruo.especie.asInstanceOf[Monstruo]
                                                  .movimientos(comida.habilidades))
                                                  
    (monstruoLleno, comida)
  }
  
  
  //-------------------------
  // MOVIMIENTOS DEL SAIYAJIN
  //------------------------
  def convertirseEnSSJ(atacante :Guerrero, oponente :Guerrero): (Guerrero, Guerrero)={
   
    var nuevoGuerrero : Guerrero = atacante
    atacante.especie match{
   
     case Saiyajin(cola,transformacion) if puedeConvertirseEnSJJ(atacante)  =>
       
       transformacion match {
          //si no tenia, se transforma en nivel 1
          case None => 
            nuevoGuerrero = atacante.multiplicarKiMaximoEn(5)
                                    .especie( Saiyajin(cola, Some(SSJ(1) ) ))
          
          case Some(SSJ(nivel)) =>
            nuevoGuerrero = atacante.multiplicarKiMaximoEn( (nivel +1)*5  )
                                           .especie(Saiyajin(cola,Some(SSJ(nivel+1))))
         }
        (nuevoGuerrero , oponente )
        
     case _ => (atacante, oponente)
   }  
  }

  //retorna si el guerrero(saiyajin) puede avanzar un nivel se SSJ
  def puedeConvertirseEnSJJ(saiyajin :Guerrero): Boolean = {
  return saiyajin.ki >= (saiyajin.kiMaximo /2)
  }
  
  
  //retorna el nivel del saiyajin, si esta en estado normal, es 0
  def nivelDelSaiyajin(saiyajin :Guerrero):Int = {
  
     saiyajin.especie.asInstanceOf[Saiyajin].nivelSaiyajin
    
  }

  
  
  
  
  
  
  
  
  
  
  //##########################################
  //            Estados
  //##########################################

  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado
  
  type Movimiento = (Guerrero,Guerrero) => (Guerrero,Guerrero)
  type Criterio = (Guerrero,Guerrero) => Int
  
  
  //##########################################
  //              Especies
  //##########################################
  
  sealed trait Especie

  case object Humano extends Especie

  case class Saiyajin(cola: Boolean = true, transformacion: Option[Transformacion] = None ) extends Especie{
    def cola(sanidad : Boolean) : Saiyajin = copy(cola = sanidad)
    
    def transformacion(nuevaTransformacion : Transformacion) :Saiyajin = copy(transformacion = Some(nuevaTransformacion))
    
    def nivelSaiyajin :Int = {
      transformacion match {
        case Some(SSJ(nivel)) => nivel
        case None => 0
      }
    }
  }
  
  case object Androide extends Especie;

  case object Namekusein extends Especie;
  
  case class Monstruo(formaDigestion: (Guerrero ,Guerrero) => (Guerrero, Guerrero), 
                      movimientosAprendidosPorDigestion : List[Movimiento]) extends Especie{
    def comer(atacante : Guerrero, oponente : Guerrero) : (Guerrero, Guerrero) = {
      var guerreros = this.formaDigestion(atacante, oponente);
      
      (guerreros._1, guerreros._2.kiTo(0).estado(Muerto))
    }
    
    def movimientos(nuevosMovimientos :List[Movimiento]) : Especie = {
      this.copy(movimientosAprendidosPorDigestion = nuevosMovimientos)
    }
  };
  
  //##########################################
  //          Transformaciones
  //##########################################
  
  sealed trait Transformacion
  case class SSJ(nivel: Int) extends Transformacion
  case class Mono(estadoAnterior: Guerrero) extends Transformacion
  
  
  //##########################################
  //              Items
  //##########################################
  sealed trait Item
  abstract class Arma() extends Item
  case object SemillaDeErmitanio extends Item
  
  case object ArmaFilosa extends Arma
  case object ArmaRoma extends Arma
  case class ArmaDeFuego(municion : Int) extends Arma
  
  case object FotoDeLaLuna extends Item
  
  
  //##########################################
  //        Resultados de pelea
  //##########################################
  
  trait ResultadoPelea
  case class tenemosUnGanador(ganador :Guerrero) extends ResultadoPelea
  case class sinGanador(atacante :Guerrero, oponente :Guerrero) extends ResultadoPelea
}
