package org.tadp.dragonball

object DragonBall{
  
  case class Guerrero(especie: Especie,
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
    
  }
  
  def dejarseFajar(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    (atacante, oponente)
  }
  
  def cargarKi(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    atacante.especie match{
      case Androide(_) => (atacante,oponente)
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
    // POR AHORA NO SE CONSUME LA MUNICION
    item match{
        case ArmaRoma => oponente.especie match{
          case Androide(_) => (atacante, oponente)
          case _ => if (oponente.ki < 300) (atacante,oponente.estado(Inconsciente)) else (atacante, oponente)
          }
        case ArmaFilosa => 
          val oponenteAtacado = oponente.ki(-1*atacante.ki)
          oponenteAtacado.especie match{
          case Saiyajin(_,transformacion) => 
            val nuevoSaiyajin = oponente
            transformacion match{
              // Los monos pierden la cola y la transformacion de mono, ademas quedan inconscientes
              case Some(Mono(_)) => (atacante,oponenteAtacado.estado(Inconsciente).especie(Saiyajin(false,None)))
              case _ => (atacante, oponenteAtacado.especie(Saiyajin(false,transformacion)).kiTo(1))
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
      case _ => (monstruo, comida)
    }
  }
  
  def comerTodo(monstruo : Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
    var monstruoLleno = monstruo.especie(monstruo.especie.asInstanceOf[Monstruo]
                                                  .movimientos(comida.habilidades))
                                                  
    (monstruoLleno, comida)
  }

  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado
  
  type Movimiento = (Guerrero,Guerrero) => (Guerrero,Guerrero)
  
  sealed trait Especie
  case object Humano extends Especie
  case class Saiyajin(cola: Boolean = true, transformacion: Option[Transformacion] = None ) extends Especie{
    def cola(sana : Boolean) : Saiyajin = copy(cola = sana)
  }
  case object Androide extends Especie
  case object Namekusein extends Especie
  case class Monstruo(formaDigestion: (Guerrero ,Guerrero) => (Guerrero, Guerrero), 
                      movimientosAprendidosPorDigestion : List[Movimiento]) extends Especie{
    def comer(atacante : Guerrero, oponente : Guerrero) : (Guerrero, Guerrero) = {
      var guerreros = this.formaDigestion(atacante, oponente);
      
      (guerreros._1, guerreros._2.kiTo(0).estado(Muerto))
    }
    
    def movimientos(nuevosMovimientos :List[Movimiento]) : Especie = {
      this.copy(movimientosAprendidosPorDigestion = nuevosMovimientos)
    }
  }
  
  sealed trait Transformacion
  case class SSJ(nivel: Int) extends Transformacion
  case class Mono(estadoAnterior: Guerrero) extends Transformacion
  
  sealed trait Item
  abstract class Arma() extends Item
  case object SemillaDeErmitanio extends Item
  
  case object ArmaFilosa extends Arma
  case object ArmaRoma extends Arma
  case class ArmaDeFuego(municion : Int) extends Arma
  
}
