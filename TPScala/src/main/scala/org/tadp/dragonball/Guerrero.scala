package org.tadp.dragonball

package object dragonBall{
  
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
    
    //def planDeAtaqueContra
    
  } 
  //aca termina el guerrero
  
  def turnoOponente(atacante: Guerrero, oponente:Guerrero) = {
    val (nuevoOponente,nuevoAtacante) = 
      oponente.hacerMovimiento(oponente.movimientoMasEfectivoContra(atacante, diferenciaDeKi), atacante)
    (nuevoAtacante,nuevoOponente)
  }
  
  /******** CRITERIOS ***********/
  
  def mayorDanioAlEnemigo(atacante: Guerrero, oponente: Guerrero) : Int = {
    oponente.kiMaximo - oponente.ki
  }
  
  def mayorKi(atacante: Guerrero, oponente: Guerrero) : Int = {
    atacante.ki
  }
  
  def diferenciaDeKi(atacante: Guerrero, oponente: Guerrero) : Int = {
    atacante.ki.compare(oponente.ki)
  }
 
  /************* MOVIMIENTOS ***************/   
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
    item match{
        case ArmaRoma => oponente.especie match{
          case Androide(_) => (atacante, oponente)
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

  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado
  
  type Movimiento = (Guerrero,Guerrero) => (Guerrero,Guerrero)
  type Criterio = (Guerrero,Guerrero) => Int
  
  sealed trait Especie
  case object Humano extends Especie
  case class Saiyajin(cola: Boolean = true, transformacion: Option[Transformacion] = None ) extends Especie{
    def cola(sanidad : Boolean) : Saiyajin = copy(cola = sanidad)
  }
  case class Androide(bateria: Int) extends Especie
  case object Namekusein extends Especie
  case object Monstruo extends Especie
  
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
