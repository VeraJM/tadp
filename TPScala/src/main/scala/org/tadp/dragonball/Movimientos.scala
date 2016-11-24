package org.tadp.dragonball

import org.tadp.dragonball.dragonBall._
import org.tadp.dragonball.Especies._
import org.tadp.dragonball.Items._

object Movimientos {
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      Movimientos de pelea
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def dejarseFajar(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    val atacanteNuevo =  atacante.aumentarPotenciador
    (atacanteNuevo, oponente)
  }
  
  def cargarKi(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    val atacanteNuevo : Guerrero = atacante.perderPotenciador
    atacante.especie match{
      case Androide => (atacanteNuevo,oponente)
      case Saiyajin (_,transformacion) => transformacion match {
        case Some(SSJ(nivel)) => (atacanteNuevo.ki(150*nivel),oponente)
        case _ => (atacanteNuevo.ki(100),oponente)
      }
      case _ => (atacanteNuevo.ki(100),oponente)
    }
  }
  
  def usarItem(item : Item)(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    // Podria crear los items como objetos que hagan apply con la tupla de guerreros
    // Queda medio incomodo tener que llevar el conteo de las balas de las armas teniendo que 
    // reflejarlo en la lista de items del atacante que devuelvo.
    // La municion seria un atributo en el objeto arma de fuego
    val atacanteSinPotenciador : Guerrero = atacante.perderPotenciador
    item match{
        case ArmaRoma => oponente.especie match{
          
          case Androide => (atacanteSinPotenciador, oponente)
          case _ => if (oponente.ki < 300) (atacanteSinPotenciador,oponente.estado(Inconsciente))
                    else (atacanteSinPotenciador, oponente)
          
          }
        case ArmaFilosa =>
          val oponenteAtacado = oponente.ki(-1*atacanteSinPotenciador.ki)
          
          oponenteAtacado.especie match{
            
            case Saiyajin(true,transformacion) =>
                val nuevoSaiyajin = oponenteAtacado.especie(Saiyajin(cola=false,transformacion)).kiTo(1)
                
                transformacion match{
                  // Los monos pierden la cola y la transformacion de mono, ademas quedan inconscientes
                  case Some(Mono(anterior)) => (atacanteSinPotenciador,anterior.especie(Saiyajin(false,None)))
                  case _ => (atacanteSinPotenciador, nuevoSaiyajin)
                  
                  }
            case _ => (atacanteSinPotenciador,oponenteAtacado)
        }
        case ArmaDeFuego(municion) => 
          val armaUsada = ArmaDeFuego(municion - 1)
          val atacanteNuevo = atacanteSinPotenciador.item(item, armaUsada)
          
          oponente.especie match {
          case Humano => (atacanteNuevo, oponente.ki(-20))
          case Namekusein => oponente.estado match {
            case Inconsciente => (atacanteNuevo,oponente.ki(-10))
            case _ => (atacanteNuevo, oponente)
          }
          case _ => (atacanteNuevo, oponente)
        }
      case SemillaDeErmitanio => (atacanteSinPotenciador.ki(atacanteSinPotenciador.kiMaximo),oponente)
      
      case _ => (atacante,oponente)
    }
  }
  
  def comerOponente(atacante :Guerrero, oponente :Guerrero) :(Guerrero, Guerrero) ={
    val atacanteNuevo : Guerrero = atacante.perderPotenciador
    
    /* Si no puede comerselo por no tener suficiente ki o no ser monstruo no pasa nada */
    /* Podriamos tirar una exception cuando no es monstruo, pero por interpretacion del enunciado no lo hacemos */
    atacanteNuevo.especie match {
      case esp:Monstruo => if (atacanteNuevo.ki > oponente.ki) esp.comer(atacanteNuevo, oponente) else (atacanteNuevo,oponente)
      case _ => (atacanteNuevo, oponente)
    }
  }
  
  def convertirseEnMono(atacante :Guerrero, oponente:Guerrero) : (Guerrero, Guerrero) = {
    
    val atacanteNuevo : Guerrero = atacante.perderPotenciador
    var atacanteTransformado = atacanteNuevo.especie match{
      
      case Saiyajin(_,Some(Mono(_))) => throw new RuntimeException("El mono no se puede convertir en mono")
      case Saiyajin(true, estado) if atacanteNuevo.inventario.contains(FotoDeLaLuna) =>
        
        estado match { //TODO este choclo se podria separar
          case Some(_) =>atacanteNuevo.perdeSSJ.multiplicarKiMaximoEn(3).recuperaKiMaximo
                                .especie(atacanteNuevo.especie.asInstanceOf[Saiyajin]
                                .transformacion(Mono(atacanteNuevo)))
          
          case None =>  atacanteNuevo.multiplicarKiMaximoEn(3).recuperaKiMaximo
                                .especie(atacanteNuevo.especie.asInstanceOf[Saiyajin]
                                .transformacion(Mono(atacanteNuevo)))
        }
      case _ => atacanteNuevo
    }
    
    return (atacanteTransformado, oponente)
  }

  def fusion(companieroDeFusion: Guerrero)(atacante: Guerrero, oponente: Guerrero): (Guerrero, Guerrero) = {

    val atacanteNuevo = atacante.perderPotenciador

    atacanteNuevo.especie match {
      case Humano | Namekusein | Saiyajin(_,_) =>

        val fusionado = atacanteNuevo.copy(
              especie = Fusion(atacanteNuevo),
              habilidades = atacanteNuevo.habilidades ++ companieroDeFusion.habilidades,
              ki = atacanteNuevo.ki + companieroDeFusion.ki,
              kiMaximo = atacanteNuevo.kiMaximo + companieroDeFusion.kiMaximo)

        (fusionado, oponente)

      case _ =>  (atacanteNuevo, oponente)
    }
  }
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //              ATAQUES
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def muchosGolpesNinja(atacante: Guerrero, oponente: Guerrero) : (Guerrero, Guerrero) = {
    val nuevoAtacante : Guerrero = atacante.perderPotenciador
    nuevoAtacante.especie match{
      case Humano => 
        oponente.especie match {
          case Androide => (nuevoAtacante.ki(-10),oponente)
          case _ => golpeNinjaNormal(nuevoAtacante,oponente)
          }
      case _ =>  golpeNinjaNormal(nuevoAtacante,oponente)
    }
  }

  def golpeNinjaNormal(atacante:Guerrero,oponente:Guerrero) : (Guerrero,Guerrero) = {
    if(atacante.ki > oponente.ki)
          {
            (atacante,oponente.ki(-20))
          } else
          {
            (atacante.ki(-20),oponente)
          }
  }
  
  def explotar(atacante:Guerrero,oponente:Guerrero) : (Guerrero,Guerrero) = {
    //Console.println("EXPLOTEEEEE" + atacante.nombre)
    val nuevoAtacante : Guerrero = atacante.perderPotenciador
    
    val nuevoOponente : Guerrero = nuevoAtacante.especie match{
      case Androide | Monstruo(_,_) => nuevoAtacante.especie match {
          case Androide => oponente.ki(nuevoAtacante.ki * -3)
          case _ => oponente.ki(nuevoAtacante.ki * -2)}
       case _ => throw new InvalidAttackException("Solo pueden explotar los androides y monstruos")
      }

    val finalOpo:Guerrero = nuevoOponente.especie match {
          case Namekusein => if (nuevoOponente.ki == 0) nuevoOponente.kiTo(1).estado(Normal) else nuevoOponente
          case _ => nuevoOponente
        }
       (nuevoAtacante.kiTo(0).morite,finalOpo)
 }
  
  def ondaDeEnergia(kiRequerido :Int)(atacante:Guerrero,oponente:Guerrero) : (Guerrero,Guerrero) = {
    var atacanteNuevo = atacante
    var oponenteNuevo = oponente
    
     if (atacante.ki >= kiRequerido){ 
        atacanteNuevo = atacante.ki(-kiRequerido) 
        
        oponente.especie match {
          case Monstruo(_,_) => oponenteNuevo =  oponente.ki(-kiRequerido/2) 
          case _ => oponenteNuevo =  oponente.ki(- 2*kiRequerido)
        }    
    }
    val atacanteSinPotenciador = atacanteNuevo.perderPotenciador
    (atacanteSinPotenciador, oponenteNuevo)
  }
  
  def genkidama(atacante:Guerrero,oponente:Guerrero) : (Guerrero,Guerrero) = {
    
    var atacanteNuevo = atacante
    var oponenteNuevo = oponente
      
    oponenteNuevo = oponenteNuevo.ki(-Math.pow(10, atacanteNuevo.potenciadorGenkidama).toInt)
    val atacanteSinPotenciador = atacanteNuevo.perderPotenciador
    (atacanteSinPotenciador, oponenteNuevo)
  }
  
  case class Ataque(nombre :String, kiRequerido :Int)
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //              MAGIA
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def magia(pase :Movimiento)(atacante :Guerrero, oponente :Guerrero): (Guerrero,Guerrero) = {
    
    val nuevoAtacante : Guerrero = atacante.perderPotenciador
    
     atacante.especie match {      
      case Namekusein | Monstruo(_,_) => pase(nuevoAtacante,oponente)
      case _  => if (tieneEsferasDelDragon(nuevoAtacante)){  
        
                      pase(perderEsferasDelDragon(nuevoAtacante),oponente)
                  }
                  else {(nuevoAtacante, oponente)}
    }
  } 
  
  def tieneEsferasDelDragon(guerrero: Guerrero) :Boolean = {
    guerrero.inventario.count(e => e.equals(EsferaDragon)).equals(7)
  }
  
  def perderEsferasDelDragon(guerrero :Guerrero) :Guerrero = {
    val lista = guerrero.inventario.filter(e => ! e.equals(EsferaDragon))
    
    guerrero.inventario(lista)
  }
  
}