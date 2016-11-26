package org.tadp.dragonball

import org.tadp.dragonball.dragonBall._
import org.tadp.dragonball.Especies._
import org.tadp.dragonball.Items._
import scala.util._

object Movimientos {

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      Movimientos de pelea
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  type Movimiento = Combatientes => Combatientes

  case object dejarseFajar extends Movimiento {
    def apply(combate: Combatientes) = combate;
  }

  case object cargarKi extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      atacante.especie match {
        case Androide => throw new InvalidAttackException("Los androides no cargan ki")
        case Saiyajin(_, transformacion) => transformacion match {
          case Some(SSJ(nivel)) => (atacante.ki(150 * nivel), oponente)
          case _                => (atacante.ki(100), oponente)
        }
        case _ => (atacante.ki(100), oponente)
      }
    }
  }

  case class usarItem(item: Item) extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      item match {
        case ArmaRoma => oponente.especie match {

          case Androide => (atacante, oponente)
          case _ => if (oponente.ki < 300) (atacante, oponente.estado(Inconsciente))
          else (atacante, oponente)
        }
        case ArmaFilosa =>
          oponente.especie match {
            case Saiyajin(true, transformacion) =>
              val nuevoSaiyajin = oponente.especie(Saiyajin(cola = false, transformacion)).kiTo(1)

              transformacion match {
                // Los monos pierden la cola y la transformacion de mono, ademas quedan inconscientes
                case Some(Mono(anterior)) => (atacante, anterior.especie(Saiyajin(false, None)))
                case _                    => (atacante, nuevoSaiyajin)
              }
            case _ => (atacante, oponente.ki(-1 * atacante.ki))
          }
        case ArmaDeFuego(municion) if municion > 0 =>
          val armaUsada = ArmaDeFuego(municion - 1)
          val atacanteNuevo = atacante.item(item, armaUsada)

          oponente.especie match {
            case Humano => (atacanteNuevo, oponente.ki(-20))
            case Namekusein => oponente.estado match {
              case Inconsciente => (atacanteNuevo, oponente.ki(-10))
              case _            => (atacanteNuevo, oponente)
            }
            case _ => (atacanteNuevo, oponente)
          }
        
        case SemillaDeErmitanio => (atacante.ki(atacante.kiMaximo), oponente)

        case _                  => (atacante, oponente)
      }
    }
    override def toString : String = "UsarItem" + item.toString()
  }

  case object comerOponente extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      /* Si no puede comerselo por no tener suficiente ki o no ser monstruo no pasa nada */
      /* Podriamos tirar una exception cuando no es monstruo, pero por interpretacion del enunciado no lo hacemos */
      atacante.especie match {
        case esp: Monstruo => if (atacante.ki > oponente.ki) esp.comer(atacante, oponente) else (atacante, oponente)
        case _             => (atacante, oponente)
      }
    }
  }

  case object convertirseEnMono extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      val atacanteTransformado = atacante.especie match {

        case Saiyajin(_, Some(Mono(_))) => throw new RuntimeException("El mono no se puede convertir en mono")
        case espSaiyajin @ Saiyajin(true, transformacion) if atacante.inventario.contains(FotoDeLaLuna) =>

          transformacion match { //TODO este choclo se podria separar
            case Some(_) => atacante.perdeSSJ.multiplicarKiMaximoEn(3).recuperaKiMaximo
              .especie(espSaiyajin.transformacion(Mono(atacante)))
            case None => atacante.multiplicarKiMaximoEn(3).recuperaKiMaximo
              .especie(espSaiyajin.transformacion(Mono(atacante)))
          }
        case _ => atacante
      }
      return (atacanteTransformado, oponente)
    }
  }

  case class fusion(companieroDeFusion: Guerrero) extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      atacante.especie match {
        case Humano | Namekusein | Saiyajin(_, _) =>

          val fusionado = atacante.copy(
            especie = Fusion(atacante),
            habilidades = atacante.habilidades ++ companieroDeFusion.habilidades,
            ki = atacante.ki + companieroDeFusion.ki,
            kiMaximo = atacante.kiMaximo + companieroDeFusion.kiMaximo)

          (fusionado, oponente)

        case _ => (atacante, oponente)
      }
    }
  }

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //              ATAQUES
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  case object muchosGolpesNinja extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      atacante.especie match {
        case Humano =>
          oponente.especie match {
            case Androide => (atacante.ki(-10), oponente)
            case _        => golpeNinjaNormal(atacante, oponente)
          }
        case _ => golpeNinjaNormal(atacante, oponente)
      }
    }
  }

  case object golpeNinjaNormal extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      if (atacante.ki > oponente.ki)
        (atacante, oponente.ki(-20))
      else
        (atacante.ki(-20), oponente)
    }
  }

  case object explotar extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      val nuevoOponente: Guerrero = atacante.especie match {
        case Androide | Monstruo(_, _) => atacante.especie match {
          case Androide => oponente.ki(atacante.ki * -3)
          case _        => oponente.ki(atacante.ki * -2)
        }
        case _ => throw new InvalidAttackException("Solo pueden explotar los androides y monstruos")
      }

      val finalOpo: Guerrero = nuevoOponente.especie match {
        case Namekusein => if (nuevoOponente.ki == 0) nuevoOponente.kiTo(1).estado(Normal) else nuevoOponente
        case _          => nuevoOponente
      }
      (atacante.kiTo(0).morite, finalOpo)
    }
  }

  case class ondaDeEnergia(kiRequerido: Int) extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      if (atacante.ki >= kiRequerido) {
        oponente.especie match {
          case Monstruo(_, _) => (atacante.ki(-kiRequerido), oponente.ki(-kiRequerido / 2))
          case _              => (atacante.ki(-kiRequerido), oponente.ki(-2 * kiRequerido))
        }
      } else { (atacante, oponente) }
    }
  }

  case object genkidama extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      val (atacante, oponente) = combate
      (atacante.perderPotenciador, oponente.ki(-Math.pow(10, atacante.potenciadorGenkidama).toInt))
    }
  }

  case class Ataque(nombre: String, kiRequerido: Int)

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //              MAGIA
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  case class magia(pase: Movimiento) extends Movimiento {
    def apply(combate: Combatientes): Combatientes = {
      combate._1.especie match {
        case Namekusein | Monstruo(_, _) => pase(combate)
        case _ => if (tieneEsferasDelDragon(combate._1)) {

          pase(perderEsferasDelDragon(combate._1), combate._2)
        } else { combate }
      }
    }
  }

  def tieneEsferasDelDragon(guerrero: Guerrero): Boolean = {
    guerrero.inventario.count(e => e.equals(EsferaDragon)).equals(7)
  }

  def perderEsferasDelDragon(guerrero: Guerrero): Guerrero = {
    val lista = guerrero.inventario.filter(e => !e.equals(EsferaDragon))

    guerrero.inventario(lista)
  }

}