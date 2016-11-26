package org.tadp.dragonball

import org.tadp.dragonball.Criterios._
import org.tadp.dragonball.Criterios.diferenciaKi
import org.tadp.dragonball.Especies._
import org.tadp.dragonball.Items._
import org.tadp.dragonball.Movimientos._
import scala.util._

package object dragonBall {

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //               Guerrero
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  type Combatientes = (Guerrero, Guerrero) with Serializable

  case class Guerrero(nombre: String,
                      especie: Especie,
                      habilidades: List[Movimiento],
                      ki: Int = 0,
                      potenciadorGenkidama: Int = 0,
                      kiMaximo: Int,
                      inventario: List[Item],
                      estado: Estado = Normal) {

    /* "SETTERS" */
    def ki(delta: Int): Guerrero = {
      val nuevoki = (ki + delta min kiMaximo) max 0
      val nuevoGuerrero = copy(ki = nuevoki)
      if (nuevoki == 0) nuevoGuerrero.morite else nuevoGuerrero
    }
    def kiTo(value: Int): Guerrero = copy(ki = value)
    def estado(nuevoEstado: Estado): Guerrero = copy(estado = nuevoEstado)
    def kiMaximo(nuevoMaximo: Int): Guerrero = copy(kiMaximo = nuevoMaximo)
    def especie(especie: Especie): Guerrero = copy(especie = especie)
    def aumentarPotenciador: Guerrero = copy(potenciadorGenkidama = potenciadorGenkidama + 1)
    def perderPotenciador: Guerrero = copy(potenciadorGenkidama = 0)
    def inventario(nuevaLista: List[Item]): Guerrero = copy(inventario = nuevaLista)
    def multiplicarKiMaximoEn(valor: Int): Guerrero = kiMaximo(kiMaximo * valor)
    // Crear la lista de inventario con un item modificado, ej: la arma de fuego con una bala menos
    def item(itemViejo: Item, itemNuevo: Item): Guerrero = copy(inventario = inventario.updated(inventario.indexOf(itemViejo), itemNuevo))
    def esDeEspecie(especie: Especie) = { this.especie == especie }

    def hacerMovimiento(movimiento: Movimiento, oponente: Guerrero): Try[Combatientes] = {
      Try(

        estado match {
          // En caso de estar muerto no pasa nada
          // Aca es donde podria tirar error por estar muerto y querer hacer algo
          case Muerto => throw new RuntimeException("un guerrero muerto no puede atacar")
          // Hay movimientos que se puede hacer por mas que esten inconcientes. Ej: comer semillas
          case Inconsciente => movimiento match {
            case usarItem(SemillaDeErmitanio) => movimiento((this.perderPotenciador, oponente))
            case _                            => (this, oponente)
          }
          case _ => movimiento match {
            case `dejarseFajar` => movimiento((this.aumentarPotenciador, oponente))
            case `genkidama`    => movimiento(this, oponente)
            case _              => movimiento(this.perderPotenciador, oponente)
          }
        })
    }

    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    //      PUNTO 1
    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

    def movimientoMasEfectivoContra(oponente: Guerrero, criterio: Criterio) = {

      val resultados = for {
        movimiento <- this.habilidades
        (atacante, oponente) <- hacerMovimiento(movimiento, oponente).toOption
        valor = criterio(atacante, oponente)
      } yield (movimiento, valor)
      //.filter(_._2 > 0)

      resultados.sortBy(_._2).reverse.headOption.map(_._1)
    }

    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    //      PUNTO 2
    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

    def pelearRound(movimiento: Movimiento)(oponente: Guerrero): Combatientes = {
      this.pelearRoundError(movimiento)(oponente).getOrElse(this,oponente)
    }

    def pelearRoundError(movimiento: Movimiento)(oponente: Guerrero): Try[Combatientes] = {
      val (atcnuevo, oponuevo) = this.hacerMovimiento(movimiento, oponente).get
      turnoOponente(atcnuevo, oponuevo)
    }

    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    //      PUNTO 3
    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

    def planDeAtaqueContra(oponente: Guerrero, cantidadRounds: Int)(criterio: Criterio): Option[List[Movimiento]] = {

      val rounds = List.fill(cantidadRounds) {
        (pelea: Pelea, plan: List[Movimiento]) =>
          pelea.map { 
          val (atc, opo) = pelea.get
          val movEfectivo = atc.movimientoMasEfectivoContra(opo, criterio).get
          val peleaNueva = atc.pelearRoundImpl(movEfectivo)(opo)
          (peleaNueva, plan :+ movEfectivo)
      }

      val plan = Try {
        val (_, planAtaque) = rounds.foldLeft(PeleaEnCurso((this, oponente)): Pelea, List[Movimiento]()) {
          case ((pelea, plan), round) => round(pelea, plan)
        }
        planAtaque
      }
      plan.toOption
    }

    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    //      PUNTO 4
    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    def pelearContra(oponente: Guerrero)(planDeAtaque: List[Movimiento]): Pelea = {
      val peleadores: Pelea = PeleaEnCurso(this, oponente)
      val estado: Pelea = planDeAtaque.foldLeft(peleadores) {
        (peleadores: Pelea, mov: Movimiento) =>
          peleadores.flatmap {
            case (atc, opo) => atc.pelearRound(mov)(opo)
          }
      }
      estado
    }

    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    //     CAMBIOS DE ESTADO
    //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀    
    def recuperaKiMaximo: Guerrero = this.kiTo(this.kiMaximo)

    def morite: Guerrero = {
      val nuevoGuerrero = this.especie match {
        case fusion @ Fusion(_) => fusion.guerroBase
        case _                  => this
      }
      nuevoGuerrero.perderPotenciador.estado(Muerto).kiTo(0)
    }

    def poneteInconsciente: Guerrero = {
      val nuevoGuerrero: Guerrero = this.perderPotenciador
      this.especie match {
        case Saiyajin(_, _)     => nuevoGuerrero.perdeSSJ.estado(Inconsciente)
        case fusion @ Fusion(_) => fusion.guerroBase.estado(Inconsciente)
        case _                  => nuevoGuerrero.estado(Inconsciente)
      }

    }

    def revitalizate: Guerrero = {
      estado match {
        case Inconsciente =>
          this.estado(Normal)
        case _ => this
      }
    }

    def perdeSSJ: Guerrero = {

      this.especie match {
        case saiyajin @ Saiyajin(cola, Some(_)) =>
          val nivel = saiyajin.nivelSaiyajin
          val kiInicial: Int = this.kiMaximo / Math.pow(5, nivel).asInstanceOf[Int]

          this.kiMaximo(kiInicial).especie(Saiyajin(cola, None))
        case _ => this
      }
    }
  }
  //----aca termina el guerrero----

  def turnoOponente(combate: Combatientes): Try[Combatientes] = {
    combate._2.hacerMovimiento(combate._2.movimientoMasEfectivoContra(combate._1, diferenciaKi).get, combate._1)
    .map { case (opo, atc) => (atc,opo)} 
  }

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //  FORMAS DE DIGESTION DE LOS MONSTRUOS
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  def soloAndroides(monstruo: Guerrero, comida: Guerrero): (Guerrero, Guerrero) = {

    monstruo.especie match {
      case espMonstruo @ Monstruo(_, _) =>
        comida.especie match {
          case esp @ Androide => (monstruo.especie(espMonstruo.movimientos(espMonstruo.movimientosAprendidosPorDigestion ++ comida.habilidades)), comida.morite)
          case _              => throw new InvalidAttackException("El monstruo no puede comer guerreros no androides")
        }
      case _ => (monstruo, comida)
    }
  }

  def comerTodo(monstruo: Guerrero, comida: Guerrero): (Guerrero, Guerrero) = {
    val monstruoLleno = monstruo.especie match {
      case especie @ Monstruo(_, _) => monstruo.especie(especie.movimientos(comida.habilidades))
      case _                        => throw new InvalidAttackException("Solo los monstruos pueden comer")
    }
    (monstruoLleno, comida.morite)
  }

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //  MOVIMIENTOS DEL SAIYAJIN
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  def convertirseEnSSJ(combate: Combatientes): Combatientes = {
    val (atacante, oponente) = combate
    val nuevoGuerrero =
      atacante.especie match {

        case Saiyajin(cola, transformacion) if puedeConvertirseEnSJJ(atacante) =>

          transformacion match {
            //si no tenia, se transforma en nivel 1
            case None => atacante.multiplicarKiMaximoEn(5)
              .especie(Saiyajin(cola, Some(SSJ(1))))

            case Some(SSJ(nivel)) =>
              // nuevoGuerrero = atacante.multiplicarKiMaximoEn( (nivel +1)*5  )
              atacante.multiplicarKiMaximoEn(5).especie(Saiyajin(cola, Some(SSJ(nivel + 1))))
            //Agrego caso mono pero que no pase nada por ahora
            case Some(Mono(_)) => atacante
          }

        //TODO: TAL VEZ DEBERIA DAR EXCEPTION SI NO ES SAIYAJIN   
        case _ => atacante
      }

    (nuevoGuerrero, oponente)
  }

  //retorna si el guerrero(saiyajin) puede avanzar un nivel se SSJ
  def puedeConvertirseEnSJJ(saiyajin: Guerrero): Boolean = saiyajin.ki >= (saiyajin.kiMaximo / 2)

  //retorna el nivel del saiyajin, si esta en estado normal, es 0
  def nivelDelSaiyajin(saiyajin: Guerrero): Int = saiyajin.especie match {
    case saiyajin @ Saiyajin(_, _) => saiyajin.nivelSaiyajin
    case _                         => throw new RuntimeException("Solo los saiyajines tienen nivel")
  }

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //            Estados
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //          Transformaciones
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  sealed trait Transformacion
  case class SSJ(nivel: Int) extends Transformacion
  case class Mono(estadoAnterior: Guerrero) extends Transformacion

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //        RESULTADOS DE PELEA
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  sealed abstract class Pelea {
    def map(f: Combatientes => Try[Combatientes]): Pelea
    def filter(p: Combatientes => Boolean): Pelea
    def flatmap(f: Combatientes => Pelea): Pelea
    def get: Combatientes
    //TODO: resolver el fold para las PeleaTerminada
    //def fold[T](e : ((Guerrero,Guerrero) => T)) (f:((Guerrero,Guerrero)=>T)): T
  }
  case class PeleaCancelada(combate: Combatientes, motivo: Exception) extends Pelea {
    def map(f: Combatientes => Try[Combatientes]): Pelea = this
    def flatmap(f: Combatientes => Pelea): Pelea = this
    def filter(p: Combatientes => Boolean): Pelea = this
    //TODO: Fijarse si el fold podria usar una monada como retorno
    //def fold[T](e : ((Guerrero,Guerrero) => T)) (f:((Guerrero,Guerrero)=>T)): T = None

    def get: Combatientes = combate
  }

  case class PeleaEnCurso(combate: Combatientes) extends Pelea {
    def map(f: Combatientes => Try[Combatientes]): Pelea = {
        f(combate).map { 
          ( comb =>
            (comb._1.estado,comb._2.estado) match {
            case (_, Muerto) => PeleaTerminada(comb._1)
            case (Muerto, _) => PeleaTerminada(comb._2)
            case _      => PeleaEnCurso(comb._1, comb._2)
            }
        )}.getOrElse (PeleaCancelada(combate,InvalidAttackException("Ataque invalido")))
    }

    def get: Combatientes = combate
    def filter(p: Combatientes => Boolean): Pelea = {
      if (p(combate)) this else PeleaCancelada(combate, InvalidAttackException("Pelea Invalida"))
    }
    def flatmap(f: Combatientes => Pelea): Pelea = f(combate)
    def fold[T](e: (Combatientes => T))(f: (Combatientes => T)): T = e(combate)
  }

  case class PeleaTerminada(ganador: Guerrero) extends Pelea {
    def map(f: Combatientes => Try[Combatientes]): Pelea = this
    def flatmap(f: Combatientes => Pelea): Pelea = this
    def filter(p: Combatientes => Boolean): Pelea = this
    //TODO: Fijarse si el fold podria usar una monada como retorno
    //def fold[T](e : ((Guerrero,Guerrero) => T)) (f:((Guerrero,Guerrero)=>T)): T = None
    def get: Combatientes = throw new PeleaTerminadaException("La Pelea ya esta terminada")
    def getGanador: Guerrero = ganador
  }

  case class InvalidAttackException(s: String) extends Exception(s)
  case class PeleaTerminadaException(s: String) extends Exception(s)

}