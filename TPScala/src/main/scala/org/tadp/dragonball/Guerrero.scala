package org.tadp.dragonball

import org.tadp.dragonball.Criterios._
import org.tadp.dragonball.Especies._
import org.tadp.dragonball.Items._
import org.tadp.dragonball.Movimientos.usarItem
import org.tadp.dragonball.Movimientos.dejarseFajar
import org.tadp.dragonball.Movimientos.genkidama
import org.tadp.dragonball.Movimientos.Movimiento
import scala.util._

package object dragonBall{
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //               Guerrero
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  case class Guerrero(nombre: String,
                      especie: Especie,
                      habilidades: List[Movimiento],
                      ki: Int = 0,
                      potenciadorGenkidama: Int = 0,
                      kiMaximo: Int,
                      inventario: List[Item],
                      estado: Estado = Normal) {
    
    /* "SETTERS" */
    def ki (delta: Int) : Guerrero = {
      val nuevoki = (ki + delta min kiMaximo) max 0
      val nuevoGuerrero = copy(ki = nuevoki)
      if(nuevoki==0) nuevoGuerrero.morite else nuevoGuerrero}
    def kiTo(value: Int) : Guerrero = ki(-(ki-value))
    def estado(nuevoEstado: Estado) : Guerrero = copy (estado=nuevoEstado)
    def kiMaximo(nuevoMaximo: Int) : Guerrero = copy (kiMaximo=nuevoMaximo)
    def especie(especie: Especie) : Guerrero = copy (especie=especie)
    def aumentarPotenciador : Guerrero = copy(potenciadorGenkidama = potenciadorGenkidama + 1)
    def perderPotenciador : Guerrero = copy(potenciadorGenkidama = 0)
    def inventario(nuevaLista: List[Item]) :Guerrero = copy(inventario = nuevaLista)
    
    def multiplicarKiMaximoEn(valor :Int) :Guerrero = kiMaximo( kiMaximo * valor)
    
    // Crear la lista de inventario con un item modificado, ej: la arma de fuego con una bala menos
    def item(itemViejo: Item, itemNuevo: Item) : Guerrero = copy(inventario = inventario.updated(inventario.indexOf(itemViejo), itemNuevo))
    
    def esDeEspecie(especie : Especie) = {this.especie == especie}
    
    def hacerMovimiento(movimiento : Movimiento, oponente : Guerrero) : Try[(Guerrero,Guerrero)] = {
      Try(      
      estado match {
        case Muerto => (this,oponente)
        case Inconsciente => movimiento match{
          case usarItem(SemillaDeErmitanio) => movimiento(this.perderPotenciador, oponente)
          case _ => (this, oponente)
        }
        case _ => movimiento match { 
          case `dejarseFajar` => movimiento(this.aumentarPotenciador,oponente)
          case `genkidama` => movimiento(this,oponente) 
          case _ => movimiento(this.perderPotenciador, oponente)
          }
        }
      )
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 1
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def movimientoMasEfectivoContra(oponente: Guerrero, criterio: Criterio) = {    
      
      val resultados = for {
        movimiento <- this.habilidades
        peleadores = hacerMovimiento(movimiento, oponente)
        if peleadores.isSuccess 
          (atacante, oponente) = peleadores.get
          valor = criterio(atacante, oponente)    
      }yield(movimiento, valor)
      
   
      resultados.sortBy(_._2).reverse.headOption.map(_._1)
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 2
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def pelearRound(movimiento: Movimiento)(oponente: Guerrero) = {
      val combatientes = this.hacerMovimiento(movimiento, oponente)
      turnoOponente(combatientes.get._1,combatientes.get._2)
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 3
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def planDeAtaqueContra(oponente :Guerrero, cantidadRounds : Int)(criterio :Criterio) : Option[List[Movimiento]] = {
    
     val rounds = List.fill(cantidadRounds) {(atc:Guerrero,opo:Guerrero,plan:List[Movimiento]) => 
                                               val movEfectivo = atc.movimientoMasEfectivoContra(opo, criterio).get
                                               val (nuevoAtc,nuevoOpo) = atc.pelearRound(movEfectivo)(opo).get
                                               (nuevoAtc,nuevoOpo,plan:+movEfectivo)
                                               }
     val plan = Try {
       val (_,_,planAtaque) = rounds.foldLeft((this,oponente,List[Movimiento]() )) {
          case ((atc,opt,plan),round) => round(atc,opt,plan)
          }
       if(planAtaque.size == cantidadRounds)
         planAtaque
       else
        throw new RuntimeException("Cantidad insuficiente de movimientos")
       }
     plan.toOption
     }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 4
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    def pelearContra(oponente :Guerrero)(planDeAtaque : List[Movimiento]) : ResultadoDePelea = {
  
      try{
        val peleadores : ResultadoDePelea = PeleaEnCurso(this,oponente)
        val resultadoFinal = planDeAtaque.foldLeft(peleadores){
            (peleadores: ResultadoDePelea, mov:Movimiento) => peleadores.map {
                (atc,opo) => atc.pelearRound(mov)(opo).get
                }
            }
        resultadoFinal
      } catch {
        /* La lista de movimientos pasada tiene un movimiento no permitido para el atacante, la pelea se cancela */
        case e:InvalidAttackException => PeleaCancelada(this,oponente,e)
      }
    }
  
        
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      Torneo (tp individual)
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def pelearTorneo(unCriterio : Criterio)(retadores : List[Guerrero], rounds : Int) : ResultadoDePelea = {
      
      val estadoInicial : ResultadoDePelea = EsperandoCompetidor(this)
      retadores.foldLeft(estadoInicial){
        (resultadoAnterior : ResultadoDePelea, oponente : Guerrero) =>
          resultadoAnterior match{
            case PeleaPerdida(_,_)=> resultadoAnterior
            case PeleaCancelada(peleador, oponente, e) => PeleaPerdida(peleador, oponente)
            case PeleaEnCurso(peleador,oponente) => PeleaPerdida(peleador, oponente)
            case PeleaGanada(peleador, _) => peleador.pelearRondaTorneo(oponente, rounds, unCriterio)
            case EsperandoCompetidor(peleador) => peleador.pelearRondaTorneo(oponente, rounds, unCriterio)
          }
      }
    }
    
    def pelearRondaTorneo(retador : Guerrero, cantidadRounds : Int, criterioMovimientos : Criterio) = {
      val movimientos = this.planDeAtaqueContra(retador, cantidadRounds)(criterioMovimientos)
      movimientos match{
        case Some(movimientos) => this.pelearContra(retador)(movimientos)
        case _ => PeleaCancelada(this, retador, new RuntimeException("Peleador sin movimientos")) 
      }
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //     CAMBIOS DE ESTADO
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀    
    def recuperaKiMaximo :Guerrero = this.kiTo( this.kiMaximo)
    
    
    def morite :Guerrero = {
      val nuevoGuerrero : Guerrero = this.perderPotenciador

      nuevoGuerrero.especie match{
        case fusion @Fusion(_) => fusion.guerroBase.copy(estado = Muerto, ki = 0)
        case _ => nuevoGuerrero.estado(Muerto).copy(ki = 0)
      }
    }
    
    def poneteInconsciente :Guerrero =  {
      val nuevoGuerrero : Guerrero = this.perderPotenciador
      
      this.especie match {
        case Saiyajin(_,_) => nuevoGuerrero.perdeSSJ.estado(Inconsciente)
        case fusion @Fusion(_) => fusion.guerroBase.estado(Inconsciente)
        case _ => nuevoGuerrero.estado(Inconsciente)
      }
      
    }
    def revitalizate :Guerrero = {
      
      estado match{
        case Inconsciente =>
          this.estado(Normal)
        case _ => this
          
      }
    }
    
    def perdeSSJ :Guerrero = {
      
      this.especie match{
        case saiyajin @Saiyajin(cola, Some(_)) =>
          val nivel = saiyajin.nivelSaiyajin
          val kiInicial : Int = this.kiMaximo / Math.pow(5,nivel).asInstanceOf[Int]
      
          this.kiMaximo(kiInicial).especie(Saiyajin(cola, None))
        case _ => this
      }
    }
  } 

  //aca termina el guerrero
  
  def turnoOponente(atacante: Guerrero, oponente:Guerrero):Try[(Guerrero,Guerrero)] = {
    val combatientes = 
      oponente.hacerMovimiento(oponente.movimientoMasEfectivoContra(atacante, diferenciaDeKi).get, atacante)
      combatientes.map {case (oponente,atacante) => (atacante,oponente)}
  }
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //  FORMAS DE DIGESTION DE LOS MONSTRUOS
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def soloAndroides(monstruo :Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
    
    monstruo.especie match {
       case espMonstruo@Monstruo(_,_) =>
         comida.especie match{
           case esp@Androide => (monstruo.especie(espMonstruo.movimientos(espMonstruo.movimientosAprendidosPorDigestion ++ comida.habilidades)),comida.morite)
           case _ => throw new InvalidAttackException("El monstruo no puede comer guerreros no androides")
         }
       case _ => (monstruo,comida)
    }
  }
  
  def comerTodo(monstruo : Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
   val monstruoLleno = monstruo.especie match{
      case especie @Monstruo(_,_) => monstruo.especie(especie.movimientos(comida.habilidades))
      case _ => throw new InvalidAttackException("Solo los monstruos pueden comer")
    }
                                                
    (monstruoLleno, comida.morite)
  }
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //  MOVIMIENTOS DEL SAIYAJIN
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  //retorna si el guerrero(saiyajin) puede avanzar un nivel se SSJ
  def puedeConvertirseEnSJJ(saiyajin :Guerrero): Boolean = saiyajin.ki >= (saiyajin.kiMaximo /2)
  
  //retorna el nivel del saiyajin, si esta en estado normal, es 0
  def nivelDelSaiyajin(saiyajin :Guerrero):Int = saiyajin.especie match{case saiyajin @Saiyajin(_,_) => saiyajin.nivelSaiyajin
                                                                        case _ => throw new RuntimeException("Solo los saiyajines tienen nivel")}
   
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //            Estados
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado
  type Criterio = (Guerrero,Guerrero) => Int
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //          Transformaciones
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  sealed trait Transformacion
  case class SSJ(nivel: Int) extends Transformacion
  case class Mono(estadoAnterior: Guerrero) extends Transformacion
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //        RESULTADOS DE PELEA
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  type  Peleadores = (Guerrero, Guerrero)
  
  trait ResultadoDePelea {
    def map(f: Movimiento): ResultadoDePelea
    def filter(f: (Peleadores => Boolean)): ResultadoDePelea
    def flatMap(f: (Peleadores => ResultadoDePelea)): ResultadoDePelea
    def fold[T](e: (Peleadores => T))(f: (Peleadores => T)): T
  }
  
  case class PeleaEnCurso(val atacante : Guerrero , val oponente : Guerrero) extends ResultadoDePelea{
    def map(f: Movimiento) = { 
      val peleadoresFinales = f(atacante, oponente)
      val estadosFinales = (peleadoresFinales._1.estado, peleadoresFinales._2.estado)
      estadosFinales match{
        case (Muerto, _) => PeleaPerdida(peleadoresFinales._1, peleadoresFinales._2)
        case (_, Muerto) => PeleaGanada(peleadoresFinales._1, peleadoresFinales._2)
        case (_,_) => PeleaEnCurso(peleadoresFinales._1, peleadoresFinales._2)
      }
    }
    
    def filter(f: (Peleadores => Boolean)) = if(f(atacante, oponente)) this else ???
    
    def flatMap(f: (Peleadores => ResultadoDePelea)) = f(atacante, oponente)
    
    def fold[T](e: (Peleadores => T))(f: (Peleadores => T)) = f(atacante, oponente)
  }
  
  case class PeleaPerdida(val atacante : Guerrero, val oponente : Guerrero) extends ResultadoDePelea{
    def map(f: Movimiento) = this
    
    def filter(f: (Peleadores => Boolean)) = this
    
    def flatMap(f: (Peleadores => ResultadoDePelea)) = this
    
    def fold[T](e: (Peleadores => T))(f: (Peleadores => T)) = e(atacante, oponente)
  }
  
  case class PeleaGanada(val atacante : Guerrero, val oponente : Guerrero) extends ResultadoDePelea{
    def map(f: Movimiento) = this
    
    def filter(f: (Peleadores => Boolean)) = this
    
    def flatMap(f: (Peleadores => ResultadoDePelea)) = this
    
    def fold[T](e: (Peleadores => T))(f: (Peleadores => T)) = e(atacante, oponente)
  }
  
  case class PeleaCancelada(val atacante : Guerrero, val oponente : Guerrero, val causa : Exception) extends ResultadoDePelea{
    def map(f: Movimiento) = this
    
    def filter(f: (Peleadores => Boolean)) = this
    
    def flatMap(f: (Peleadores => ResultadoDePelea)) = this
    
    def fold[T](e: (Peleadores => T))(f: (Peleadores => T)) = e(atacante, oponente)
  }
  
  case class EsperandoCompetidor(val peleador : Guerrero) extends ResultadoDePelea{
        def map(f: Movimiento) = this
    
    def filter(f: (Peleadores => Boolean)) = this
    
    def flatMap(f: (Peleadores => ResultadoDePelea)) = this
    
    def fold[T](e: (Peleadores => T))(f: (Peleadores => T)) = ???
  }
  
  case class InvalidAttackException(s:String) extends Exception(s)
  
}