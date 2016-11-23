package org.tadp.dragonball

import org.tadp.dragonball.Criterios._
import org.tadp.dragonball.Especies._
import org.tadp.dragonball.Items._

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
    
    def hacerMovimiento(movimiento : Movimiento, oponente : Guerrero) : (Guerrero,Guerrero) = {
      estado match {
        // En caso de estar muerto no pasa nada
        // Aca es donde podria tirar error por estar muerto y querer hacer algo
        case Muerto => (this,oponente)
        // Hay movimientos que se puede hacer por mas que esten inconcientes. Ej: comer semillas
        case _ => movimiento(this,oponente)
      }
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 1
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def movimientoMasEfectivoContra(oponente: Guerrero, criterio: Criterio) = {
      
      /*
       * TODO: PODRIA NO HABER MOVIMIENTO QUE SATISFAGA EL CRITERIO, HAY QUE DEVOLVERLO USANDO UNA MONADA
       * */
      val resultados = for {
        movimiento <- this.habilidades
        (nuevoAtacante,nuevoOponente) = movimiento(this,oponente)
        valor = criterio(nuevoAtacante, nuevoOponente)
      }yield (movimiento,valor)
      
      resultados.sortBy(_._2).reverse.head._1
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 2
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def pelearRound(movimiento: Movimiento)(oponente: Guerrero) = {
      val (nuevoAtacante,nuevoOponente) = this.hacerMovimiento(movimiento, oponente)
      turnoOponente(nuevoAtacante,nuevoOponente)
    }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 3
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    
    def planDeAtaqueContra(oponente :Guerrero, cantidadRounds : Int)(criterio :Criterio) :List[Movimiento] = {
     
     /*
      * TODO: EL PLAN DE ATAQUE PUEDE NO ENCONTRAR UN MOVIMIENTO QUE LO SATISFAGA
      * EN ESE CASO NO SE DEVUELVE NINGUN PLAN, HAY QUE CAMBIAR MEJOR MOVIMIENTO PARA QUE DEVUELVA
      * UNA MONADA. PLAN DE ATAQUE TAMBIEN DEVUELVE UNA MONADA
      * */ 
     val rounds = List.fill(cantidadRounds) {(atc:Guerrero,opo:Guerrero,plan:List[Movimiento]) => 
                                               val movEfectivo = atc.movimientoMasEfectivoContra(opo, criterio)
                                               val (nuevoAtc,nuevoOpo) = atc.pelearRound(movEfectivo)(opo)
                                               (nuevoAtc,nuevoOpo,plan:+movEfectivo)
                                               }
     
     val (_,_,planAtaque) = rounds.foldLeft((this,oponente,List[Movimiento]() )) {
        case ((atc,opt,plan),round) => round(atc,opt,plan)
        }
      
      planAtaque
     }
    
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //      PUNTO 4
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    def pelearContra(oponente :Guerrero)(planDeAtaque : List[Movimiento]) : Pelea = {
  
      val peleadores:Pelea = PeleaEnCurso(this,oponente)
      val estado : Pelea = planDeAtaque.foldLeft(peleadores){
          (peleadores:Pelea,mov:Movimiento) => peleadores.map {
              (atc,opo) => atc.pelearRound(mov)(opo)
              }
          }
      estado
    }
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //     CAMBIOS DE ESTADO
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀    
    def recuperaKiMaximo :Guerrero = this.kiTo( this.kiMaximo)
    
    
    def morite :Guerrero = {
      val nuevoGuerrero : Guerrero = this.perderPotenciador
      nuevoGuerrero.estado(Muerto).copy(ki = 0)
    }
    
    def poneteInconsciente :Guerrero =  {
      val nuevoGuerrero : Guerrero = this.perderPotenciador
      
      this.especie match {
        case Saiyajin(_,_) => nuevoGuerrero.perdeSSJ.estado(Inconsciente)
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
        case Saiyajin(cola, Some(_)) =>
          val nivel = especie.asInstanceOf[Saiyajin].nivelSaiyajin
          val kiInicial = this.kiMaximo / Math.pow(5,nivel).asInstanceOf[Int] 
      
          this.kiMaximo(kiInicial).especie(Saiyajin(cola, None))
        case _ => this
      }
    }
  } 

  //aca termina el guerrero
  
  def turnoOponente(atacante: Guerrero, oponente:Guerrero) = {
    val (nuevoOponente,nuevoAtacante) = 
      oponente.hacerMovimiento(oponente.movimientoMasEfectivoContra(atacante, diferenciaDeKi), atacante)
    (nuevoAtacante,nuevoOponente)
  }
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //  FORMAS DE DIGESTION DE LOS MONSTRUOS
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def soloAndroides(monstruo :Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
    comida.especie match{
      case Androide => var movimientosDeVictima = comida.habilidades
                       var movimientosMonstruo = monstruo.especie.asInstanceOf[Monstruo].movimientosAprendidosPorDigestion
                       var monstruoLleno = monstruo.especie(monstruo.especie.asInstanceOf[Monstruo]
                                                                .movimientos(movimientosMonstruo ++ movimientosDeVictima))
                       (monstruoLleno, comida.morite)
      case _ => throw new RuntimeException("El monstruo no puede comer guerreros no androides")
    }
  }
  
  def comerTodo(monstruo : Guerrero, comida : Guerrero) : (Guerrero, Guerrero) = {
    var monstruoLleno = monstruo.especie(monstruo.especie.asInstanceOf[Monstruo]
                                                  .movimientos(comida.habilidades))
    //Console.println("ME LO COMI")                                              
    (monstruoLleno, comida.morite)
  }
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //  MOVIMIENTOS DEL SAIYAJIN
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def convertirseEnSSJ(atacante :Guerrero, oponente :Guerrero): (Guerrero, Guerrero)={
   
    val nuevoAtacante : Guerrero = atacante.perderPotenciador
    var nuevoGuerrero : Guerrero = nuevoAtacante    
    nuevoAtacante.especie match{
         
      case Saiyajin(cola,transformacion) if puedeConvertirseEnSJJ(nuevoAtacante)  =>
        
        transformacion match {
          //si no tenia, se transforma en nivel 1
          case None => 
            nuevoGuerrero = nuevoAtacante.multiplicarKiMaximoEn(5)
                                    .especie( Saiyajin(cola, Some(SSJ(1) ) ))
          
          case Some(SSJ(nivel)) =>
            // nuevoGuerrero = atacante.multiplicarKiMaximoEn( (nivel +1)*5  )
            nuevoGuerrero = nuevoAtacante.multiplicarKiMaximoEn(5).especie( Saiyajin(cola,Some(SSJ(nivel+1))))
          //Agrego caso mono pero que no pase nada por ahora
          case Some(Mono(_)) => 
         }
        (nuevoGuerrero , oponente )
        
     case _ => (nuevoAtacante, oponente)
    }  
  } 

  //retorna si el guerrero(saiyajin) puede avanzar un nivel se SSJ
  def puedeConvertirseEnSJJ(saiyajin :Guerrero): Boolean = saiyajin.ki >= (saiyajin.kiMaximo /2)
  
  //retorna el nivel del saiyajin, si esta en estado normal, es 0
  def nivelDelSaiyajin(saiyajin :Guerrero):Int = saiyajin.especie.asInstanceOf[Saiyajin].nivelSaiyajin
   
  
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //            Estados
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado
  
  type Movimiento = (Guerrero,Guerrero) => (Guerrero,Guerrero)
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
 
  sealed abstract class Pelea {
    def map(f : Movimiento): Pelea
    def filter(p: (Guerrero,Guerrero) => Boolean): Option[Pelea]
    def flatmap(f: (Guerrero,Guerrero) => Pelea): Pelea
    //TODO: resolver el fold para las PeleaTerminada
    //def fold[T](e : ((Guerrero,Guerrero) => T)) (f:((Guerrero,Guerrero)=>T)): T
  }
  
  case class PeleaEnCurso(atc:Guerrero,opo:Guerrero) extends Pelea {
    def map(f: Movimiento):Pelea = {
      val (nuevoAtc,nuevoOpo) = f(atc,opo)
      val estados = (nuevoAtc.estado,nuevoOpo.estado)
      estados match{
        case (_,Muerto) => PeleaTerminada(nuevoAtc)
        case (Muerto,_) => PeleaTerminada(nuevoOpo)
        case (_,_) => PeleaEnCurso(nuevoAtc,nuevoOpo)
      }  
    }
    
    def get : (Guerrero,Guerrero) = (this.atc,this.opo)
    
    def filter(p: (Guerrero,Guerrero) => Boolean): Option[Pelea] = {
      if (p(this.atc,this.opo)) Some(this) else None
    }
    
    def flatmap(f: (Guerrero,Guerrero) => Pelea ):Pelea = f(this.atc,this.opo)
    
    def fold[T](e : ((Guerrero,Guerrero) => T)) (f:((Guerrero,Guerrero)=>T)): T = e(this.atc,this.opo)
    
  }
  
  case class PeleaTerminada(ganador:Guerrero) extends Pelea {
    def map(f: Movimiento):Pelea = this
    def flatmap(f: (Guerrero,Guerrero) => Pelea ):Pelea = this
    def filter(p: (Guerrero,Guerrero) => Boolean): Option[Pelea] = None
    //TODO: Fijarse si el fold podria usar una monada como retorno
    //def fold[T](e : ((Guerrero,Guerrero) => T)) (f:((Guerrero,Guerrero)=>T)): T = None
    
    def get : Guerrero = ganador
  }
  
  case class InvalidAttackException(s:String) extends Exception(s)
  
}