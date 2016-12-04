package org.tadp.dragonball

import org.tadp.dragonball.dragonBall._
import org.tadp.dragonball.Movimientos.Movimiento

object Especies {
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //              Especies
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  sealed trait Especie

  case object Humano extends Especie

  case class Saiyajin(cola: Boolean = true, transformacion: Option[Transformacion] = None) extends Especie{
    //def kiMaximoInicial(nuevoMaximo: Int) : Guerrero = copy (kiMaximoInicial=nuevoMaximo)
    
    def cola(sanidad : Boolean) : Saiyajin = copy(cola = sanidad)
    
    def transformacion(nuevaTransformacion : Transformacion) :Saiyajin = copy(transformacion = Some(nuevaTransformacion))
    
    def nivelSaiyajin :Int = {
      transformacion match {
        case Some(SSJ(nivel)) => nivel
        case None => 0
        case _ => 0
      }
    }
    
  }
  
  case object Androide extends Especie;

  case object Namekusein extends Especie;
  
  case class Monstruo(formaDigestion: (Guerrero ,Guerrero) => (Guerrero, Guerrero), 
                      movimientosAprendidosPorDigestion : List[Movimiento]) extends Especie{
    def comer(atacante : Guerrero, oponente : Guerrero) : (Guerrero, Guerrero) = {
      val guerreros = this.formaDigestion(atacante, oponente);
      
      (guerreros._1, guerreros._2.kiTo(0).estado(Muerto))
    }
    
    def movimientos(nuevosMovimientos :List[Movimiento]) : Especie = {
      this.copy(movimientosAprendidosPorDigestion = nuevosMovimientos)
    }
  }

  case class Fusion(guerroBase: Guerrero) extends Especie
}