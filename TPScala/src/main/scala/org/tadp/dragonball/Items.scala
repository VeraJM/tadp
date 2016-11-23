package org.tadp.dragonball

object Items {
  sealed trait Item
  abstract class Arma() extends Item
  case object SemillaDeErmitanio extends Item
  
  case object ArmaFilosa extends Arma
  case object ArmaRoma extends Arma
  case class ArmaDeFuego(municion : Int) extends Arma
  
  case object FotoDeLaLuna extends Item
  case object EsferaDragon extends Item
}