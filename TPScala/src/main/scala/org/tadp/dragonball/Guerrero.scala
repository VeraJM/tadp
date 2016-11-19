package org.tadp.dragonball

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
      
     var plan:List[Movimiento] = List()
      /* Podria hacer directamente la funcion mejor movimiento aplicada parcialmente con el criterio */
      val rounds = List.fill(cantidadRounds) (criterio)
      rounds.foldLeft((this,oponente)) {
        case ((atc,opt),criter) => {
          val movimiento = atc.movimientoMasEfectivoContra(opt, criter)
          plan = plan :+ (movimiento)
          atc.pelearRound(movimiento)(opt)
        }
      }
      plan
     }
    
    def pelearContra(oponente :Guerrero)(planDeAtaque : List[Movimiento]) : Pelea = {
  
      val peleadores:Pelea = PeleaEnCurso(this,oponente)
                                                                                                      
      val estado : Pelea = planDeAtaque.foldLeft(peleadores)({
        (peleadores:Pelea,mov:Movimiento) => peleadores.pelearRound(mov)
      })
      /* LO DEJO SOLO PARA SABER QUE ERA LO ANTERIOR */
//      estadoFinalPeleadores._1.estado match{
//        case Muerto => tenemosUnGanador(estadoFinalPeleadores._2)
//        case _ => estadoFinalPeleadores._2.estado match{
//          case Muerto => tenemosUnGanador(estadoFinalPeleadores._1)
//          case _ => sinGanador(estadoFinalPeleadores._1, estadoFinalPeleadores._2)
//        }
//      }
      estado
    }
  
    //cambios de estado    
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
  //      Criterios de movimientos
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  def mayorDanioAlEnemigo(atacante: Guerrero, oponente: Guerrero) : Int = {
    oponente.kiMaximo - oponente.ki
  }
  
  def mayorKi(atacante: Guerrero, oponente: Guerrero) : Int = {
    atacante.ki
  }
  
  def diferenciaDeKi(atacante: Guerrero, oponente: Guerrero) : Int = {
    atacante.ki - oponente.ki
  }
 
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
  //              MAGIA
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  //type Pase = (Guerrero,Guerrero) => (Guerrero,Guerrero)
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
  //            Estados
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

  sealed trait Estado
  case object Muerto extends Estado
  case object Inconsciente extends Estado
  case object Normal extends Estado
  
  type Movimiento = (Guerrero,Guerrero) => (Guerrero,Guerrero)
  type Criterio = (Guerrero,Guerrero) => Int
    
  
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
      var guerreros = this.formaDigestion(atacante, oponente);
      
      (guerreros._1, guerreros._2.kiTo(0).estado(Muerto))
    }
    
    def movimientos(nuevosMovimientos :List[Movimiento]) : Especie = {
      this.copy(movimientosAprendidosPorDigestion = nuevosMovimientos)
    }
  };
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //          Transformaciones
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
  sealed trait Transformacion
  case class SSJ(nivel: Int) extends Transformacion
  case class Mono(estadoAnterior: Guerrero) extends Transformacion
  
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //              Items
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  sealed trait Item
  abstract class Arma() extends Item
  case object SemillaDeErmitanio extends Item
  
  case object ArmaFilosa extends Arma
  case object ArmaRoma extends Arma
  case class ArmaDeFuego(municion : Int) extends Arma
  
  case object FotoDeLaLuna extends Item
  case object EsferaDragon extends Item
  
  
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  //        Resultados de pelea
  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  
//  trait ResultadoPelea
//  case class tenemosUnGanador(ganador :Guerrero) extends ResultadoPelea
//  case class sinGanador(atacante :Guerrero, oponente :Guerrero) extends ResultadoPelea
  
  /*  
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }
  def size[T: (Int |∨| String)#λ](t: T) =
  t match {
    case i: Int => i
    case s: String => s.length
  }
 
  */
  //type Pelea = (Guerrero,Guerrero)
  
  
  sealed abstract class Pelea {
    //MAP por ahora esta al pedo, lo dejo por las dudas porque lo hice primero
    def map(f : Movimiento): Pelea
    def pelearRound(mov : Movimiento): Pelea
    override def equals(x : Any) : Boolean
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
    
    override def equals(x:Any) = { x match {
      case PeleaEnCurso(atacante,oponente) => atacante.nombre == atc.nombre && oponente.nombre == opo.nombre
      case _ => false}
    }
    override def hashCode = (atc.nombre + opo.nombre).hashCode
    def get : (Guerrero,Guerrero) = (atc,opo)
    
    def pelearRound(movimiento: Movimiento) = {
      val (nuevoAtacante,nuevoOponente) = atc.hacerMovimiento(movimiento, opo)
      val (finalAtc,finalOpo) = turnoOponente(nuevoAtacante,nuevoOponente)

      val estados = (finalAtc.estado,finalOpo.estado)
      estados match {
        case (_,Muerto) => PeleaTerminada(finalAtc)
        case (Muerto,_) => PeleaTerminada(finalOpo)
        case (_,_) => PeleaEnCurso(finalAtc,finalOpo)
      }
    }
    
  }
  
  case class PeleaTerminada(ganador:Guerrero) extends Pelea {
    def map(f: Movimiento):Pelea = this
    def get : Guerrero = ganador
    def pelearRound(mov:Movimiento) : Pelea = this
    override def equals(x:Any) = { x match {
      case PeleaTerminada(aGanador) => aGanador.nombre == ganador.nombre
      case _ => false}
    }
    override def hashCode = ganador.nombre.hashCode
  }
  
  case class InvalidAttackException(s:String) extends Exception(s)
  /*
  class PeleaOrGuerrero[T]
  object PeleaOrGuerrero {
    implicit object PeleaWitness extends PeleaOrGuerrero[Pelea]
    implicit object GuerreroWitness extends PeleaOrGuerrero[Guerrero]
  }
  
  sealed abstract class ResultadoPeleaMonad[PeleaOrGuerrero extends Product with Serializable{

    def get: PeleaOrGuerrero
    
    def map[PeleaOrGuerrero](f: Movimiento ) : ResultadoPeleaMonad[PeleaOrGuerrero]
    
    def apply[PeleaOrGuerrero](x: PeleaOrGuerrero): ResultadoPeleaMonad[PeleaOrGuerrero] = if (haFinalizado) None else Some(x)
    
    def haFinalizado : Boolean
    def getGanador: Guerrero
  }
  
  case class ConGanador[Guerrero](x: Guerrero) extends ResultadoPeleaMonad[Guerrero] {
    def haFinalizado = true
    def get : Guerrero = x
    def getGanador : Guerrero = x
  }
  
  case class SinGanador[+Pelea] (x : (Guerrero,Guerrero)) extends ResultadoPeleaMonad[Pelea]{
    def haFinalizado = false
    
    def map(f : Movimiento) : ResultadoPeleaMonad[PeleaOrGuerrero[_]] = {
      val (nuevoAtacante,nuevoOponente) = f(x._1,x._2)
      nuevoOponente.estado match{
        case Muerto => ConGanador(nuevoAtacante)
        case _ => 
      }
    }
    
    
    def get : Pelea = x
    def getGanador : Guerrero = {
      this.get match {
        case (atacante,oponente) => if (oponente.estado == Muerto) atacante else oponente
      }
    }
  }
  * 
  */
}
