require_relative 'Modelos/Persona'

require_relative '../src/Motor'
require_relative '../src/Resultado'
require_relative '../src/Validacion'

require_relative '../src/Condiciones'
require_relative '../src/Parser'
class Prueba_espia

  def testear_que_se_llama_a_edad_al_preguntar_viejo
    juan = Persona.new 'juan', 22
    objeto_espiado = espiar(juan)
    objeto_espiado.viejo?

    objeto_espiado.deberia haber_recibido :edad
  end

  def testear_que_se_llama_a_joven_al_preguntar_viejo
    juan = Persona.new 'juan', 22
    espia = espiar(juan)
    espia.viejo?

    espia.deberia haber_recibido :joven?
  end

  def testear_que_al_llamar_a_viejo_edad_se_llama_1_vez
    juan = Persona.new 'juan', 22
    juan = espiar(juan)
    juan.viejo?

    juan.deberia haber_recibido(:edad).veces(1)
  end

  def testear_que_se_llama_la_cantidad_de_veces_correcta
    juan = Persona.new 'juan', 22
    juan = espiar(juan)

    juan.edad
    juan.edad
    juan.edad

    juan.deberia haber_recibido(:edad).veces(3)
  end

  def testear_que_al_llamar_a_edad_no_se_llama_a_viejo
    juan = Persona.new 'juan', 22
    juan = espiar(juan)

    juan.edad
    juan.edad
    juan.edad

    juan.deberia haber_recibido(:viejo?).veces(1)
  end

  def testear_que_al_llamar_con_parametros_se_registren
    juan = Persona.new 'juan', 22
    juan = espiar(juan)
    juan.metodo_con_parametros 'hola', 2, true

    juan.deberia haber_recibido(:metodo_con_parametros).con_argumentos 'hola', 2, true
  end

  def testear_que_al_llamar_sin_parametros_no_se_registran
    juan = Persona.new 'juan', 22
    juan = espiar(juan)
    juan.metodo_con_parametros

    juan.deberia haber_recibido(:metodo_con_parametros).con_argumentos
  end
end