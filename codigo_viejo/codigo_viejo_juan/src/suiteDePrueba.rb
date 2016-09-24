require_relative 'Persona'

class SuiteDePrueba

  #Tests que pasan

  def testear_que_7_es_7
    7.deberia ser 7
  end

  def testear_que_8_es_mayor_que_5
    8.deberia ser mayor_a 5
  end

  #Tests que no pasan

  def testear_que_6_pertenece_a_lista_con_6
    6.deberia ser uno_de_estos [2,3,9,'hola']
  end

  def testear_que_una_persona_entiende_new
    (Persona.new 20).deberia entender :new
  end

  #Tests que explotan

  def testear_que_persona_sin_edad_no_es_vieja
    Persona.new.edad.deberia ser mayor_a 8
  end
end