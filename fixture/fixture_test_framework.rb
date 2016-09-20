require_relative '../src/Motor'
require_relative '../src/Resultado'
require_relative '../src/Validacion'

require_relative '../src/Condiciones'
require_relative '../src/Parser'

#----------------------------------------------------------------------------------------#
class MiSuiteDeTests
  def testear_que_pasa_algo
  end

  def testear_que_pasa_otra_cosa
  end

  def no_soy_test_me_cole
  end
end

#----------------------------------------------------------------------------------------#
class Test_de_prueba_ser
  def testear_que_7_es_igual_a_7
    7.deberia ser 7
  end

  def testear_que_true_es_igual_a_true
    true.deberia ser true
  end

  def testear_que_7_es_igual_a_9
    7.deberia ser 9
  end

  def testear_que_hola_es_igual_a_chau
    'hola'.deberia ser 'chau'
  end
end

#----------------------------------------------------------------------------------------#
class Prueba_Test_condiciones
  def testear_que_5_mayor_a_3
    5.deberia ser mayor_a 3
  end

  def testear_que_4_mayor_a_6
    4.deberia ser mayor_a 6
  end

  def testear_que_4_mayor_a_4
    4.deberia ser mayor_a 4
  end

  def testear_que_2_menor_a_100
    3.deberia ser menor_a 100
  end

  def testear_que_6_menor_a_3
    6.deberia ser menor_a 3
  end

  def testear_que_6_menor_a_6
    6.deberia ser menor_a 6
  end

  def testear_que_1_debia_ser_uno_de_estos
    1.deberia ser uno_de_estos [1,2]
  end

  def testear_que_uno_de_estos_con_parametros
    "asd".deberia ser uno_de_estos 1,"asd",false
  end

  def testear_que_true_es_uno_de_una_lista_de_numeros
    true.deberia ser uno_de_estos 1,2,3,5
  end

  def testear_que_funca_el_deberia_entender
    Class.deberia entender :new
  end

  def testear_que_un_objeto_entiende_new
    7.deberia entender :new
  end

  def testear_que_no_funca_el_deberia_entender
    7.deberia entender :saluda
  end
end

#----------------------------------------------------------------------------------------#
class Campo_de_explosiones_Test

  def testear_que_explota_Zero
    proc{1/0}.deberia explotar_con ZeroDivisionError
  end

  def testear_que_explota_con_exception
    proc{1/0}.deberia explotar_con Exception
  end

  def testear_que_explota_no_entender
    proc{Class.dormir}.deberia explotar_con NoMethodError
  end

  def testear_que_no_explota_al_entender
      proc{1/3}.deberia explotar_con Exception
  end
end


#----------------------------------------------------------------------------------------#
class Persona
  attr_accessor :nombre, :edad

  @suenio = 20

  def initialize(nombre, edad)
    self.edad = edad
    self.nombre = nombre
  end

  def equal?(otro)
    self.nombre = otro.nombre && self.edad = otro.edad
  end

  def viejo?
    self.edad > 40
  end
end


class Prueba_azucar_sintactico_ser_Test

  def testear_que_pepe_deberia_ser_viejo
    pepe = Persona.new "pepe", 60
    pepe.deberia ser_viejo
  end

  def testear_que_joven_deberia_ser_viejo
    joven = Persona.new 'lucas', 14
    joven.deberia ser_viejo
  end

  def testear_que_pepe_deberia_ser_joven
    pepe = Persona.new "pepe", 60
    pepe.deberia ser_joven
  end

end

class Prueba_azucar_sintactico_tener_Test
  def testear_que_pepe_deberia_tener_fiaca
    pepe = Persona.new "pepe", 60
    pepe.deberia tener_suenio 20
  end

  def testear_que_pepe_deberia_tener_nombre_juan
    pepe = Persona.new 'pepe', 40
    pepe.deberia tener_nombre 'juan'
  end

  def testear_que_pepe_deberia_tener_apellido_gomez
    pepe = Persona.new 'pepe', 30
    pepe.deberia tener_apellido 'gomez'
  end
end

#----------------------------------------------------------------------------------------#
class PersonalHome
  def todas_las_personas
    #bla bla bla
  end

  def cantidad_personas
    0
  end

  def duplico_cantidad_personas
    self.cantidad_personas*2
  end

  def personas_viejas
    self.todas_las_personas.select {|p| p.viejo?}
  end

end

class PersonaHomeTests
  def testear_que_personas_viejas_trae_solo_a_los_viejos
    nico = Persona.new('nico', 50)
    axel = Persona.new('axel',60)
    lean = Persona.new('lean',22)

   PersonalHome.mockear(:todas_las_personas) do
    [nico,axel,lean]
    end
   viejos = PersonalHome.new.personas_viejas

   viejos.deberia ser [nico, axel]
  end
end


class Test_mock

  def testear_que_el_mock_funca
    PersonalHome.mockear(:cantidad_personas) do
      100
    end

    respuesta = PersonalHome.new.cantidad_personas
    respuesta.deberia ser 100
  end

  def testear_que_se_pierde_el_contexto_entre_tests
    PersonalHome.mockear(:nuevo_metodo) do
      "no importa, estoy probando cantidad_personas"
    end

    #en el test anterior modifique la cantidad_personas para que devuelva 100
    #si no perderia el contexto deberia mantener ese 100, pero com lo pierde, vuelve al original que es 0
    respuesta = PersonalHome.new.cantidad_personas
    respuesta.deberia ser 0
  end

  def testear_que_explota_porque_no_entiende
    PersonalHome.mockear(:nueva_funcion) do
      "no me van a usar"
    end

    proc{PersonalHome.new.sarasa}.deberia explotar_con NoMethodError
  end

  ##
  # Este test estaba antes cuando habia objetos que salian de new y objetos que salian de new_mock
  # Podriamos borrarlo

  def testear_que_no_se_ensucia_la_clase_mockeada
    PersonalHome.mockear(:cantidad_personas) do
      100
    end

    respuesta = PersonalHome.new.cantidad_personas
    respuesta.deberia ser 100
  end


  def testear_que_un_metodo_llama_a_otro

    PersonalHome.mockear(:cantidad_personas) do
      10
    end

    respuesta = PersonalHome.new.duplico_cantidad_personas
    respuesta.deberia ser 20

  end
end