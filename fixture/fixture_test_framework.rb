require_relative '../fixture/Modelos/Persona'
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

  def testear_que_dos_strings_son_iguales
    'hola'.deberia ser 'hola'
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
    2.deberia ser menor_a 100
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