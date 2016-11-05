require_relative 'Modelos/Persona'
require_relative '../src/Motor'
require_relative '../src/Resultado'
require_relative '../src/Validacion'

require_relative '../src/Condiciones'
require_relative '../src/Parser'

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

  def testear_que_mockeo_sarasa_en_b

    B.mockear(:sarasa) { "Soy un mock" }

    respuesta = B.new.sarasa
    respuesta.deberia ser "Soy un mock"
  end

  def testear_que_b_hereda_sarasa
    A.send(:define_method, :sarasa) { "No soy mas A" }
    respuesta = B.new.sarasa
    respuesta.deberia ser "No soy mas A"
  end
end