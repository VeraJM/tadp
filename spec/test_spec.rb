require 'rspec'
require_relative '../src/Testing'

describe 'TADsPec tests' do

  verificador = Verificador.new
  #TEST CON DEBERIA
  it 'cualquier objeto entiende :deberia' do
    expect(Object.respond_to? (:deberia)).to be(true)
  end

  it 'cualquier objeto entiende :ser' do
    expect(Object.respond_to? (:ser)).to be(true)
  end

  #TESTS CON SER
  it 'se pasa el test deberia con ser' do
    leandro = Persona.new 20
    expect(leandro.edad.deberia ser 15).to be(false)
  end

  it 'falla el test deberia con ser' do
    expect(1.deberia ser 2).to be(false)
  end


  #TESTS CON TENER
  it 'se pasa el test tener_' do
    leandro = Persona.new(22)
    expect(leandro.deberia tener_edad 22).to be(true)
  end

  it 'falla el test_tener' do
    leandro = Persona.new 20
    expect(leandro.deberia tener_edad 22).to be(false)
  end

  #TESTS CON ENTENDER
  it 'pasa el tests con metodos heredados' do
    leandro = Persona.new 20
    expect(leandro.deberia entender :class).to be(true)
  end

  it 'pasa el tests con metodos definidos' do

    leandro = Persona.new 20
    Persona.send(:define_method, :saludar, proc{'hola'})

    expect(leandro.deberia entender :saludar).to be(true)
  end

  it 'falla el test con metodos no definidos' do

    leandro = Persona.new 20
    expect(leandro.deberia entender :tirate).to be(false)
  end

  #TESTS CON EXPLOTAR


  #TODO: cambiar las aserciones de los tests!
  it 'se testea el contexto' do
    expect(verificador.testear).to be(0)
  end

  it 'se testea la suite entera' do
    expect(verificador.testear MiSuiteDeTests).to be(1)
  end

  it 'se testean solo algunos metodos de la suite' do
    expect(verificador.testear MiSuiteDeTests, (:testear_que_pasa_algo)).to be(2)
  end

  it 'es un test' do
    expect(verificador.es_test? (:testear_que_algo)).to be(true)
  end

  it 'se testea una clase con tests' do
    expect(verificador.contiene_tests MiSuiteDeTests).to be(true)
  end

  it 'se testea un test de la clase con tests' do
    verificador.testear MiSuiteDeTests, :testear_que_pasa_algo
    expect(verificador.resultado.tests_corridos).to be(1)
  end

  it 'se testea mas de un test de una suite' do
    verificador.testear PersonaTest, :testear_que_se_use_la_edad, :testear_que_la_edad
    expect(verificador.resultado.tests_corridos).to be(2)
  end

  it 'se elimina el mensaje deberia' do
    verificador.sacar_deberia
    expect(Object.respond_to? (:deberia)).to be(false)
  end

end