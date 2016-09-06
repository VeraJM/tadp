require 'rspec'

require_relative '../src/clases'
require_relative '../fixture/fixture_test_framework'


describe 'test metaprogramacion del motor' do

  it 'obtengo los test de la clase test' do

    motor = Motor.new MiSuiteDeTests, Test_de_prueba_ser
    esperado1 = [:testear_que_pasa_algo,:testear_que_pasa_otra_cosa]
    esperado2 = [:testear_que_7_es_igual_a_7,:testear_que_true_es_igual_a_true]

    expect(motor.obtener_metodos_de_test MiSuiteDeTests).to eq(esperado1)
    expect(motor.obtener_metodos_de_test Test_de_prueba_ser).to eq(esperado2)
  end

  it 'la clase con el test entiende ser' do

    expect(MiSuiteDeTests.instance_methods.include? :ser).to eq(true)
  end

  it 'la clase con el test entiende mayor_a' do

    expect(MiSuiteDeTests.instance_methods.include? :mayor_a).to eq(true)
  end

  it 'la validacion entiende equal?' do
    motor = Motor.new Test_de_prueba_ser

    expect(MiSuiteDeTests.instance_methods.include? :mayor_a).to eq(true)
  end

end


describe 'test del framework' do

  it 'pruebo el deberia ser' do
    motor = Motor.new Test_de_prueba_ser

    lista_resultados = motor.testear(Test_de_prueba_ser)
    expect( lista_resultados.all? { |resultado| resultado.resultado_del_equal} ).to eq(true)
  end

  it 'pruebo las condiciones menor, mayor y uno_de_estos' do
    motor = Motor.new Prueba_Test_condiciones

    lista_resultados = motor.testear(Prueba_Test_condiciones)
    expect( lista_resultados.all? { |resultado| resultado.resultado_del_equal} ).to eq(true)
  end

end