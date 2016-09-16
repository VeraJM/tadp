require 'rspec'

require_relative '../src/Motor'
require_relative '../src/Resultado'
require_relative '../fixture/fixture_test_framework'


describe 'test metaprogramacion del motor' do

  it 'obtengo los test de la clase test' do

    motor = Motor.new MiSuiteDeTests, Test_de_prueba_ser

    esperado1 = [:testear_que_pasa_algo,:testear_que_pasa_otra_cosa]
    esperado2 = [:testear_que_7_es_igual_a_7,:testear_que_true_es_igual_a_true, :testear_que_7_es_igual_a_9,
                  :testear_que_hola_es_igual_a_chau]
    
    expect(motor.obtener_metodos_de_test MiSuiteDeTests).to eq(esperado1)
    expect(motor.obtener_metodos_de_test Test_de_prueba_ser).to eq(esperado2)
  end

  it 'la clase con el test entiende ser' do

    expect(MiSuiteDeTests.instance_methods.include? :ser).to eq(true)
  end

  it 'la clase con el test entiende mayor_a' do

    expect(MiSuiteDeTests.instance_methods.include? :mayor_a).to eq(true)
  end

  it 'cargo varios suite y veo que se cargaron todos sus test' do
    motor = Motor.new Prueba_Test_condiciones, Test_de_prueba_ser
    expect(motor.lista_de_test_cargados.count ).to eq(15)
  end

end

describe 'test del framework' do

  it 'pruebo el deberia ser' do
    motor = Motor.new Test_de_prueba_ser
    lista_resultados = motor.testear(Test_de_prueba_ser)

    expect(contarResultados(lista_resultados,:paso?)).to eq(2)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(2)
  end

  it 'pruebo las condiciones menor, mayor y uno_de_estos' do
    motor = Motor.new Prueba_Test_condiciones
    lista_resultados = motor.testear(Prueba_Test_condiciones)

    expect(contarResultados(lista_resultados,:paso?)).to eq(5)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(6)
  end

  it 'ejecuto todos los test cargados' do
    motor = Motor.new Prueba_Test_condiciones, Test_de_prueba_ser, Campo_de_explosiones_Test
    lista_resultados = motor.testear

    expect(contarResultados(lista_resultados,:paso?)).to eq(10)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(9)
  end

  it 'ejecuto test de una suite' do
    motor = Motor.new Test_de_prueba_ser, Prueba_Test_condiciones
    lista_resultados = motor.testear Prueba_Test_condiciones

    expect(contarResultados(lista_resultados,:paso?)).to eq(5)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(6)
  end

  it 'ejecuto test especifico' do
    motor = Motor.new Test_de_prueba_ser, Prueba_Test_condiciones
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_5_mayor_a_3,
                                     :testear_que_uno_de_estos_con_parametros

    expect(contarResultados(lista_resultados,:paso?)).to eq(2)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(0)
  end

  it 'al ejecutar tests especificos de un suite no deberia ejecutar tests de mas' do
    motor = Motor.new Test_de_prueba_ser, Prueba_Test_condiciones
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_5_mayor_a_3,
                                     :testear_que_uno_de_estos_con_parametros, :testear_que_7_es_igual_a_9

    expect(contarResultados(lista_resultados,:paso?)).to eq(2)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(0)
  end

  it 'deberia explotar con' do
    motor = Motor.new Campo_de_explosiones_Test
    lista_resultados = motor.testear

    expect(contarResultados(lista_resultados,:paso?)).to eq(3)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(1)
  end

  it 'azucar sintactico ser_' do
    motor = Motor.new Prueba_azucar_sintactico_ser_Test
    lista_resultados = motor.testear

    expect(contarResultados(lista_resultados,:paso?)).to eq(1)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(1)
    expect(contarResultados(lista_resultados,:exploto?)).to eq(1)
  end

  it 'azucar sintactico tener_' do
    motor = Motor.new Prueba_azucar_sintactico_tener_Test
    lista_resultados = motor.testear


    expect(contarResultados(lista_resultados,:paso?)).to eq(1)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(2)
    #TODO el test de apellido chequea por un atributo que el objeto no tiene, chequear si deberia explotar o si deberia
    #dar test fallido, en caso de test fallido eliminar expect inferior y sumar 1 al expect superior
    #expect( lista_resultados.select{ |resultado| resultado.exploto?}.count ).to eq(1)
  end

  def contarResultados(resultados,metodo)
    resultados.select{|resultado| resultado.send(metodo)}.count
  end
end

