require 'rspec'

require_relative '../src/Motor'
require_relative '../src/Resultado'
require_relative '../fixture/fixture_test_framework'
require_relative '../src/Comportamiento'
require_relative '../fixture/fixture_test_mock'


describe 'test del framework' do

  def contarResultados(resultados,metodo)
    resultados.select{|resultado| resultado.send(metodo)}.count
  end

  it 'pruebo el deberia ser' do
    motor = Motor.new Test_de_prueba_ser
    lista_resultados = motor.testear(Test_de_prueba_ser)

    expect(contarResultados(lista_resultados,:paso?)).to eq(3)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(2)
  end

  it 'pruebo las condiciones menor, mayor y uno_de_estos' do
    motor = Motor.new Prueba_Test_condiciones
    lista_resultados = motor.testear(Prueba_Test_condiciones)

    expect(contarResultados(lista_resultados,:paso?)).to eq(5)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(7)
  end

  it 'ejecuto todos los test cargados' do
    motor = Motor.new Prueba_Test_condiciones, Test_de_prueba_ser, Campo_de_explosiones_Test
    lista_resultados = motor.testear

    expect(contarResultados(lista_resultados,:paso?)).to eq(11)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(10)
  end

  it 'ejecuto test de una suite' do
    motor = Motor.new Test_de_prueba_ser, Prueba_Test_condiciones
    lista_resultados = motor.testear Prueba_Test_condiciones

    expect(contarResultados(lista_resultados,:paso?)).to eq(5)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(7)
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

  end

  it 'prueba del mock' do
    motor = Motor.new Test_mock
    lista_resultados = motor.testear

    expect(contarResultados(lista_resultados,:paso?)).to eq(7)
    expect(contarResultados(lista_resultados,:fallo?)).to eq(0)
    expect(contarResultados(lista_resultados,:exploto?)).to eq(0)
  end
end

describe 'prueba de tests unitarios' do

  motor = Motor.new Test_de_prueba_ser,
                    Prueba_Test_condiciones,
                    Prueba_azucar_sintactico_ser_Test,
                    Prueba_azucar_sintactico_tener_Test,
                    Campo_de_explosiones_Test

  it 'se pasa el deberia ser' do
    lista_resultados = motor.testear Test_de_prueba_ser , :testear_que_7_es_igual_a_7
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'falla el deberia ser' do
    lista_resultados = motor.testear Test_de_prueba_ser , :testear_que_7_es_igual_a_9
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se pasa el deberia ser con strings' do
    lista_resultados = motor.testear Test_de_prueba_ser , :testear_que_dos_strings_son_iguales
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'se pasa el mayor_a' do
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_5_mayor_a_3

    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end


  it 'falla el mayor_a' do
    lista_resultados = motor.testear Prueba_Test_condiciones , :testear_que_4_mayor_a_6

    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'pasa el menor_a' do
    lista_resultados = motor.testear Prueba_Test_condiciones , :testear_que_2_menor_a_100

    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'falla el menor_a' do
    lista_resultados = motor.testear Prueba_Test_condiciones , :testear_que_6_menor_a_3

    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se pasa ser uno_de_estos'do
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_1_debia_ser_uno_de_estos
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'falla el ser uno_de_estos'do
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_true_es_uno_de_una_lista_de_numeros
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se pasa ser_algo'do
    lista_resultados= motor.testear Prueba_azucar_sintactico_ser_Test, :testear_que_pepe_deberia_ser_viejo
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'no se pasa ser_aglo'do
    lista_resultados = motor.testear Prueba_azucar_sintactico_ser_Test, :testear_que_joven_deberia_ser_viejo
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se pasa tener_algo'do
    lista_resultados = motor.testear Prueba_azucar_sintactico_tener_Test, :testear_que_pepe_deberia_tener_fiaca
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'falla tener_algo'do
    lista_resultados = motor.testear Prueba_azucar_sintactico_tener_Test, :testear_que_pepe_deberia_tener_nombre_juan
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se pasa entender'do
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_funca_el_deberia_entender
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'no se pasa entender'do
    lista_resultados = motor.testear Prueba_Test_condiciones, :testear_que_no_funca_el_deberia_entender
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se pasa explotar'do
    lista_resultados = motor.testear Campo_de_explosiones_Test, :testear_que_explota_Zero
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'no se pasa explotar'do
    lista_resultados = motor.testear Campo_de_explosiones_Test, :testear_que_no_explota_al_entender
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end
end
