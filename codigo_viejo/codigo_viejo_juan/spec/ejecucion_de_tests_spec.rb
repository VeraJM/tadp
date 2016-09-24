require 'rspec'
require_relative '../src/suiteDePrueba'
require_relative '../src/TADsPEC'

describe 'tests para controlar la correcta ejecucion de los tests' do
  let (:testeador){
    TADsPEC.new
  }

  it 'correr el suit de prueba completo deberia devolver 2 pasados' do
    testeador.testear SuiteDePrueba
    pasados = testeador.resultados.select {|resultado| resultado.paso?}
    expect(pasados.count).to eq 2
  end

  it 'correr el suit de prueba completo deberia devolver 2 fallidos' do
    testeador.testear SuiteDePrueba
    no_pasados = testeador.resultados.select {|resultado| resultado.fallo?}
    expect(no_pasados.count).to eq 2
  end

  it 'correr el suit de prueba completo deberia devolver 1 explotado' do
    testeador.testear SuiteDePrueba
    explotados = testeador.resultados.select {|resultado| resultado.exploto?}
    expect(explotados.count).to eq 1
  end

  it 'la clase Object no deberia incluir el modulo testeable despues de ejecutar los tests' do
    testeador.testear SuiteDePrueba
    expect(Object.include? Testeable).to eq false
  end

  it'al correr solo los test que pasan no deberia haber tests explotados' do
    testeador.testear SuiteDePrueba, :testear_que_7_es_7, :testear_que_8_es_mayor_que_5
    explotados = testeador.resultados.select {|resultado| resultado.exploto?}
    expect(explotados.count).to eq 0
  end

  it'al correr solo los test que no pasan no deberia haber tests pasados' do
    testeador.testear SuiteDePrueba, :testear_que_6_pertenece_a_lista_con_6, :testear_que_una_persona_entiende_new
    pasados = testeador.resultados.select {|resultado| resultado.paso?}
    expect(pasados.count).to eq 0
  end

  it'al correr solo los test que no pasan deberia haber 2 no pasados' do
    testeador.testear SuiteDePrueba, :testear_que_6_pertenece_a_lista_con_6, :testear_que_una_persona_entiende_new
    no_pasados = testeador.resultados.select {|resultado| resultado.fallo?}
    expect(no_pasados.count).to eq 2
  end
end