require 'rspec'
require_relative '../fixture/fixture_test_espiar'

describe 'tests de espionaje de objetos' do

  let(:una_persona) do
    Persona.new 'juan', 22
  end

  let(:un_motor) do
    Motor.new Prueba_espia
  end

  it 'un objeto que no se espio no deberia entender haber_recibido' do
    expect{una_persona.deberia haber_recibido :edad}.to raise_exception NoMethodError

  end

  it 'haber_recibido deberia devolver un resultado exitoso si el metodo fue llamado' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_se_llama_a_edad_al_preguntar_viejo
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'haber_recibido deberia devolver un fallo si el metodo no fue llamado' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_se_llama_a_joven_al_preguntar_viejo
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se deberia llamar a edad 1 sola vez al llamar a viejo?' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_al_llamar_a_viejo_edad_se_llama_1_vez
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'se deberia llamar a edad la cantidad de veces correcta' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_se_llama_la_cantidad_de_veces_correcta
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'no se deberia llamar a viejo al llamar edad' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_al_llamar_a_edad_no_se_llama_a_viejo
    expect(lista_resultados.first.class).to eq(ResultadoFallo)
  end

  it 'se deberian registrar correctamete las llamadas con parametros' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_al_llamar_con_parametros_se_registren
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'se deberian registrar correctamete las llamadas sin parametros' do
    lista_resultados = un_motor.testear Prueba_espia, :testear_que_al_llamar_sin_parametros_no_se_registran
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

end