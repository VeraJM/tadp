require 'rspec'
require_relative '../fixture/fixture_test_framework'

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
end