require 'rspec'

require_relative '../fixture/fixture_test_mock'

describe 'test de mocks' do

  motor = Motor.new PersonaHomeTests, Test_mock
  Mock.enseniar_mockear_a_class

  it 'se define el metodo mockear' do
    expect(Class.respond_to? :mockear).to eq(true)
  end

  it 'se reemplaza el metodo mockeado' do
    PersonalHome.mockear(:todas_las_personas) do [nico,axel,lean] end

    expect(Mock.metodos_mockeados.member?(:todas_las_personas)).to be(true)

    Mock.recomponer_comportamiento_mockeado
  end

  it 'se quita el metodo mockeado a Motor luego del test'do
    motor.testear PersonaHomeTests, :testear_que_personas_viejas_trae_solo_a_los_viejos
    expect(Mock.metodos_mockeados.member?(:todas_las_personas)).to be(false)
  end

  it 'se pasa el test mockeando' do
    lista_resultados = motor.testear Test_mock, :testear_que_el_mock_funca
    expect(lista_resultados.first.class).to eq(ResultadoPaso)
  end

  it 'se borra el metodo mockear'do
    Mock.olvidar_mockear_a_class
    expect(Class.respond_to? :mockear).to eq(false)
  end

  it 'falla el mockeo porque no esta definido el metodo a mockear' do
    lista_resultados = motor.testear Test_mock, :testear_que_explota_porque_no_entiende
    expect(lista_resultados.first.class).to eq(ResultadoPaso)

    # expect{PersonalHome.mockear(:gritar) do 'aaahh' end}.to raise_error(NoMethodError)
  end

  it 'pasan los dos ultimos tests' do
    resultados = motor.testear Test_mock, :testear_que_mockeo_sarasa_en_b, :testear_que_b_hereda_sarasa

    expect(resultados.all?(&:paso?)).to be true
  end
end
