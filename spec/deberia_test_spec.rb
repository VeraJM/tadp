require 'rspec'
require_relative '../src/prueba.rb'
require_relative '../src/mocks.rb'
require_relative '../src/tadspec.rb'
describe 'test_deberia' do

  let(:nico){
    Docente.new 'nico'
  }

  let(:persona_vieja){
    Persona.new 30
  }

  let(:tadp){
    GrupoDocente.new nico
  }

  let(:erwin){
    Persona.new 24
  }

  let(:leandro){
    Persona.new 22
  }

  let(:division_cero){
    Proc.new {7/0}
  }

  let(:division_cero){
    Proc.new {7/0}
  }

  let(:leandro_nombre){
    Proc.new {leandro.nombre}
  }

  let(:leandro_viejo){
    Proc.new {leandro.viejo?}
  }

  let(:division_cero){
    Proc.new {7/0}
  }

  it 'deberia ser igual a 1' do
    expect(1.deberia ser 1).to eq true
  end

  it 'deberia ser mayor a 2' do
    expect(3.deberia ser mayor_que 2).to eq true
  end

  it 'deberia ser menor a 2' do
    expect(1.deberia ser menor_que 2).to eq true
  end

  it 'deberia ser nico' do
    expect(tadp.docentes[0].deberia ser nico).to eq true
  end

  it 'erwin deberia ser mayor a 18' do
    expect(erwin.edad.deberia ser mayor_que 18).to eq true
  end

  it 'erwin deberia ser menor a 40' do
    expect(erwin.edad.deberia ser menor_que 40).to eq true
  end

  it '7 deberia ser 7' do
    expect(7.deberia ser 7).to eq true
  end

  it 'falla true deberia ser false' do
    expect(true.deberia ser false).to eq false
  end

  it 'falla leandro deberia ser 25 (tiene 22)' do
    expect(leandro.edad.deberia ser 25).to eq false
  end

  it 'leandro deberia ser mayor a 20' do
    expect(leandro.edad.deberia ser mayor_que 20).to eq true
  end

  it 'leandro deberia ser menor a 25' do
    expect(leandro.edad.deberia ser menor_que 25).to eq true
  end

  it 'leandro deberia ser uno de estos [7,22,"hola"]' do
    expect(leandro.edad.deberia ser uno_de_estos [7,22,"hola"]).to eq true
  end
  it 'leandro deberia ser uno de estos 7,22,"hola"' do
    expect(leandro.edad.deberia ser uno_de_estos 7,22,"hola").to eq true
  end

  it 'leandro deberia ser uno de estos 7' do
    expect(leandro.edad.deberia ser uno_de_estos 7).to eq false
  end

  it 'leandro deberia ser uno de estos 22' do
    expect(leandro.edad.deberia ser uno_de_estos 22).to eq true
  end

  it 'leandro deberia ser uno de estos [7]' do
    expect(leandro.edad.deberia ser uno_de_estos [7]).to eq false
  end

  it 'edad de leandro deberia ser uno de estos [22]' do
    expect(leandro.edad.deberia ser uno_de_estos [22]).to eq true
  end

  it 'Una persona deberia ser viejo?' do
    expect(persona_vieja.viejo?.deberia ser true).to eq true
  end

  it 'Una persona deberia ser_viejo?' do
    expect(persona_vieja.deberia ser_viejo).to eq true
  end

  it 'Leandro no deberia ser viejo' do
    expect(leandro.deberia ser_viejo).to eq false
  end

  it 'Leandro no deberia ser viejo' do
  expect{leandro.deberia ser_joven}.to raise_error(NoMethodError)
  end

  it 'Leandro deberia tener_edad 22' do
    expect(leandro.deberia tener_edad 22).to eq true
  end

  it 'Leandro deberia tener_edad 22' do
    expect(leandro.deberia tener_nombre "leandro").to eq false
  end

  it 'Leandro deberia tener nombre nil' do
    expect(leandro.deberia tener_nombre nil).to eq true
  end

  it 'Leandro deberia tener edad  mayor a 20' do
    expect(leandro.deberia tener_edad mayor_a 20).to eq true
  end

  it 'Leandro deberia tener edad  menor a 25' do
    expect(leandro.deberia tener_edad menor_a 25).to eq true
  end

  it 'Leandro deberia tener edad  uno de estos' do
    expect(leandro.deberia tener_edad uno_de_estos [7,22,"hola"]).to eq true
  end

  it 'Leandro deberia entender :viejo?' do
    expect(leandro.deberia entender :viejo?).to eq true
  end

  it 'Leandro deberia entender :class' do
    expect(leandro.deberia entender :class).to eq true
  end

  it 'Leandro no deberia entender :nombre' do
    expect(leandro.deberia entender :nombre).to eq false
  end

  it '7 /0 deberia explotar con zeroDivisionError' do
    expect(division_cero.deberia explotar_con ZeroDivisionError).to eq true
  end

  it 'leandro.nombre deberia explotar con NoMethodError' do
    expect(leandro_nombre.deberia explotar_con NoMethodError).to eq true
  end

  it 'leandro.nombre deberia explotar con Error' do
    expect(leandro_nombre.deberia explotar_con StandardError).to eq true
  end

  it 'leandro.viejo? deberia explotar con NoMethodError' do
    expect(leandro_viejo.deberia explotar_con NoMethodError).to eq false
  end

  it '{7/0} deberia explotar con NoMethodError' do
    expect(division_cero.deberia explotar_con NoMethodError).to eq false
  end

end

describe 'test_suites' do

  let(:tadspec) do
    TADsPec.new
  end
  it 'Test sobre todos los suites' do
    expect(TADsPec.testear).to eq true
  end

  it 'Test sobre la suite MiSuite' do
    expect(TADsPec.testear MiSuite).to eq false
  end

  it 'Test sobre misuite y el test testear_que_tenga_nombre' do
    expect(TADsPec.testear MiSuite ,:testear_que_tenga_nombre).to eq false
  end

end