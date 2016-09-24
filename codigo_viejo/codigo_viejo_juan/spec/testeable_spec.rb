require 'rspec'
require_relative '../src/Testeable'
require_relative '../src/Persona'

Object.include Testeable #Incluyo el modulo para poder testear las aserciones por fuera de los test del framework

describe 'tests para aserciones de deberia ser' do

  let (:unViejo){
    Persona.new 60
  }

  it '7 deberia ser igual a 7 ' do
    expect(7.deberia ser 7).to eq true
  end

  it 'true no deberia ser false' do
    expect(true.deberia ser false).to eq false
  end

  it '30 deberia ser mayor a 20' do
    expect(30.deberia ser mayor_a 20).to eq true
  end

  it '30 no deberia ser mayor a 40' do
    expect(30.deberia ser mayor_a 40).to eq false
  end

  it '30 no deberia ser mayor a 30' do
    expect(30.deberia ser mayor_a 30).to eq false
  end

  it '30 deberia ser menor a 40' do
    expect(30.deberia ser menor_a 40).to eq true
  end

  it '30 no deberia ser menor a 20' do
    expect(30.deberia ser menor_a 20).to eq false
  end

  it '30 no deberia ser menor a 30' do
    expect(30.deberia ser menor_a 30).to eq false
  end

  it 'true deberia ser uno de [true, false]' do
    expect(true.deberia ser uno_de_estos [true,false]).to eq true
  end

  it '5 no deberia ser uno de [2, 3, 6, true]' do
    expect(5.deberia ser uno_de_estos [2, 3, 6, true]).to eq false
  end

  it 'true deberia ser uno de true, false (varargs)' do
    expect(true.deberia ser uno_de_estos true,false).to eq true
  end

  it '5 no deberia ser uno de 2, 3, 6, true (varargs)' do
    expect(5.deberia ser uno_de_estos 2, 3, 6, true).to eq false
  end


  it 'unViejo deberia ser viejo' do
    expect(unViejo.deberia ser_viejo).to eq true
  end

end

describe 'tests de aserciones tener' do
  let (:unaPersona){
    Persona.new 25
  }

  it 'una persona de 25 deberia tener atributo edad con valor 25' do
    expect(unaPersona.deberia tener_edad 25).to eq true
  end

  it 'una persona de 25 deberia tener edad mayor a 20' do
    expect(unaPersona.deberia tener_edad mayor_a 20).to eq true
  end

  it 'una persona de 25 deberia tener edad menor a 30' do
    expect(unaPersona.deberia tener_edad menor_a 30).to eq true
  end

  it 'una persona de 25 deberia tener nombre nill' do
    expect(unaPersona.deberia tener_nombre nil).to eq true
  end

  it 'una persona de 25 deberia tener una de estas edades [20,30,25,87]' do
    expect(unaPersona.deberia tener_edad uno_de_estos 20,30,25,87).to eq true
  end

end

describe 'tests de aserciones entender' do

  let (:unaPersona){
    Persona.new 25
  }

  it 'una persona deberia entender el mensaje viejo?' do
    expect(unaPersona.deberia entender :viejo?).to eq true
  end

  it 'una persona no deberia entender el mensaje joven?' do
    expect(unaPersona.deberia entender :joven?).to eq false
  end

end

describe 'tests de aserciones explotar' do

  let (:unaPersona){
    Persona.new 25
  }

  it ' division por cero deberia explotar' do
    expect((Proc.new {7 / 0}).deberia explotar_con ZeroDivisionError).to eq true
  end

  it 'unaPersona.nombre deberia explotar con NoMethodError' do
    expect((Proc.new {unaPersona.nombre}).deberia explotar_con NoMethodError).to eq true
  end

  it 'unaPersona.nombre deberia explotar con Exception' do
    expect((Proc.new {unaPersona.nombre}).deberia explotar_con Exception).to eq true
  end

  it 'unaPersona.viejo? no deberia explotar' do
    expect((Proc.new {unaPersona.viejo?}).deberia explotar_con NoMethodError).to eq false
  end

  it ' division por cero no deberia explotar con NoMethodError' do
    expect((Proc.new {7 / 0}).deberia explotar_con NoMethodError).to eq false
  end

end
