require_relative 'Testeable'
require_relative 'TestExplotado'
require_relative 'TestNoPasado'
require_relative 'TestPasado'

class TADsPEC

  attr_accessor :resultados

  def initialize
    self.resultados = []
  end


  # TODO eliminar el modulo testeable de la clase Object luego de testear o realizar la extension de otra manera
  def testear(*args)
    Object.include Testeable
    case args.length
      when 0
        testear_todas_las_suites
      when 1
        testear_suite_completo args[0]
      else
        testear_tests_de_suite args[0], args[1..-1]
    end
    mostrar_resultados
  end

  private

  def obtener_tests(metodos)
    metodos.select {|metodo| metodo.to_s.start_with? 'testear_que_'}
  end

  def mostrar_resultados
    puts "Tests ejecutados: #{resultados.count},
          tests pasados: #{resultados.count {|resultado|  resultado.paso?}},
          tests fallidos: #{resultados.count {|resultado| resultado.fallo?}},
          tests explotados: #{resultados.count{|resultado| resultado.exploto?}}."

    puts 'Tests pasados:'
    resultados.select {|resultado| resultado.paso?}.each {|pasado| puts"#{pasado.nombre_test}, " }

    puts 'Tests fallidos:'
    resultados.select {|resultado| resultado.fallo?}.each {|fallido| fallido.mostrarse}

    puts 'Tests explotados:'
    resultados.select {|resultado| resultado.exploto?}.each {|explotado| explotado.mostrarse}

  end

  # TODO buscar la forma de obtener los datos necesarios para mostrar los distintos tipos de resultados
  def ejecutar_un_test(test, suite)
    begin
     if (suite.send test)
       resultado = TestPasado.new test
     else
       resultado = TestNoPasado.new test, true, false
     end
    rescue Exception => e
      resultado = TestExplotado.new test, e, e.backtrace
    end

    resultados.push resultado
  end

  def ejecutar_tests(tests, suite)
    tests.each {|test| ejecutar_un_test test, suite}
  end

  def testear_suite_completo(nombre_clase)
    suite = nombre_clase.new

    ejecutar_tests obtener_tests(suite.methods), suite
  end

  def testear_tests_de_suite(nombre_clase, *nombres_tests)
    ejecutar_tests obtener_tests(*nombres_tests), nombre_clase.new
  end

  def testear_todas_las_suites
  # TODO definir cuerpo del metodo
  end
end