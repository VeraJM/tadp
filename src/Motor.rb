require 'set'

require_relative 'Parser'
require_relative 'Manejo_Resultados'
require_relative 'Test_tadp'

class Motor
  include Parser,
          Manejo_Resultados

  @lista_de_test
  @clases_cargadas

  def initialize (*clases_test)
    @clases_cargadas = clases_test.select {|clase| es_un_test_suite? clase}
  end

  def testear(*args)
    @lista_de_test = []

    cargar_lista_de_test_a_partir_de_argumentos args

    ejecutar_tests_cargados
    mostrar_resultados_de_test_cargados

    obtener_resultados_de_tests_cargados
  end

  private
  def esta_cargado? suite
    @clases_cargadas.any? { |clase_suite| clase_suite == suite}
  end

  private
  def mostrar_resultados_de_test_cargados
    mostrar_resultados @lista_de_test
  end

  private
  def crear_y_cargar_objetos_test (suite)

    (filtrar_metodos_test suite.instance_methods).each { |test|
      @lista_de_test << Test_tadp.new(suite, test)
    }
  end

  private
  def obtener_resultados_de_tests_cargados
    @resultados_obtenidos = []

    @lista_de_test.each{|test|
      @resultados_obtenidos << test.resultado if test.te_ejecutaste?
    }

    @resultados_obtenidos
  end

  private
  def cargar_lista_de_test_a_partir_de_argumentos args
    case
      when args.count == 0
        @clases_cargadas.each{|suite| crear_y_cargar_objetos_test suite}

      when args.count == 1 && esta_cargado?(args[0])
        crear_y_cargar_objetos_test args[0]

      when args.count > 1
        suite = args[0]
        metodos_test = (filtrar_metodos_test args[1..-1]).select{|metodo| suite.new.respond_to? metodo}

        metodos_test.each {|test| @lista_de_test << Test_tadp.new(suite, test)}
    end

  end

  private
  def ejecutar_tests_cargados
    @lista_de_test.each { |test| test.testear}
  end

end