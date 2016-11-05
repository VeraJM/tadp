require_relative 'Parser'
require_relative 'Manejo_Resultados'
require_relative 'Test_tadp'
require_relative 'suite'

class Motor
  include Parser,
          Manejo_Resultados

  @clases_cargadas

  def initialize (*clases_test)
    @clases_cargadas = clases_test.select {|clase| es_un_test_suite? clase}
  end

  def testear(*args)

    suites = generar_suites_a_partir_de_argumentos args
    ejecutar_suites suites

    resultados = obtener_resultados_de_suites suites
    mostrar_resultados resultados

    resultados
  end

  def obtener_resultados_de_suites(suites)
    resultados = []
    suites.each{|suite| resultados << suite.obtener_resultados_de_tests}
    resultados.flatten!
    resultados
  end

  def crear_suite(clase_suite)
    tests = tests_de_la_clase_suite(clase_suite).map {|test| Test_tadp.new(clase_suite,test)}
    Suite.new(tests,clase_suite)
  end

  private
  def generar_suites_a_partir_de_argumentos(args)
    suites = []
    case
      when args.count == 0
        @clases_cargadas.each{|clase_suite| suites << crear_suite(clase_suite)}

      when args.count == 1
       suites << crear_suite(args[0])

      when args.count > 1
        suite = crear_suite(args[0])
        # creo la suite, modifico sus test para que solo sean los especificados por parametro
        metodos_test = (filtrar_metodos_test args[1..-1]).select{|metodo| args[0].new.respond_to? metodo}
        suite.tests = metodos_test.map {|test| Test_tadp.new(args[0],test) }
        suites << suite
    end
    suites
  end

  private
  def ejecutar_suites(suites)
    suites.each { |suite| suite.testear}
  end

end