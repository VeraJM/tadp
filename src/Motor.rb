require 'set'

require_relative 'Parser'
require_relative 'Condiciones'
require_relative 'Manejo_Resultados'
require_relative 'comportamiento'
require_relative 'Espia'

#----------------------------------------------------------------------------------------#
# Motor es el encargado de cargar las clases de los test
# y luego ejecutarlos creando una instancia y ejecutando testear()
class Motor
  include Parser, Manejo_Resultados

  @@metodos_mockeados
  @@lista_test_suites

  # initialize(clase_test) -> Motor
  # cuando se inicializa recibe la clase con los test que va a ejecutar
  def initialize (*clases_test)

    @@lista_test_suites = clases_test.clone
    self.preparar_tests_suite_cargados
  end

  # preparar_tests_suite_cargados -> Void
  # prepara el motor para la ejecucion de los test
  def preparar_tests_suite_cargados

    self.incluir_condiciones_y_parser_a_suites_cargados
    self.redefinir_method_missing_a_suites_cargados
    enseniar_espiar_a_suites
  end

  # enseniar_condiciones_a_clase(Class) -> void
  # hace que la clase entienda los mensajes ser, mayor_a, etc
  def incluir_condiciones_y_parser_a_suites_cargados

    clases_test_filtradas = obtener_test_suites

    clases_test_filtradas.each { |clase|
      clase.include Condiciones
      clase.include Parser
    }
  end

  # redefinir_method_missing_a_suites_cargados -> Void
  # redefine el metodo missing para los azucares sintacticos
  def redefinir_method_missing_a_suites_cargados

    @@lista_test_suites.each{ |clase|
      clase.send(:define_method, :method_missing, proc {|simbolo, *args, &block|
        case
          when es_un_metodo_ser_?(simbolo)
            ser_(simbolo)
          when es_un_metodo_tener_?(simbolo)
            tener_(simbolo, args[0])
          else
            super simbolo, include_all, &block
        end
      })
    }
  end

  # lista_de_test_cargados -> [:Methods]
  # devuelve los test de cada suite cargado en el motor
  def lista_de_test_cargados
    @lista_test = Set.new

    @@lista_test_suites.each { |suite|
      (obtener_metodos_de_test suite).each{ |test|
        @lista_test.add test
      }
    }
    @lista_test
  end

  # obtener_metodos_de_test(Class) -> [:Method]
  # dado una clase devuelve los metodos que son test
  def obtener_metodos_de_test(clase_test)
    clase_test.instance_methods.select{ |metodo| es_un_test?(metodo)}
  end

  #----------------------------------------------------------------------#
  # METODOS MOCK #
  # Definidos como metodos de clase para que los objetos pueden guardar su comportamiento
  # en la clase Motor

  def self.enseniar_mockear_a_class
    Class.send(:define_method, :mockear, proc { |metodo, &block|
      begin
          Motor.agregar_metodo_mockeado(self, metodo, self.instance_methods.include?(metodo) ? self.instance_method(metodo) : nil)
        self.send(:define_method, metodo, block)
      end }
    )
  end

  def self.olvidar_mockear_a_class
    Class.send(:undef_method, :mockear)
  end

  def self.agregar_metodo_mockeado(klass,metodo,comportamiento_viejo)
    @@metodos_mockeados ||= []
    @@metodos_mockeados << Comportamiento.new(klass,metodo,comportamiento_viejo)
  end

  def self.recomponer_comportamiento_mockeado
    @@metodos_mockeados ||= []
    @@metodos_mockeados.each do |comportamiento| comportamiento.recomponer()
    end
    @@metodos_mockeados.clear ##Los limpio para la siguiente corrida
  end

  def self.metodos_mockeados
    self.class_variable_get('@@metodos_mockeados').map{|comportamiento| comportamiento.metodo}

  end

#-------------------------------------------------------------------------#

  # testear() -> [Resultado]
  # realiza el testeo dependiendo de los parametros que recibe
  def testear(*args)

    enseniar_deberia_a_Object
    self.class.enseniar_mockear_a_class

    case
      when args.count == 0
        lista_resultados = testear_todo_lo_cargado
      when args.count == 1 && esta_cargado?(args[0])
        lista_resultados = testear_un_test_suit (args[0])
      when args.count > 1
        lista_resultados = testear_test_especifico(args)
    end

    olvidar_deberia_a_Object
    self.class.recomponer_comportamiento_mockeado

    mostrar_resultados lista_resultados

    lista_resultados
  end

  private
  def obtener_test_suites
    @@lista_test_suites.select {|clase| es_un_test_suite?(clase)}
  end

  private
  # enseniar_deberia_a_Object -> void
  # fuerza a que todos los objetos entiendan deberia
  def enseniar_deberia_a_Object
    unless Object.instance_methods.include? :deberia
      Object.send(:define_method, :deberia, proc {|objeto_a_comparar|
        begin
          if objeto_a_comparar.validar(self)
            resultado = ResultadoPaso.new
          else
            resultado = ResultadoFallo.new
            resultado.resultado_esperado = objeto_a_comparar.obtener_objeto
            resultado.resultado_obtenido = self

            resultado
          end
        rescue Exception => e
          resultado = ResultadoExploto.new
          resultado.clase_error = e.class
          resultado.mensaje_error = e.backtrace
          resultado
        end} )
    end

  end

  # El metodo espiar recibe un objeto para que sea espiado.
  # Se clona el objeto con el fin de no alterar el objeto original, al objeto clonado se le incluye el modulo Espia
  # que contiene todos los metodos necesarios por un objeto espia. Por ultimo se lo manda a inicializar para que
  # intervenga los metodos del objeto.
  def enseniar_espiar_a_suites
    clases_test_filtradas = obtener_test_suites

    clases_test_filtradas.each { |clase|
      clase.send :define_method, :espiar, proc {|un_objeto|
        espia = un_objeto.clone
        espia.singleton_class.include Espia
        espia.inicializar

        espia}
    }
  end

  # olvidar_deberia_a_Object -> void
  def olvidar_deberia_a_Object
    Object.send(:undef_method, :deberia)
  end

  # esta_cargado?(Class)-> bool
  # devuelve si la clase test fue cargado en el initialize
  # de la instancia creada del Motor
  def esta_cargado?(test_suite)
    @@lista_test_suites.include? (test_suite)
  end

  # testear_test_especifico([Class, :Method..])-> [Resultado]
  # testea pasandole la clase de uno de los test suite cargados
  # y los test especificos que se quiere correr
  private
  def testear_test_especifico(args)
    instancia = args[0].new
    lista_methodos = args[1..-1]
    lista_resultados = Set.new

    lista_methodos.each { |test|
      if(instancia.respond_to? test)
        resultado = ejecutar_test(instancia, test)
        resultado.nombre_test = test.to_s
        resultado.nombre_test_suite = instancia.class.to_s

        lista_resultados.add(resultado)
      end
    }
    lista_resultados
  end

  # testear_todo_lo_cargado() -> [Resultado]
  # testea todos los test de todos los test suite cargados
  # en el initialize de la instancia del motor
  private
  def testear_todo_lo_cargado
    lista_resultados = Set.new

    @@lista_test_suites.each { |test_suite|
      (testear_un_test_suit test_suite).each { |resultado|
        lista_resultados.add resultado
      }
    }
    lista_resultados
  end

  # testear_un_test_suit(Class) -> [Resultado]
  # corre todos los test del test suite pasado por parametro,
  # el test suite debe estar cargado
  private
  def testear_un_test_suit(clase_test)
    instancia = clase_test.new
    lista_resultados = Set.new

    obtener_metodos_de_test(clase_test).each { |metodo_test|
      resultado = ejecutar_test(instancia, metodo_test)
      resultado.nombre_test = metodo_test.to_s
      resultado.nombre_test_suite = instancia.class.to_s

      lista_resultados.add resultado
    }
    lista_resultados
  end

  #ejecutar_test(Class, :Method) -> Resultado
  # hace el send para ejecutar el test
  private
  def ejecutar_test(instancia_test_suit, test)

    resultado = instancia_test_suit.send(test)

    Motor.recomponer_comportamiento_mockeado
    resultado
  end

  def contarResultado(resultados,metodo)
    resultados.count {|resultado| resultado.send(metodo)}
  end

  def mostrar_test(resultados,metodo)
    resultados.select {|resultado| resultado.send(metodo)}.each {|test| test.mostrarse}
  end

  def mostrar_resultados(resultados)

    puts "Tests ejecutados: #{resultados.count},
    tests pasados: #{contarResultado(resultados,:paso?)},
    tests fallidos: #{contarResultado(resultados,:fallo?)},
    tests explotados: #{contarResultado(resultados,:exploto?)}."
    puts 'Tests pasados:'
    mostrar_test(resultados,:paso?)
    puts 'Tests fallidos:'
    mostrar_test(resultados,:fallo?)
    puts 'Tests explotados:'
    mostrar_test(resultados,:exploto?)
  end

end