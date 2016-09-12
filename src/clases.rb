require 'set'

require_relative '../src/mixines'

#----------------------------------------------------------------------------------------#
# Motor es el encargado de cargar las clases de los test
# y luego ejecutarlos creando una instancia y ejecutando testear()
class Motor
  include Parser

  @@lista_test_suites

  # initialize(clase_test) -> Motor
  # cuando se inicializa recibe la clase con los test que va a ejecutar
  def initialize (*clases_test)

    @@lista_test_suites = clases_test.clone
    preparar_tests_suite_cargados
  end

  # preparar_tests_suite_cargados -> Void
  # prepara el motor para la ejecucion de los test
  def preparar_tests_suite_cargados

    incluir_condiciones_y_parser_a_suites_cargados
    redefinir_method_missing_a_suites_cargados
  end

  # enseniar_condiciones_a_clase(Class) -> void
  # hace que la clase entienda los mensajes ser, mayor_a, etc
  def incluir_condiciones_y_parser_a_suites_cargados

    clases_test_filtradas = @@lista_test_suites.select {|clase| es_un_test_suite?(clase)}

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
            super
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

  # testear() -> [Resultado]
  # realiza el testeo dependiendo de los parametros que recibe
  def testear(*args)

    enseniar_deberia_a_Object

    case
      when args.count == 0
        lista_resultados = testear_todo_lo_cargado
      when args.count == 1 && esta_cargado?(args[0])
        lista_resultados = testear_un_test_suit (args[0])
      when args.count > 1
        lista_resultados = testear_test_especifico(args)
    end

    olvidar_deberia_a_Object

    lista_resultados
  end

  # enseniar_deberia_a_Object -> void
  # fuerza a que todos los objetos entiendan deberia
  private
  def enseniar_deberia_a_Object
    unless Object.instance_methods.include? :deberia
      Object.send(:define_method, :deberia, proc {|objeto_a_comparar|
          begin
            if objeto_a_comparar.equal?(self)
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
            resultado.mensage_error = e.backtrace
            resultado
          end} )
      end

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
    lista_methodos = args[1..0].clone
    lista_resultados = Set.new

    lista_methodos.each { |test|
      resultado = instancia.send(test).clone
      resultado.nombre_test = test.to_s
      resultado.nombre_test_suite = instancia.class.to_s

      lista_resultados.add(resultado)
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
      resultado = instancia.send(metodo_test).clone
      resultado.nombre_test = metodo_test.to_s
      resultado.nombre_test_suite = instancia.class.to_s

      lista_resultados.add resultado
    }

    lista_resultados
  end

end


#----------------------------------------------------------------------------------------#
# Validacion es el objeto que crean los metodos del mixin Condicion (ser, mayor_a, etc.)
# y es el parametro de la funcion deberia del mixin Deberia
class Validacion

  attr_accessor :objeto

  # initialize(Object) -> Validacion
  def initialize(objeto_para_preguntar_equal)
    self.objeto = objeto_para_preguntar_equal
  end

  # obtener_objeto -> Object
  # debuelve el atributo objeto, pero se fija si ese objeto es de tipo
  # Validacion para pedirselo a el, esto pasa por el decorado de las validaciones
  # que generan los metodos del mixin Condicion
  def obtener_objeto
    if objeto.class.equal?(Validacion)
      objeto.obtener_objeto
    else
      self.objeto
    end
  end

end

class ValidacionTener_ < Validacion

  attr_accessor :metodo

  # initialize(Object) -> Validacion
  def initialize(metodo_tener_, objeto_para_preguntar_equal)
    self.objeto = objeto_para_preguntar_equal
    self.metodo = metodo_tener_
  end
end

#----------------------------------------------------------------------------------------#
# Resultado es lo que devuelve la funcion deberia
class Resultado
  attr_accessor :resultado_del_equal,:nombre_test,:nombre_test_suite
end

class ResultadoPaso < Resultado

  def initialize
    self.resultado_del_equal = true
  end
end

class ResultadoFallo < Resultado
    attr_accessor :resultado_esperado, :resultado_obtenido

   def initialize
     self.resultado_del_equal = false
   end
end

class ResultadoExploto < Resultado
  attr_accessor :clase_error, :mensage_error

  def initialize
    self.resultado_del_equal = false
  end
end
