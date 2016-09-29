require_relative '../src/Mock'
require_relative '../src/Espia'
require_relative '../src/Parser'
require_relative '../src/Resultado'
require_relative '../src/Condiciones'

class Test_tadp

  include Parser,
          Mock,
          Espia

  attr_accessor :suite, :test, :resultado

  def initialize (suite, test)
    self.suite = suite
    self.test = test
    self.resultado = nil
  end

  def testear
    preparar_suite

    enseniar_deberia_a_Object

    Mock.enseniar_mockear_a_class

    self.resultado =  self.suite.new.send(self.test)
      self.resultado.nombre_test_suite = suite
      self.resultado.nombre_test = test

    olvidar_deberia_a_Object

    Mock.recomponer_comportamiento_mockeado
    Mock.olvidar_mockear_a_class
  end

  def te_ejecutaste?
    not self.resultado.nil?
  end

  private
  def preparar_suite
    self.suite.include Condiciones
    self.suite.include Parser

    self.suite.send(:define_method, :method_missing, proc {|simbolo, *args, &block|
      case
        when es_un_metodo_ser_?(simbolo)
          ser_(simbolo)
        when es_un_metodo_tener_?(simbolo)
          tener_(simbolo, args[0])
        else
          super simbolo, args, &block
      end
    })

    enseniar_espiar_a_suite (self.suite)
  end
  private
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

  private
  def olvidar_deberia_a_Object
    Object.send(:undef_method, :deberia)
  end
end