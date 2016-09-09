require_relative '../src/Tester'
require_relative '../src/Resultado'
require_relative '../src/Validacion'
class TADsPec

  include Validacion

  attr_accessor :tester, :resultado

  def definir_deberia
    #defino el metodo deberia a todos los objetos
    Object.send(:define_method, (:deberia), proc { |proc| instance_eval &proc})
  end

  def definir_ser
    # Object.send(:define_method, (:ser), Proc.new do |resultado| self.eql? resultado end)
    Object.send(:define_method, (:ser), proc { |resultado|
      Proc.new do
        self.equal? resultado
      end })

  end

  def testear(*args)
    tamanio = args.size
    if(tamanio == 0)
      #corre_test(args.first)
      0
    else if (tamanio == 1)
           self.resultado.tests_corridos = self.tester.correr_suite(args)
           1
         else
           tester.corre_conjunto_de_tests(args[1..-1])
           2
         end
    end
  end

  def sacar_deberia
    Object.send(:undef_method, :deberia)
  end

  def initialize
    self.tester= Tester.new
    self.resultado = Resultado.new
    definir_deberia
    definir_ser
  end

end


class Object
  include Validacion

  def method_missing(symbol, *args)
    if symbol.to_s.start_with? 'tener_'
      self.variable_definida? symbol, args

    else if symbol.to_s.start_with? 'ser_'
           self.validar_booleano symbol
         else
           super
         end
    end

  end

  def uno_de_estos(*args)
    Proc.new{
      args.any? {|elem| self.eql? elem }
    }
  end


  def entender(sym)
    Proc.new{
      self.respond_to? sym}
  end

  def explotar_con (object)
    catch (self) do
      while gets
        throw self.to_sym object
      end
      return true

    end
  end

end