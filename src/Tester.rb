class Tester

  def initialize
    resultado= Resultado.new
  end

  def corre_test(test)
    "blabla"
  end

  def corre_conjunto_de_tests(*args)
    args.each do |test|  corre_test test
    end
  end

  def es_test?(sym)
    sym.to_s[0..11] == 'testear_que_'
  end


  def correr_suite(clase)
    tests =  clase.methods(false).select do |elem|
      self.es_test? elem
    end

    tests.each do |metodo|
      self.corre_test metodo
    end

    return tests.size
  end

  def correr_metodos_de(clase, *args)
    if(args.all? {|metodo| clase.respond_to? metodo})
      self.corre_conjunto_de_tests args
    end
  end


end

class Verificador

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


  def contiene_tests(una_clase)
    var = una_clase.new
    metodos = var.methods(regular=true).select do |elem|
      self.es_test? elem
    end
    not metodos.empty?
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

  def es_test?(sym)
    sym.to_s[0..11] == 'testear_que_'
  end



end

class MiSuiteDeTests
  def testear_que_pasa_algo
    'hola'
  end

  def otro_metodo_que_no_es_un_test
  end
end

class Resultado
  attr_accessor :tests_corridos, :tests_pasados, :tests_explotados

  def initialize
    self.tests_corridos= 0
    self.tests_pasados= 0
    self.tests_explotados= 0
  end

  def aumentar
    self.tests_corridos += 1
  end

end

class Persona
  attr_accessor :edad

  def initialize(anios)
    self.edad = edad
  end

end

class PersonaTest
  def testear_que_se_use_la_edad
    lean = Persona . new ( 22)
    pato = Persona . new ( 23)
  end

  def testear_que_la_edad
    lean = Persona . new ( 22)
  end

end

class Object

  def variable_definida?(symbol,*args)
    atributo = symbol.to_s[6..-1]
    if args[0].class.equal? Proc
      Proc.new {variable = self.instance_variable_get '@'+atributo
      args[0].call variable}
    else
      Proc.new {(self.instance_variable_get ('@'+atributo)).equal? args.first}
    end
  end

  def validar_booleano(symbol)
    atributo= symbol.to_s[4..-1]+'?'
    Proc.new {self.send(atributo)}
  end

  def method_missing(symbol, *args)
    if symbol.to_s.start_with? 'tener_'
      variable_definida? symbol, args

    else if symbol.to_s.start_with? 'ser_'
           validar_booleano symbol
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