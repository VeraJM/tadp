require_relative '../src/Validacion'

class Tester

  def corre_test(test)

  end

  def corre_conjunto_de_tests(*args)
    args.each do |test|  corre_test test
    end
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

  def sacar_deberia
    Object.send(:undef_method, :deberia)
  end

end

class MiSuiteDeTests
  def testear_que_pasa_algo
    'hola'
  end

  def otro_metodo_que_no_es_un_test
  end
end