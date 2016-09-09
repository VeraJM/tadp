module Validacion

  def es_test?(sym)
    sym.to_s[0..11] == 'testear_que_'
  end

  def contiene_tests(una_clase)
    var = una_clase.new
    metodos = var.methods(regular=true).select do |elem|
      self.es_test? elem
    end
    not metodos.empty?
  end

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

end