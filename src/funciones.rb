module FuncionesComparativas

  def deberia proc
    self.instance_eval &proc
  end

  def ser parametro
    if parametro.class.equal? Proc
      parametro
    else
      Proc.new {self.equal? parametro}
    end
  end

  def mayor_que valor
    Proc.new {self > valor}
  end

  def mayor_a valor
    self.send(:mayor_que,valor)
  end

  def menor_que valor
    Proc.new {self < valor}
  end

  def menor_a valor
    self.send(:menor_que,valor)
  end

  def uno_de_estos *args
    (args.length == 1 && args[0].is_a?(Array)) ? valores = args[0] : valores = args
    Proc.new {valores.member?(self)}
  end

  def method_missing(sym_metodo,*args,&block)
    if sym_metodo.to_s.start_with?("ser_")
      consulta = sym_metodo.to_s[4,sym_metodo.to_s.length-4] + "?"
      Proc.new {self.send(consulta)}
    else
      super
    end
  end

end