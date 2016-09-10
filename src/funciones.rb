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

  def entender metodo
   Proc.new {self.respond_to?(metodo)}
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

  def comienza_con palabra,prefijo
    palabra.to_s.start_with?(prefijo)
  end

  def method_missing(sym_metodo,*args,&block)
    if comienza_con(sym_metodo,"ser_")
      consulta = sym_metodo.to_s[4,sym_metodo.to_s.length-4] + "?"
      Proc.new {self.send(consulta)}
    elsif comienza_con(sym_metodo,"tener_")
       variable = sym_metodo.to_s[6,sym_metodo.to_s.length-6]
      Proc.new {
          self.instance_variable_get("@"+variable).deberia ser args[0]}
    else
      super
    end
  end

end