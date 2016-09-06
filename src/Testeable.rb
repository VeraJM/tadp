class Object

  def deberia(proc)
    instance_eval &proc
  end

  def ser(parametro)
    if parametro.class.equal? Proc
      parametro
    else
      Proc.new {self.equal? parametro}
    end
  end

  def mayor_a(valor)
    Proc.new {self > valor}
  end

  def menor_a(valor)
    Proc.new {self < valor}
  end


  def uno_de_estos(*extras)
    if extras.length == 1
      Proc.new {extras[0].any? do |elem|
        self.equal? elem
      end}
    else
      Proc.new {extras.any? do |elem|
        self.equal? elem
      end}
    end
  end

  def entender(symbol)
    Proc.new {self.respond_to? symbol}
  end

  def explotar_con(error)
    Proc.new {
      begin
        self.call
      rescue Exception => e
        e.is_a? error
      end}
  end
  
end
