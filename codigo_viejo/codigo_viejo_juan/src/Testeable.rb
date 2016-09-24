module Testeable

  def deberia(proc)
     if proc.arity.equal? 1
       proc.call self
     else
       instance_eval &proc
     end
  end

  def ser(parametro)
    if parametro.class.equal? Proc
      parametro
    else
      Proc.new {self.equal? parametro}
    end
  end

  def mayor_a(valor)
    Proc.new {|arg| arg > valor}
  end

  def menor_a(valor)
    Proc.new {|arg| arg < valor}
  end


  def uno_de_estos(*extras)
    if extras.length == 1
      Proc.new {|arg| extras[0].any? do |elem|
        arg.equal? elem
      end}
    else
      Proc.new {|arg| extras.any? do |elem|
        arg.equal? elem
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

  def method_missing(sym_metodo,*args,&block)
    if sym_metodo.to_s.start_with?('ser_')
      chequear_metodo_booleano sym_metodo
    else if sym_metodo.to_s.start_with?('tener_')
          chequear_variable sym_metodo, *args
         else
           super
         end
    end
  end

  private

  def chequear_metodo_booleano(nombre_metodo)
    consulta = nombre_metodo.to_s[4..-1] + '?'
    Proc.new {self.send(consulta)}
  end

  def chequear_variable(nombre_metodo_enviado, *argumentos)
    atributo = nombre_metodo_enviado.to_s[6..-1]
    if argumentos[0].class.equal? Proc
      Proc.new {variable = self.instance_variable_get '@'+atributo
                argumentos[0].call variable}
    else
      Proc.new {(self.instance_variable_get '@'+atributo).equal? argumentos[0]}
    end
    end
end
