# Resultado es lo que devuelve la funcion deberia
class Resultado
  attr_accessor :resultado_del_equal,:nombre_test,:nombre_test_suite

  def paso?
    false
  end

  def fallo?
    false
  end

  def exploto?
    false
  end
end

##########################################################################
class ResultadoExploto < Resultado
  attr_accessor :clase_error, :mensage_error

  def initialize
    self.resultado_del_equal = false
  end

  def exploto?
    true
  end

  def mostrarse
    puts "#{nombre_test}, con causa #{clase_error} y stack #{mensage_error}."
  end
end

##########################################################################
class ResultadoFallo < Resultado
  attr_accessor :resultado_esperado, :resultado_obtenido

  def initialize
    self.resultado_del_equal = false
  end

  def fallo?
    true
  end

  def mostrarse
    puts "#{self.nombre_test}, se esperaba: #{self.resultado_esperado} y se obtuvo #{self.resultado_obtenido}."
  end
end
##########################################################################
class ResultadoPaso < Resultado

  def initialize
    self.resultado_del_equal = true
  end

  def paso?
    true
  end

  def mostrarse
    puts "#{nombre_test}. "
  end

end