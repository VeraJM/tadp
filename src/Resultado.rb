# Resultado es lo que devuelve la funcion deberia
class Resultado
  attr_accessor :resultado_del_equal,:nombre_test,:nombre_test_suite

  def initialize
    self.resultado_del_equal = (es_clase ResultadoPaso)
  end

  def paso?
    es_clase ResultadoPaso
  end

  def fallo?
    es_clase ResultadoFallo
  end

  def exploto?
    es_clase ResultadoExploto
  end

  def es_clase clase
    self.class == clase
  end
end
##########################################################################
class ResultadoExploto < Resultado
  attr_accessor :clase_error, :mensage_error

  def mostrarse
    puts "#{nombre_test}, con causa #{clase_error} y stack #{mensage_error}."
  end
end
##########################################################################
class ResultadoFallo < Resultado
  attr_accessor :resultado_esperado, :resultado_obtenido

  def mostrarse
    puts "#{self.nombre_test}, se esperaba: #{self.resultado_esperado} y se obtuvo #{self.resultado_obtenido}."
  end
end
##########################################################################
class ResultadoPaso < Resultado

  def mostrarse
    puts "#{nombre_test}. "
  end
end