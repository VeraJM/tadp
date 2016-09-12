require_relative 'Resultado'

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