require_relative 'Resultado'

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