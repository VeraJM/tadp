require_relative 'Resultado'

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