class TestExplotado
  attr_accessor :nombre_test,
                :excepcion_lanzada,
                :stack

  def initialize(nombre_del_test_explotado, excepcion, backtrace)
    self.nombre_test = nombre_del_test_explotado
    self.excepcion_lanzada = excepcion
    self.stack = backtrace
  end

  def paso?
    false
  end

  def fallo?
    false
  end

  def exploto?
    true
  end

  def mostrarse
    puts "#{nombre_test}, con causa #{excepcion_lanzada} y stack #{stack}."
  end
end