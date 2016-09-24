class TestNoPasado
  attr_accessor :nombre_test,
                :valor_esperado,
                :valor_obtenido

  def initialize(nombre_del_test_no_pasado, valor_esperado_pasado, valor_obtenido_pasado)
    self.nombre_test = nombre_del_test_no_pasado
    self.valor_esperado = valor_esperado_pasado
    self.valor_obtenido = valor_obtenido_pasado
  end

  def paso?
    false
  end

  def fallo?
    true
  end

  def exploto?
    false
  end

  def mostrarse
    puts "#{self.nombre_test}, se esperaba: #{self.valor_esperado} y se obtuvo #{self.valor_obtenido}"
  end
end