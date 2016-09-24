class TestPasado
  attr_accessor :nombre_test

  def initialize(nombre_del_test_pasado)
    self.nombre_test = nombre_del_test_pasado
  end

  def paso?
    true
  end

  def fallo?
    false
  end

  def exploto?
    false
  end

end