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