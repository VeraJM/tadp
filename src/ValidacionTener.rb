class ValidacionTener_ < Validacion

  attr_accessor :metodo

  # initialize(Object) -> Validacion
  def initialize(metodo_tener_, objeto_para_preguntar_equal)
    self.objeto = objeto_para_preguntar_equal
    self.metodo = metodo_tener_
  end
end