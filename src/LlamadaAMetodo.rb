class LlamadaAMetodo
  attr_accessor :nombre_metodo, :parametros

  def initialize un_metodo, parametros = []
    self.nombre_metodo = un_metodo
    self.parametros = parametros
  end

  def es?(un_metodo)
    un_metodo.eql? nombre_metodo
  end

end