class LlamadaAMetodo
  attr_accessor :nombre_metodo, :parametros

  def initialize nombre_metodo_llamado, parametros_recibidos = []
    self.nombre_metodo = nombre_metodo_llamado
    self.parametros = parametros_recibidos
  end

  def es?(metodo)
    metodo.eql? nombre_metodo
  end

end