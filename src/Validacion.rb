#----------------------------------------------------------------------------------------#
# Validacion es el objeto que crean los metodos del mixin Condicion (ser, mayor_a, etc.)
# y es el parametro de la funcion deberia del mixin Deberia
class Validacion

  attr_accessor :objeto

  # initialize(Object) -> Validacion
  def initialize(objeto_para_preguntar_equal)
    self.objeto = objeto_para_preguntar_equal
  end

  # obtener_objeto -> Object
  # debuelve el atributo objeto, pero se fija si ese objeto es de tipo
  # Validacion para pedirselo a el, esto pasa por el decorado de las validaciones
  # que generan los metodos del mixin Condicion
  def obtener_objeto
    if objeto.class.equal?(Validacion)
      objeto.obtener_objeto
    else
      self.objeto
    end
  end

end