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
  # devuelve el atributo objeto, pero se fija si ese objeto es de tipo
  # Validacion para pedirselo a el, esto pasa por el decorado de las validaciones
  # que generan los metodos del mixin Condicion
  def obtener_objeto
     objeto.class.equal?(Validacion) ? objeto.obtener_objeto : self.objeto
  end
end

################################################################################################
class ValidacionTener_ < Validacion

  attr_accessor :metodo

  # initialize(Object) -> Validacion
  def initialize(metodo_tener_, objeto_para_preguntar_equal)
    self.objeto = objeto_para_preguntar_equal
    self.metodo = metodo_tener_
  end
end
################################################################################################
# Este tipo de validacion se usa para los espias, extiende el comportamiento para poder entender veces(cantidad) y
# con_argumentos(*args)
class ValidacionEspia < Validacion

  # numero => ValidacionEspia
  # crea un nuevo proc que chequea la cantidad de llamados y lo agrega al metodo equals
  def veces(cantidad)
    validacion_cantidad = proc {|espia| espia.llamadas_a(self.objeto).length.equal? cantidad}
    agregar_validacion validacion_cantidad

    self
  end

  # lista de parametros => ValidacionEspia
  # crea un nuevo proc que chequea los argumentos pasados al metodo y lo agrega al metodo equals
  def con_argumentos(*args)
    if not args.empty?
      validacion_parametros = proc {|espia| espia.se_llamo_con_parametros(self.objeto, *args)}
      agregar_validacion validacion_parametros
    end

    self
  end

  private

  # toma el proc del metodo equals y el proc de la nueva validacion y los junta en un nuevo metodo equals con un
  # "y" logico ya que ambas devuelven un booleano
  def agregar_validacion(nueva_validacion)
    antigua_validacion = method(:equal?).to_proc

    define_singleton_method :equal?, proc {|espia|
      (antigua_validacion.call espia) && (nueva_validacion.call espia)
    }
  end

end