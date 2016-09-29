class Validacion

  attr_accessor :objeto

  def initialize(objeto_para_preguntar_equal)
    self.objeto = objeto_para_preguntar_equal
  end

  def obtener_objeto
     objeto.class.equal?(Validacion) ? objeto.obtener_objeto : self.objeto
  end
end

################################################################################################
class ValidacionEspia < Validacion

  def veces(cantidad)
    validacion_cantidad = proc {|espia| espia.llamadas_a(self.objeto).length.equal? cantidad}
    agregar_validacion validacion_cantidad

    self
  end

  def con_argumentos(*args)
    if not args.empty?
      validacion_parametros = proc {|espia| espia.se_llamo_con_parametros(self.objeto, *args)}
      agregar_validacion validacion_parametros
    end

    self
  end

  private
  def agregar_validacion(nueva_validacion)
    antigua_validacion = method(:validar).to_proc

    define_singleton_method :validar, proc {|espia|
      (antigua_validacion.call espia) && (nueva_validacion.call espia)
    }
  end

end