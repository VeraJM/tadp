
require_relative '../src/Validacion'
module Condiciones

  def ser (un_objeto)
    if un_objeto.is_a? Validacion
      procedimiento = proc {|otro| self.objeto.validar(otro) }
    else
      procedimiento = proc{|otro| objeto.eql? otro }
    end
    crear_validacion_personalizada(un_objeto, &procedimiento)
  end

  def mayor_a (un_objeto)
    crear_validacion_personalizada(un_objeto) {|otro| otro > self.objeto}
  end

  def menor_a (un_objeto)
    crear_validacion_personalizada(un_objeto) {|otro| otro < self.objeto}
  end

  def uno_de_estos (*args)
    if args.count == 1 && args[0].class.equal?(Array)
      una_lista = args[0]
    else
      una_lista = args
    end
    crear_validacion_personalizada(una_lista) {|otro| self.objeto.include? otro}
  end

  def entender(metodo)
    crear_validacion_personalizada(metodo) {|otro| otro.respond_to? (self.objeto)}
  end

  def explotar_con (clase_error)
    crear_validacion_personalizada(clase_error) {|bloque|
      begin
        bloque.call
        false
      rescue Exception => e
        e.is_a? clase_error
      end
    }
  end

  def ser_(metodo_con_ser_)
    mensaje = metodo_con_ser_.to_s[4..-1] + '?'

    crear_validacion_personalizada(mensaje) {|otro| otro.send(self.objeto)}
  end

  def tener_(atributo_con_tener_, un_objeto)
    nombre_atributo = '@'+  atributo_con_tener_.to_s[6..-1]

    crear_validacion_personalizada(un_objeto) {|otro|
      (self.objeto).equal?(otro.class.instance_variable_get(nombre_atributo))
    }
  end

  def haber_recibido(symbol)
    crear_validacion_espia(symbol) {|espia| espia.se_llamo_a(self.objeto) }
  end

  private
  def crear_validacion_personalizada(objeto_para_validar, &bloque)
    validacion = Validacion.new objeto_para_validar
    validacion.singleton_class.send(:define_method, :validar, bloque)

    validacion
  end

  def crear_validacion_espia(objeto_para_validad, &bloque)
    validacion = ValidacionEspia.new objeto_para_validad
    validacion.singleton_class.send(:define_method, :validar, bloque)

    validacion
  end
end
