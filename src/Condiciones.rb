# Condiciones es un mixin que le da a las clasesTest
# los mensajes de validacion
# Cada funcion retorna un objeto de tipo Validacion con un metodo
# hardcodeado equal?, la funcion deberia ejecuta el equal? pasandole
# como parametro el objeto que invoco al deberia
# que recibe la funcion deberia que entiende Object
module Condiciones

  # ser (un_objeto)-> Validacion
  def ser (un_objeto)
    if un_objeto.class.equal?(String)
      crear_validacion_personalizada(un_objeto) {|otro| self.objeto == otro}
    else
      crear_validacion_personalizada(un_objeto) {|otro| self.objeto.equal?(otro)}
    end
  end

  # mayor_a (un_objeto)-> Validacion
  def mayor_a (un_objeto)
    crear_validacion_personalizada(un_objeto) {|otro| otro > self.objeto}
  end

  # menor_a (un_objeto)-> Validacion
  def menor_a (un_objeto)
    crear_validacion_personalizada(un_objeto) {|otro| otro < self.objeto}
  end

  # uno_de_estos(Objetos) -> Validacion
  def uno_de_estos (*args)
    if args.count == 1 && args[0].class.equal?([].class)
      una_lista = args[0]
    else
      una_lista = args
    end

    crear_validacion_personalizada(una_lista) {|otro| self.objeto.include? otro}
  end

  # entender(Method) -> Validacion
  def entender(metodo)
    crear_validacion_personalizada(metodo) {|otro| otro.respond_to? (self.objeto)}
  end

  # explotar_con(ClassError) -> Validacion
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

  # ser_ (:Method) -> Validacion
  def ser_(metodo_con_ser_)
    mensage = metodo_con_ser_.to_s[4..-1] + '?'

    crear_validacion_personalizada(mensage) {|otro|
      otro.send(self.objeto)
    }
  end

  def tener_(atributo_con_tener_, un_objeto)
    nombre_atributo = '@'+  atributo_con_tener_.to_s[6..-1]

    crear_validacion_personalizada(un_objeto) {|otro|
      (self.objeto).equal?(otro.class.instance_variable_get(nombre_atributo))
    }
  end

  # crear_validacion_personalizada(Object) {|Object| algo...} -> Validacion
  # Crea una instancia de validacion con el metodo cumple_condicion? personalizado que se le pasa como bloque
  # Dentro del bloque se necetita definir el parametro que recibe el equal? para comparar
  private
  def crear_validacion_personalizada(objeto_para_validar, &bloque)
    validacion = Validacion.new(objeto_para_validar)
    validacion.singleton_class.send(:define_method, :equal?, bloque)

    validacion
  end
end
