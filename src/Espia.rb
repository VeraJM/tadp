require_relative 'LlamadaAMetodo'

module Espia
  attr_accessor :metodos_llamados

  def inicializar
    self.metodos_llamados = []
    espiar_metodos
  end

  def se_llamo_a(metodo)
    self.metodos_llamados.any? do |metodo_llamado|
      metodo_llamado.es? metodo
    end
  end

  def llamadas_a(metodo)
    self.metodos_llamados.select do |metodo_llamado| metodo_llamado.es? metodo end
  end

  def se_llamo_con_parametros(metodo, *args)
    llamadas_a(metodo).any? do |metodo_llamado|
      metodo_llamado.parametros.eql? args
    end
  end

  def respond_to?(sym, include_all = false)
    if sym.to_s.eql? 'haber_recibido'
      true
    else
      super sym, include_all
    end
  end

  private

  # toma todos los metodos de instancia y espia
  def espiar_metodos
    self.class.instance_methods(false).each do |metodo|
      nombre_metodo = metodo
      bloque_metodo = method(nombre_metodo).to_proc
      espiar_metodo nombre_metodo, bloque_metodo
    end
  end

  # espiar un metodo consiste en sobreescribir el mismo extendiendole su comportamiento
  # se recibe el nombre del metodo y el proc que representa su implementacion
  # se define un metodo con el mismo nombre que tome el nombre y los parametros y cree un objeto Llamada a metodo con
  # los datos recibidos y lo almacene en la lista de llamados. Luego se ejecuta el proc del metodo original.
  def espiar_metodo(nombre_metodo, metodo)
    self.define_singleton_method(nombre_metodo) {|*args|
      self.metodos_llamados.push LlamadaAMetodo.new nombre_metodo, args
      instance_exec &metodo
    }
  end
end