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

  def respond_to?(sym, include_all = false)
    if sym.to_s.eql? 'haber_recibido'
      true
    else
      super sym, include_all
    end
  end

  private

  def espiar_metodos
    self.class.instance_methods(false).each do |metodo|
      nombre_metodo = metodo
      bloque_metodo = method(nombre_metodo).to_proc
      espiar_metodo nombre_metodo, bloque_metodo
    end
  end

  def espiar_metodo(nombre_metodo, metodo)
    self.define_singleton_method(nombre_metodo) {|*args|
      self.metodos_llamados.push LlamadaAMetodo.new nombre_metodo, args
      instance_exec &metodo
    }
  end
end