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

    unless sym.to_s.eql? 'haber_recibido'
      super sym, include_all
    end
  end

  private

  def espiar_metodos
    self.class.instance_methods(false).each do |metodo|
      bloque_metodo = method(metodo).to_proc
      espiar_metodo metodo, bloque_metodo
    end
  end

  def espiar_metodo(nombre_metodo, metodo)
    self.define_singleton_method(nombre_metodo) {|*args|
      self.metodos_llamados.push LlamadaAMetodo.new nombre_metodo, args
      instance_exec &metodo
    }
  end

  def enseniar_espiar_a_suite clase
    clase.send(:define_method, :espiar, proc {|un_objeto|
      espia = un_objeto.clone
      espia.singleton_class.include Espia
      espia.inicializar

      espia
    })
  end
end