require_relative '../src/Comportamiento'

module Mock

  @@metodos_mockeados

  def self.enseniar_mockear_a_class
    Class.send(:define_method, :mockear, proc { |metodo, &block|
      begin
        Mock.agregar_metodo_mockeado(self, metodo, self.instance_methods.include?(metodo) ? self.instance_method(metodo) : nil)
        self.send(:define_method, metodo, block)
      end }
    )
  end

  def self.olvidar_mockear_a_class
    Class.send(:undef_method, :mockear) if Class.respond_to? :mockear
  end

  def self.agregar_metodo_mockeado(klass,metodo,comportamiento_viejo)
    @@metodos_mockeados ||= []
    @@metodos_mockeados << Comportamiento.new(klass,metodo,comportamiento_viejo)
  end

  def self.recomponer_comportamiento_mockeado
    @@metodos_mockeados ||= []
    @@metodos_mockeados.each do |comportamiento| comportamiento.recomponer()
    end
    @@metodos_mockeados.clear ##Los limpio para la siguiente corrida
  end

  def self.metodos_mockeados
    self.class_variable_get('@@metodos_mockeados').map{|comportamiento| comportamiento.metodo}
  end

end