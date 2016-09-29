class Comportamiento
  attr_accessor :klass,:metodo,:comportamiento

  def initialize(klass,metodo,comportamiento)
    self.klass= klass
    self.metodo= metodo
    self.comportamiento= comportamiento
  end

  def recomponer

    if(self.comportamiento == nil || self.comportamiento.owner !=self.klass)
      self.klass.send(:remove_method,self.metodo)
    else
      self.klass.send(:define_method,self.metodo,self.comportamiento)
    end

  end
end