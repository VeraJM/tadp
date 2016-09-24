class Comportamiento
  attr_accessor :klass,:metodo,:comportamiento

  def initialize(klass,metodo,comportamiento)
    self.klass= klass
    self.metodo= metodo
    self.comportamiento= comportamiento
  end

  def recomponer
      self.klass.send(:remove_method,self.metodo)

      puts "#{self.klass}" + "#{self.metodo}"
      unless (self.klass.instance_methods(false).include?(self.metodo))
        self.klass.send(:define_method,self.metodo,self.comportamiento)
      end
  end

end