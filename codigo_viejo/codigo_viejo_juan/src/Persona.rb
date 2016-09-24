class Persona
  attr_accessor :edad

  def initialize(valor_edad = nil)
     self.edad = valor_edad
  end

  def viejo?
    self.edad > 30
  end

end