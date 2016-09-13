class Persona
  attr_accessor :edad

  def initialize(valor_edad)
     self.edad = valor_edad
  end

  def viejo?
    self.edad > 30
  end

end

class PersonaTest
  def testear_que_se_use_la_edad
    lean = Persona . new ( 22)
    pato = Persona . new ( 23)
  end

  def testear_que_la_edad
    lean = Persona . new ( 22)
  end

end
