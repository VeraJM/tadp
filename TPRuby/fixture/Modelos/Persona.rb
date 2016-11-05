class Persona
  attr_accessor :nombre, :edad

  @suenio = 20

  def initialize(nombre, edad)
    self.edad = edad
    self.nombre = nombre
  end

  def equal?(otro)
    self.nombre = otro.nombre && self.edad = otro.edad
  end

  def metodo_con_parametros(*args)
    #nada
  end

  def viejo?
    self.edad > 40
  end
end

class A
  def sarasa
    "Soy A"
  end
end

class B < A
end