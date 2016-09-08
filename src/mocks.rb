class Docente
  attr_accessor :nombre

  def initialize(nombre)
    self.nombre = nombre
  end
end

class GrupoDocente
  attr_accessor :docentes

  def initialize(docente)
    self.docentes = []
    docentes[0] = docente
  end
end

class Persona
  attr_accessor :edad

  def initialize(edad)
    self.edad = edad
  end

  def viejo?
    self.edad > 29
  end
end