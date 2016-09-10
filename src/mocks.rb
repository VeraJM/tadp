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

# Esto es un test
class MiSuiteDeTests
  # Esto es un test
  def testear_que_las_personas_de_mas_de_29_son_viejas
    persona = Persona.new(30)
    persona.deberia ser_viejo
  end
  # Esto no
  def las_personas_de_mas_de_29_son_viejas
    persona = Persona.new(30)
    persona . deberia ser_viejo
  end
end