require_relative 'Condiciones'
require_relative 'Parser'

class Suite
  include Condiciones
  include Parser

  attr_accessor :tests, :clase

  def initialize(tests,clase)
    self.tests = tests
    self.clase = clase
  end

  def testear
    self.tests.each{|test| test.testear}
  end

  def obtener_resultados_de_tests
    resultados_obtenidos = []

    self.tests.each{|test|
      resultados_obtenidos << test.resultado if test.te_ejecutaste? }

    resultados_obtenidos
  end

end