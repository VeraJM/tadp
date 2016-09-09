class Resultado
  attr_accessor :tests_corridos, :tests_pasados, :tests_explotados

  def initialize
    self.tests_corridos= 0
    self.tests_pasados= 0
    self.tests_explotados= 0
  end

  def aumentar
    self.tests_corridos += 1
  end

end