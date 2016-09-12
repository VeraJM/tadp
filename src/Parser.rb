# Parser es quien se encarga de todo lo relacionado
# con la sintaxis de como se escriben los test y
# los hace objetos que el motor u otro objeto los puedan usar
module Parser

  @@STRING_TEST = 'testear_que_'
  @@STRING_TEST_SUITE = 'Test'

  # es_un_tests?(:method) -> bool
  # se fija si los metodos empiesan con el nombre de los test
  def es_un_test?(metodo_de_instancia)
    metodo_de_instancia.to_s.start_with?(@@STRING_TEST)
  end

  # es_un_tests?(Class) -> bool
  # se fija si la clase es una clase tipo testSuite
  def es_un_test_suite?(clase)
    clase.name.include? (@@STRING_TEST_SUITE)
  end

  # es_un_metodo_ser_?(:Method) -> bool
  # es para el azucar sintactico ser_'algunMetodo'
  def es_un_metodo_ser_?(nombre_metodo)
    nombre_metodo.to_s.start_with?("ser_")
  end

  # es_un_metodo_tener_?(:Method) -> bool
  # es para el azucar sintactico tener_'algunAtributo'
  def es_un_metodo_tener_?(nombre_metodo)
    nombre_metodo.to_s.start_with?("tener_")
  end
end