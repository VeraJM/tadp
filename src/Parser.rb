# Parser es quien se encarga de todo lo relacionado
# con la sintaxis de como se escriben los test y
# los hace objetos que el motor u otro objeto los puedan usar
module Parser

  # es_un_tests?(:method) -> bool
  # se fija si los metodos empiesan con el nombre de los test
  def es_un_test?(metodo_de_instancia)
    empieza_con(metodo_de_instancia,"testear_que_")
  end

  # es_un_tests?(Class) -> bool
  # se fija si la clase es una clase tipo testSuite
  def es_un_test_suite?(clase)
    tests = filtrar_metodos_test(clase.instance_methods())
    tests.length > 0
  end

  def filtrar_metodos_test metodos
    metodos.select{|test| es_un_test?(test)}
  end

  # es_un_metodo_ser_?(:Method) -> bool
  # es para el azucar sintactico ser_'algunMetodo'
  def es_un_metodo_ser_?(nombre_metodo)
    empieza_con(nombre_metodo,"ser_")
  end

  # es_un_metodo_tener_?(:Method) -> bool
  # es para el azucar sintactico tener_'algunAtributo'
  def es_un_metodo_tener_?(nombre_metodo)
    empieza_con(nombre_metodo,"tener_")
  end

  def empieza_con(palabra,prefijo)
    palabra.to_s.start_with?(prefijo)
  end
end