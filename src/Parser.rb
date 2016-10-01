
module Parser

  def es_un_test?(metodo_de_instancia)
    empieza_con(metodo_de_instancia,'testear_que_')
  end

  def es_un_test_suite?(clase)
    tests = filtrar_metodos_test(clase.instance_methods())
    tests.length > 0
  end

  def filtrar_metodos_test metodos
    metodos.select{|test| es_un_test?(test)}
  end

  def es_un_metodo_ser_?(nombre_metodo)
    empieza_con(nombre_metodo,'ser_')
  end

  def es_un_metodo_tener_?(nombre_metodo)
    empieza_con(nombre_metodo,'tener_')
  end

  def empieza_con(palabra,prefijo)
    palabra.to_s.start_with?(prefijo)
  end

  def tests_de_la_clase_suite(clase_suite)
    filtrar_metodos_test(clase_suite.instance_methods())
  end

end