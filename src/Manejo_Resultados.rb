module Manejo_Resultados
  def contarResultado(lista_tests,metodo)
    lista_tests.count {|test| test.resultado.send(metodo)}
  end

  def mostrar_test(lista_tests,metodo)
    lista_tests.select {|test| test.resultado.send(metodo)}.each {|test| test.resultado.mostrarse}
  end

  def mostrar_resultados(lista_tests)#resultados)

    puts "Tests ejecutados: #{lista_tests.count},
    tests pasados: #{contarResultado(lista_tests,:paso?)},
    tests fallidos: #{contarResultado(lista_tests,:fallo?)},
    tests explotados: #{contarResultado(lista_tests,:exploto?)}.\n\n"

    puts 'Tests pasados:'
    mostrar_test(lista_tests,:paso?)
    puts ''

    puts 'Tests fallidos:'
    mostrar_test(lista_tests,:fallo?)
    puts ''

    puts 'Tests explotados:'
    mostrar_test(lista_tests,:exploto?)
    puts ''
  end
end