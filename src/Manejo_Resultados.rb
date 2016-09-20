module Manejo_Resultados
  def contarResultado(resultados,metodo)
    resultados.count {|resultado| resultado.send(metodo)}
  end

  def mostrar_test(resultados,metodo)
    resultados.select {|resultado| resultado.send(metodo)}.each {|test| test.mostrarse}
  end

  def mostrar_resultados(resultados)

    puts "Tests ejecutados: #{resultados.count},
    tests pasados: #{contarResultado(resultados,:paso?)},
    tests fallidos: #{contarResultado(resultados,:fallo?)},
    tests explotados: #{contarResultado(resultados,:exploto?)}."
    puts 'Tests pasados:'
    mostrar_test(resultados,:paso?)
    puts 'Tests fallidos:'
    mostrar_test(resultados,:fallo?)
    puts 'Tests explotados:'
    mostrar_test(resultados,:exploto?)
  end
end