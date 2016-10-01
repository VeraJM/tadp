module Manejo_Resultados
  def contarResultado(resultados,estado)
    resultados.count {|r| r.send(estado)}
  end

  def mostrar_test(resultados,metodo)
    resultados.select {|resultado|resultado.send(metodo)}.each {|resultado| resultado.mostrarse}
  end

  def mostrar_resultados(resultados)

    puts "Tests ejecutados: #{resultados.count},
    tests pasados: #{contarResultado(resultados,:paso?)},
    tests fallidos: #{contarResultado(resultados,:fallo?)},
    tests explotados: #{contarResultado(resultados,:exploto?)}.\n\n"

    puts 'Tests pasados:'
    mostrar_test(resultados,:paso?)
    puts ''

    puts 'Tests fallidos:'
    mostrar_test(resultados,:fallo?)
    puts ''

    puts 'Tests explotados:'
    mostrar_test(resultados,:exploto?)
    puts ''
  end
end