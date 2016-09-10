class TADsPec
  require_relative './funciones.rb'

  attr_accessor :contexto,:resultados,:spec
  def initialize
    self.contexto = []
    self.resultados = Hash.new
  end

  def agregar_test test
    contexto.push(test)
  end

  def testear *args
    pruebas_efectivas = filtrar_metodos_test(contexto)
    if args[0].is_a?(Class)
      ejecutarSuite args[0]
    end
    self.mostrarResultados
  end

  def es_test metodo
    metodo.to_s.start_with?("testear_que_")
  end

  def ejecutarSuite clase
    binder = clase.new
    metodos_a_ejecutar = tests_de_suites(clase)
    metodos_a_ejecutar.each  {
        |metodo|
        resultado = binder.send(metodo)
        self.resultados[metodo] = resultado
    }
  end

  def mostrarResultados
    resultados.each do |key, resultado|
      puts "El test #{key} fue #{resultado}"
    end
  end

  def filtrar_metodos_test metodos
    metodos.select{|test| es_test(test)}
  end

  def tests_de_suites clase
    filtrar_metodos_test(clase.instance_methods(false))
  end
end
