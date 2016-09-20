  module Mock

    #enseniar_metodos_mock_a_Class -> Void
    #agrega los metodos necesarios para que funcione el mock
    private
    def enseniar_metodos_mock_a_Class
      Class.send(:define_method, :get_instancia_mock, proc {
        @@instancia_mock
      })

      Class.send(:define_method, :set_instancia_mock, proc {|clase_mockeada|
        if clase_mockeada.nil?
          @@instancia_mock = nil
        else
          @@instancia_mock = clase_mockeada.clone
        end
      })
      Class.send(:set_instancia_mock, nil)

      Class.send(:define_method, :new_mock, proc {
        @@instancia_mock.new
      })

      Class.send(:define_method, :mockear, proc {|metodo, &bloque|
        #seteo el atributo_mock si está en nil
        self.set_instancia_mock(self) if self.get_instancia_mock == nil

        #borro el metodo si ya existe para crearlo de nuevo con el bloque
        self.get_instancia_mock.send(:undef_method, metodo) if self.instance_methods.include? metodo
        self.get_instancia_mock.send(:define_method, metodo, bloque)
      })

    end

    #olvidar_metodos_mock_de_Class -> Void
    # saca todos los metodos agregados
    private
    def olvidar_metodos_mock_de_Class
      Class.send(:undef_method, :get_instancia_mock)
      Class.send(:undef_method, :set_instancia_mock)
      Class.send(:undef_method, :new_mock)
      Class.send(:undef_method, :mockear)
    end

  end