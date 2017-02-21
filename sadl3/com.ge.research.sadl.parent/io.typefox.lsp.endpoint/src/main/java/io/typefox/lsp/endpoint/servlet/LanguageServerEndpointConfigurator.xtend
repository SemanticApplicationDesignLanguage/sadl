package io.typefox.lsp.endpoint.servlet

import com.google.inject.Inject
import com.google.inject.Injector
import javax.websocket.server.ServerEndpointConfig.Configurator

class LanguageServerEndpointConfigurator extends Configurator {
    
    @Inject
    static Injector injector
    
    override <T> getEndpointInstance(Class<T> endpointClass) throws InstantiationException {
        injector.getInstance(endpointClass)
    }
    
}