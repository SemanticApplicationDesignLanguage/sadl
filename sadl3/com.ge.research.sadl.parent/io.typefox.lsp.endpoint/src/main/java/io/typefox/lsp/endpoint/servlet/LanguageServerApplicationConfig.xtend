package io.typefox.lsp.endpoint.servlet

import java.util.Set
import javax.websocket.Endpoint
import javax.websocket.server.ServerApplicationConfig

/**
 * A dummy implementation to prevent auto-instantiation of web socket end points without Guice.
 */
class LanguageServerApplicationConfig implements ServerApplicationConfig {

    override getAnnotatedEndpointClasses(Set<Class<?>> scanned) {
        emptySet
    }

    override getEndpointConfigs(Set<Class<? extends Endpoint>> endpointClasses) {
        emptySet
    }

}
