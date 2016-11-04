package io.typefox.lsp.endpoint.services

import com.google.inject.Inject
import com.google.inject.Provider
import com.google.inject.Singleton
import io.typefox.lsapi.services.LanguageServer
import io.typefox.lsapi.services.json.MessageJsonHandler
import io.typefox.lsapi.services.transport.LanguageEndpoint
import io.typefox.lsp.endpoint.servlet.LanguageServerEndpointConfigurator
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import javax.websocket.OnClose
import javax.websocket.OnOpen
import javax.websocket.Session
import javax.websocket.server.ServerEndpoint

@Singleton
@ServerEndpoint(value='/languageServer', configurator=LanguageServerEndpointConfigurator)
class LanguageServerEndpoint {
    
    static val LANGUAGE_SERVER = "LANGUAGE_SERVER"

    @Inject
    Provider<LanguageServer> languageServerProvider

    @Inject
    MessageJsonHandler jsonHandler

    // TOOD: @Inject
    ExecutorService executorService = Executors.newCachedThreadPool

    val endpoints = new ConcurrentHashMap<Session, LanguageEndpoint>

    @OnOpen
    def void onOpen(Session session) {
        val reader = new WebSocketMessageReader(session, jsonHandler)
        val writer = new WebSocketMessageWriter(session, jsonHandler)
        val languageServer = languageServerProvider.get
        session.userProperties.put(LANGUAGE_SERVER, languageServer)
        val endpoint = new io.typefox.lsapi.services.transport.server.LanguageServerEndpoint(
            languageServer,
            executorService
        )
        endpoint.messageTracer = new LoggingMessageTracer 
        endpoint.connect(reader, writer)
        endpoints.put(session, endpoint)
    }

    @OnClose
    def void onClose(Session session) {
        endpoints.remove(session)
        val languageServer = session.userProperties.remove(LANGUAGE_SERVER) as LanguageServer
        languageServer.shutdown
    }

}
