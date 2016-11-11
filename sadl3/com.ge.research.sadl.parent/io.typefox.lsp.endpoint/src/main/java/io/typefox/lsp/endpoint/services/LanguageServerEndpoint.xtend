package io.typefox.lsp.endpoint.services

import com.google.inject.Inject
import com.google.inject.Provider
import com.google.inject.Singleton
import io.typefox.lsp.endpoint.servlet.LanguageServerEndpointConfigurator
import java.util.LinkedHashMap
import javax.websocket.OnClose
import javax.websocket.OnOpen
import javax.websocket.Session
import javax.websocket.server.ServerEndpoint
import org.apache.log4j.Logger
import org.eclipse.lsp4j.jsonrpc.RemoteEndpoint
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethod
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethodProvider
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer

@Singleton
@ServerEndpoint(value='/languageServer', configurator=LanguageServerEndpointConfigurator)
class LanguageServerEndpoint {
    
    final val LOG = Logger.getLogger(LanguageServerEndpoint)
    
    static val LANGUAGE_SERVER = "LANGUAGE_SERVER"

    @Inject
    Provider<LanguageServer> languageServerProvider

    @OnOpen
    def void onOpen(Session session) {
        val languageServer = languageServerProvider.get
        session.userProperties.put(LANGUAGE_SERVER, languageServer)
        
        val supportedMethods = new LinkedHashMap<String, JsonRpcMethod>();
        if (languageServer instanceof JsonRpcMethodProvider) {
	        supportedMethods.putAll(languageServer.supportedMethods());
        } else  {
        	supportedMethods.putAll(ServiceEndpoints.getSupportedMethods(languageServer.getClass()));	
	        supportedMethods.putAll(ServiceEndpoints.getSupportedMethods(LanguageClient));
        }
        
        val jsonHandler = new MessageJsonHandler(supportedMethods);
        val reader = new WebSocketMessageProducer(session, jsonHandler)
        val writer = new WebSocketMessageConsumer(session, jsonHandler)
        val endpoint = new RemoteEndpoint([
            if (LOG.isInfoEnabled()) {
                LOG.info("Server : " + it)
            }
            writer.consume(it)
        ], ServiceEndpoints.toEndpoint(languageServer))
        
        val client = ServiceEndpoints.toServiceObject(endpoint, LanguageClient);
        if (languageServer instanceof LanguageClientAware) {
            languageServer.connect(client)
        }
        jsonHandler.setMethodProvider(endpoint)
        
        reader.listen[
            if (LOG.isInfoEnabled()) {
                LOG.info("Client : " + it)
            }
            endpoint.consume(it)
        ]
    }

    @OnClose
    def void onClose(Session session) {
        val languageServer = session.userProperties.remove(LANGUAGE_SERVER) as LanguageServer
        languageServer.shutdown
    }

}