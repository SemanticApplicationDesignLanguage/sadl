package io.typefox.lsp.endpoint.services

import java.io.ByteArrayInputStream
import java.io.Closeable
import javax.websocket.CloseReason
import javax.websocket.MessageHandler.Partial
import javax.websocket.Session
import org.eclipse.lsp4j.jsonrpc.MessageConsumer
import org.eclipse.lsp4j.jsonrpc.MessageProducer
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.eclipse.lsp4j.jsonrpc.json.StreamMessageProducer
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@FinalFieldsConstructor
class WebSocketMessageProducer implements MessageProducer, Closeable {

    val Session session
    val MessageJsonHandler jsonHandler
    var buffer = new StringBuilder

    override listen(MessageConsumer messageConsumer) {
        session.addMessageHandler(String, new Partial<String>() {

            override onMessage(String partialMessage, boolean last) {
                buffer.append(partialMessage);
                if (last) {
                    process(buffer.toString, messageConsumer)
                    buffer = new StringBuilder
                }
            }

        })
    }

    protected def void process(String content, MessageConsumer messageConsumer) {
        val inputStream = new ByteArrayInputStream(content.bytes)
        val reader = new StreamMessageProducer(inputStream, jsonHandler)
        reader.listen(messageConsumer)
    }

    override close() {
        session.close(new CloseReason(CloseReason.CloseCodes.NORMAL_CLOSURE, null))
    }
    
}