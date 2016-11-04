package io.typefox.lsp.endpoint.services

import io.typefox.lsapi.Message
import io.typefox.lsapi.NotificationMessage
import io.typefox.lsapi.RequestMessage
import io.typefox.lsapi.ResponseMessage
import io.typefox.lsapi.services.transport.trace.MessageTracer
import org.eclipse.xtext.util.internal.Log

@Log
class LoggingMessageTracer implements MessageTracer {

    override onError(String message, Throwable throwable) {
        LOG.error(message, throwable)
    }

    override onRead(Message message, String json) {
        switch message {
            RequestMessage:
                LOG.info('Client Request:\n\t' + json)
            NotificationMessage:
                LOG.info('Client Notification:\n\t' + json)
        }
    }

    override onWrite(Message message, String json) {
        switch message {
            ResponseMessage:
                LOG.info('Server Response:\n\t' + json)
            NotificationMessage:
                LOG.info('Server Notification:\n\t' + json)
        }
    }

}
