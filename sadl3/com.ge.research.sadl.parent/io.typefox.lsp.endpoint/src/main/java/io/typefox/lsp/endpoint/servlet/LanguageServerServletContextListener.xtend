package io.typefox.lsp.endpoint.servlet

import com.squarespace.jersey2.guice.JerseyGuiceServletContextListener
import io.typefox.lsp.endpoint.services.LanguageServerEndpoint
import javax.servlet.ServletContext
import javax.servlet.ServletContextEvent
import javax.websocket.server.ServerContainer
import io.typefox.lsp.endpoint.WorkspaceServerModule

class LanguageServerServletContextListener extends JerseyGuiceServletContextListener {

	@Override
	override protected modules() {
		#[new LanguageServerServletModule, new WorkspaceServerModule]
	}

	@Override
	override contextInitialized(ServletContextEvent servletContextEvent) {
		super.contextInitialized(servletContextEvent)

		val servletContext = servletContextEvent.servletContext
		val serverContainer = servletContext.serverContainer
		serverContainer.addEndpoint(LanguageServerEndpoint)
	}

	protected def ServerContainer getServerContainer(ServletContext servletContext) {
		val serverContainer = servletContext.getAttribute(ServerContainer.name)
		if (serverContainer instanceof ServerContainer)
			return serverContainer

		throw new IllegalStateException('Web container does not support JSR 356, API for WebSocket.')
	}

}
