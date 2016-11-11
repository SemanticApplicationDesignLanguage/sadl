package io.typefox.lsp.endpoint

import com.google.inject.AbstractModule
import com.google.inject.Guice
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.xtext.ide.server.ServerModule

class MonacoServerModule extends AbstractModule {

	override protected configure() {
		val serverInjector = Guice.createInjector(new ServerModule)
		bind(LanguageServer).toProvider [
			val languageServer = serverInjector.getInstance(LanguageServer)
			return new FileWatchingLanguageServer(languageServer)
		]
	}

}
