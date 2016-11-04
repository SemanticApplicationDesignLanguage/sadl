package io.typefox.lsp.endpoint

import com.google.inject.AbstractModule
import com.google.inject.Guice
import io.typefox.lsapi.services.LanguageServer
import org.eclipse.xtext.ide.server.ServerModule

class MonacoServerModule extends AbstractModule {

	override protected configure() {
		val serverInjector = Guice.createInjector(new ServerModule)
		val languageServer = serverInjector.getInstance(LanguageServer)
		bind(LanguageServer).toProvider [
			return new FileWatchingLanguageServer(languageServer)
		]
	}

}
