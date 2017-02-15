package io.typefox.lsp.endpoint.servlet

import com.google.inject.servlet.ServletModule

class LanguageServerServletModule extends ServletModule {

	@Override
	override protected configureServlets() {
		super.configureServlets();
		filter("/*").through(HttpAccessControlFilter);
		requestStaticInjection(LanguageServerEndpointConfigurator);
	}

}
