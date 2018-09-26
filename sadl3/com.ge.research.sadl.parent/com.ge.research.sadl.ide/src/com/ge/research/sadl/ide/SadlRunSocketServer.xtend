/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.ide

import com.ge.research.sadl.ide.lsp.^extension.SadlLanguageServer
import com.google.inject.Guice
import java.net.InetSocketAddress
import java.nio.channels.AsynchronousServerSocketChannel
import java.nio.channels.Channels
import java.util.concurrent.Executors
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClientExtensions
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.util.Modules2

/**
 * Class for running the LS from a {@code main} method instead of starting the JAR.
 * Use this when you want to debug the server. This only works when the Theia backend is started with {@code yarn run start:socket}.
 * 
 * @author akos.kitta
 */
class SadlRunSocketServer {

	def static void main(String[] args) throws Exception {
		new SADLIdeSetup().createInjectorAndDoEMFRegistration();

		val injector = Guice.createInjector(Modules2.mixin(new SadlServerModule, [
			bind(IResourceServiceProvider.Registry).toProvider(IResourceServiceProvider.Registry.RegistryProvider)
		]))
		val serverSocket = AsynchronousServerSocketChannel.open.bind(new InetSocketAddress("localhost", 5007))
		val threadPool = Executors.newCachedThreadPool()

		while (true) {
			val socketChannel = serverSocket.accept.get
			val in = Channels.newInputStream(socketChannel)
			val out = Channels.newOutputStream(socketChannel)
			val languageServer = injector.getInstance(SadlLanguageServer)
			val launcher = Launcher.createIoLauncher(languageServer, LanguageClientExtensions, in, out, threadPool, [it])
			languageServer.connect(launcher.remoteProxy)
			launcher.startListening
			println("Started language server for client " + socketChannel.remoteAddress)
		}
	}
}
