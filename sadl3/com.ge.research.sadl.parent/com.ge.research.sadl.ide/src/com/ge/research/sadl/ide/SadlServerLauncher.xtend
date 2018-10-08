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

import com.google.inject.Inject
import java.util.concurrent.Executors
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClientExtensions
import org.eclipse.xtext.ide.server.LanguageServerImpl
import org.eclipse.xtext.ide.server.LaunchArgs
import org.eclipse.xtext.ide.server.ServerLauncher
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.util.Modules2
import com.ge.research.sadl.ide.lsp.^extension.SadlLanguageServer

/**
 * LS launcher for SADL. This is the main entry point of the JAR file.
 * 
 * @author akos.kitta
 */
class SadlServerLauncher extends ServerLauncher {

	def static void main(String[] args) {
		// We need to silent the stream for the injector creation.
		// Save the stream into a local variable, then restore it for the `launch`.
		val originalStdIn = System.in;
		val originalStdOut = System.out;
		silentStandardStreams;
		new SADLIdeSetup().createInjectorAndDoEMFRegistration();
		System.setIn(originalStdIn);
		System.setOut(originalStdOut);
		launch(SadlServerLauncher.name, args, Modules2.mixin(new SadlServerModule, [
			bind(ServerLauncher).to(SadlServerLauncher)
			bind(LanguageServerImpl).to(SadlLanguageServer)
			bind(IResourceServiceProvider.Registry).toProvider(IResourceServiceProvider.Registry.RegistryProvider)
		]))
	}

	@Inject LanguageServerImpl languageServer;

	override start(LaunchArgs args) {
		val executorService = Executors.newCachedThreadPool;
		val launcher = Launcher.createIoLauncher(languageServer, LanguageClientExtensions, args.in, args.out,
			executorService, [it]);
		languageServer.connect(launcher.remoteProxy);
		val future = launcher.startListening;
		while (!future.done) {
			Thread.sleep(10_000L);
		}
	}

}
