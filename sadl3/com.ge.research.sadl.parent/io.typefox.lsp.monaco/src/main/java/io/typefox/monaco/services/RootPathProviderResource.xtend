/**
 * Copyright (c) 2016 TypeFox GmbH (http://typefox.io)
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 */
package io.typefox.monaco.services

import javax.ws.rs.GET
import javax.ws.rs.Path
import javax.ws.rs.Produces

import static javax.ws.rs.core.MediaType.*
import com.google.inject.Provider
import com.google.inject.Inject
import org.apache.log4j.Logger

/**
 * REST endpoint for returning the root path for the language server.
 * 
 * @author akos.kitta
 */
@Path('/rootPathProvider')
class RootPathProviderResource {

	static val Logger LOGGER = Logger.getLogger(RootPathProviderResource);

	@Inject
	Provider<RootPathProviderService> provider;

	@GET
	@Produces(#[APPLICATION_XML, APPLICATION_JSON, TEXT_PLAIN])
	def String rootPath() {
		val path = provider.get().get();
		LOGGER.info('''Root path is under: «path».''');
		return path;
	} 
	
}