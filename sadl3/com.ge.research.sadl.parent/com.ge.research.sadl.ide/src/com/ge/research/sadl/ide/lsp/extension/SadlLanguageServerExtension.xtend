/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.lsp.^extension

import com.google.common.base.Supplier
import com.google.common.base.Suppliers
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import java.util.List
import org.eclipse.lsp4j.jsonrpc.Endpoint
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethodProvider
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints
import org.eclipse.xtext.ide.server.ILanguageServerAccess
import org.eclipse.xtext.ide.server.ILanguageServerAccess.IBuildListener
import org.eclipse.xtext.ide.server.ILanguageServerExtension
import org.eclipse.xtext.ide.server.coloring.IColoringClient
import org.eclipse.xtext.ide.server.coloring.IColoringService
import org.eclipse.xtext.resource.IResourceDescription.Delta
import org.eclipse.xtext.resource.XtextResource

import static extension java.util.Collections.unmodifiableMap

/**
 * Language server extension for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SadlLanguageServerExtension.Impl)
interface SadlLanguageServerExtension extends ILanguageServerExtension {

	static class Impl implements ILanguageServerExtension, SadlLanguageServerExtension, IBuildListener, JsonRpcMethodProvider {

		@Inject
		extension IColoringService;

		ILanguageServerAccess access;
		Supplier<IColoringClient> client;

		@Override
		override initialize(ILanguageServerAccess access) {
			this.access = access;
			this.access.addBuildListener(this);
			this.client = Suppliers.memoize [
				ServiceEndpoints.toServiceObject(this.access.languageClient as Endpoint, IColoringClient)
			];
		}

		@Override
		override afterBuild(List<Delta> deltas) {
			deltas.map[uri.toString].forEach [
				access.<Void>doRead(it) [ ctx |
					if (ctx.documentOpen) {
						val resource = ctx.resource as XtextResource;
//						if (!resource.errors.nullOrEmpty) {
						val doc = ctx.document;
						val highlight = resource.getColoring(doc);
						client.get.updateColoring(highlight);
						return /*void*/ null;
//						}
					}
				];
			];
		}

		@Override
		override supportedMethods() {
			return ServiceEndpoints.getSupportedMethods(IColoringClient).unmodifiableMap;
		}

	}
}
