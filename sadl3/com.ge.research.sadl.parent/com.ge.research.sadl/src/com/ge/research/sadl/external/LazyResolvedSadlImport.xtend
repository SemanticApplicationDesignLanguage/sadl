/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.external

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.impl.SadlImportImpl
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.scoping.IScopeProvider

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

/**
 * SADL import that lazily resolves the imported SADL model based on its public URI.
 * 
 * @author akos.kitta
 */
class LazyResolvedSadlImport extends SadlImportImpl {

	val IScopeProvider provider;
	val String alias;
	val String baseUri;

	new(IScopeProvider provider, String alias, String baseUri) {
		super();
		this.provider = provider;
		this.alias = alias;
		this.baseUri = baseUri;
	}

	override getImportedResource() {
		val scope = provider.getScope(this, SADL_IMPORT__IMPORTED_RESOURCE);
		val importedModel = scope.getSingleElement(QualifiedName.create(baseUri));
		if (importedModel === null) {
			return null;
		}
		val importedResourceUri = importedModel.EObjectURI.trimFragment;
		val importedResource = eResource.resourceSet.getResource(importedResourceUri, true);
		return importedResource.contents.head as SadlModel;
	}

	override getAlias() {
		return alias;
	}

	override setImportedResource(SadlModel model) {
		throw new UnsupportedOperationException();
	}

	override setAlias(String newAlias) {
		throw new UnsupportedOperationException();
	}

}