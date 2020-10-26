/************************************************************************
 * Copyright © 2007-2014 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.jena;

import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.generic.AbstractGenericResourceRuntimeModule;

import com.ge.research.sadl.naming.SadlSimpleNameProvider;

public class JenaRuntimeModule extends AbstractGenericResourceRuntimeModule {
	@Override
	protected String getFileExtensions() {
		return "owl,nt,n3,rdf";
	}

	public Class<? extends IResourceFactory> bindIResourceFactory() {
		return JenaResourceFactory.class;
	}
	
	public Class<? extends IDefaultResourceDescriptionStrategy> bindIDefaultResourceDescriptionStrategy() {
		return JenaResourceDescriptionStrategy.class;
	}

	@Override
	protected String getLanguageName() {
		return "com.ge.research.sadl.Jena";
	}
	
	public Class<? extends IQualifiedNameProvider> bindIQualifiedNameProvider() {
		return SadlSimpleNameProvider.class;
	}

}
