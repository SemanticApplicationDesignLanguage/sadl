/************************************************************************
 * Copyright ? 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.ui.editor;

import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.xtext.resource.XtextResourceSet;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Inject;
import com.google.inject.Injector;

public class ValidateModel extends SadlActionDelegate implements IObjectActionDelegate {

    @Inject
    private XtextResourceSet resourceSet;
    
    @Inject
	private SadlModelManagerProvider sadlModelManagerProvider;

	public ValidateModel () {
		Injector injector = SadlActivator.getInstance().getInjector("com.ge.research.sadl.Sadl");//new SadlStandaloneSetup().createInjectorAndDoEMFRegistration();
		injector.injectMembers(this);
	}

	@Override
	protected void run(IPath testFilePath) {
		SadlModelManager visitor = sadlModelManagerProvider.get(URI.createURI(testFilePath.toString()));
		prepareModel(visitor, testFilePath, resourceSet);
		String modelName = visitor.getModelName();

	    SadlConsole.writeToConsole(MessageType.INFO, "Validating model: " + testFilePath.lastSegment() + "(" + modelName + ")\n");
		int errorCnt = visitor.validateModel(modelName, null);
		SadlConsole.displayMessages(visitor);
	}

}
