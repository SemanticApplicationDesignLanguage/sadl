/************************************************************************
 * Copyright © 2007-2018 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.applications;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.builder.clustering.CurrentDescriptions;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.containers.IAllContainersState.Provider;
import org.eclipse.xtext.resource.containers.ResourceSetBasedAllContainersStateProvider;

import com.ge.research.sadl.SADLRuntimeModule;

/**
 * Customized SADL runtime module that contains a workaround for the
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=426212 issue.
 */
@SuppressWarnings("restriction")
public class SADLCliAppRuntimeModule extends SADLRuntimeModule {

	@Override
	public Class<? extends Provider> bindIAllContainersState$Provider() {
		return MyResourceSetBasedAllContainersStateProvider.class; // org.eclipse.xtext.ui.containers.ContainerStateProvider
	}

	public static class MyResourceSetBasedAllContainersStateProvider extends ResourceSetBasedAllContainersStateProvider
			implements Provider {

		@Override
		protected ResourceSet getResourceSet(IResourceDescriptions context) {
			if (context instanceof CurrentDescriptions) {
				Notifier target = ((CurrentDescriptions) context).getTarget();
				if (target instanceof ResourceSet) {
					return (ResourceSet) target;
				}
			}
			return super.getResourceSet(context);
		}

	}

}
