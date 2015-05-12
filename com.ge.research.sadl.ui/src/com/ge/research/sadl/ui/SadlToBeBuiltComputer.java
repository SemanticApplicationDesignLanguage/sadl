package com.ge.research.sadl.ui;

import javax.inject.Inject;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.xtext.builder.impl.ToBeBuilt;
import org.eclipse.xtext.builder.impl.javasupport.JdtToBeBuiltComputer;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;

import com.google.inject.Provider;

public class SadlToBeBuiltComputer extends JdtToBeBuiltComputer {
	@Inject
	private Provider<IResourceDescriptions> indexProvider;
	
	@Override
	public ToBeBuilt updateProject(IProject project, IProgressMonitor monitor) throws CoreException {
		// TODO: Move to customized XtextBuilder and do on a full build:
		IResourceDescriptions index = indexProvider.get();
		for (IResourceDescription desc: index.getAllResourceDescriptions()) {
			// if desc.getURI() is external then remove
		}
		
		
		// TODO: Compute external URIs used by the project
		URI uri = URI.createURI("http://sadl.sourceforge.net/owl/apvf.owl");
		ToBeBuilt toBeBuilt = super.updateProject(project, monitor);

		// TODO: Which URIs can be deleted from the index?
		toBeBuilt.getToBeUpdated().add(uri);
		
		return toBeBuilt;
	}
}
