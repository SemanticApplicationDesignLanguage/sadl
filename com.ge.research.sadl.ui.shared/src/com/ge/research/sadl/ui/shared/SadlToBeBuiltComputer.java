package com.ge.research.sadl.ui.shared;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.xtext.builder.impl.ToBeBuilt;
import org.eclipse.xtext.builder.impl.javasupport.JdtToBeBuiltComputer;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.utils.SadlUtils;
import com.google.inject.Provider;

public class SadlToBeBuiltComputer extends JdtToBeBuiltComputer {
	
	@Inject
	private Provider<IResourceDescriptions> indexProvider;
	
	@Inject
	private SadlModelManagerProvider sadlModelManagerProvider;

	private SadlUtils sadlUtils = null;
	
	@Override
	public ToBeBuilt updateProject(IProject project, IProgressMonitor monitor) throws CoreException {
		// TODO: Move to customized XtextBuilder and do on a full build:
		IResourceDescriptions index = indexProvider.get();
		for (IResourceDescription desc: index.getAllResourceDescriptions()) {
			// if desc.getURI() is external then remove
			System.out.println(desc.getURI());
		}
		
		
		// TODO: Compute external URIs used by the project
		List<URI> externals = null;
		try {
			String prjLocation = getSadlUtils().fileNameToFileUrl(project.getLocation().toPortableString());
			SadlModelManager visitor = sadlModelManagerProvider.get(URI.createURI(prjLocation));
			if (visitor != null) {
				IConfigurationManagerForIDE cmgr = visitor.getConfigurationMgr(prjLocation + ResourceManager.OWLDIR);
				if (cmgr != null) { 
					externals = cmgr.getExternalModelURIs();
				}
			}
						
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ToBeBuilt toBeBuilt = super.updateProject(project, monitor);

		if (externals != null && externals.size() > 0) {
			toBeBuilt.getToBeUpdated().addAll(externals);
		}
		
		// TODO: Which URIs can be deleted from the index? project.getToBeDeleted() or project.getAndRemoveToBeDeleted()
		
		return toBeBuilt;
	}

	private SadlUtils getSadlUtils() {
		if (sadlUtils  == null) {
			sadlUtils = new SadlUtils();
		}
		return sadlUtils;
	}
}
