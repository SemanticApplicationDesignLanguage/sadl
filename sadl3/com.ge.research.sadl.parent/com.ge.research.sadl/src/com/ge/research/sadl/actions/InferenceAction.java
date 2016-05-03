package com.ge.research.sadl.actions;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.processing.SadlModelProcessorProvider;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class InferenceAction extends SadlAction {

	@Inject Provider<XtextResourceSet> resourceSetProvider;
	@Inject SadlModelProcessorProvider processorProvider;
	@Inject IPreferenceValuesProvider preferenceProvider;

	public InferenceAction(File prjFolder, File trgtFile, Map<String, String> prefMap) {
		setProjectFolder(prjFolder);
		setTargetFile(trgtFile);
		setPreferenceMap(prefMap);
	}
	
	public Object run(IProgressMonitor monitor) {
		try {
			String projectPath = getProjectFolder().getCanonicalPath();
			String targetFileName =getTargetFile().getCanonicalPath();
			if (resourceSetProvider != null) {
				XtextResourceSet resourceSet = resourceSetProvider.get();
				if (resourceSet != null) {
			    	Resource res = resourceSet.createResource(URI.createFileURI(targetFileName));
			    	if (res != null) {
						if (processorProvider != null) {
							IModelProcessor processor = processorProvider.getProcessor(res);
							processor.processCommands(res, new ValidationAcceptor(null),  new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(res)));
						}
			    	}
				}
			}
			return null;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
    	
	}

}
