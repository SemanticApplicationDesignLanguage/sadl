package com.ge.research.sadl.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;

import com.google.inject.Inject;

@SuppressWarnings("restriction")
public class SadlPreferencesProvider {
	
	private @Inject IPreferenceValuesProvider preferenceValuesProvider;

	public IPreferenceValues getPreferenceValues(Resource resource) {
		return preferenceValuesProvider.getPreferenceValues(resource);
	}

	public IPreferenceValues getPreferenceValues(IProject project) {
		if(project == null) {
			return getPreferenceValues(new ResourceImpl(URI.createURI("ws_ctx")));
		}
		return getPreferenceValues(new ResourceImpl(URI.createPlatformResourceURI(project.getName(), true)));
	}
}
