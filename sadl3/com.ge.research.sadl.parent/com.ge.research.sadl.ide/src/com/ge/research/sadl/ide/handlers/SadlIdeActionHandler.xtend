/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.handlers

import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.processing.ISadlInferenceProcessor
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider
import com.ge.research.sadl.reasoner.ResultSet
import com.ge.research.sadl.utils.ResourceManager
import com.ge.research.sadl.utils.SadlConsole
import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.common.base.Preconditions
import com.google.inject.Inject
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Map
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.preferences.IPreferenceValues
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.preferences.PreferenceKey
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.util.UriExtensions
import org.eclipse.xtext.validation.CheckMode

/**
 * IDE independent action handler that works with the inferencer under the hood.
 * 
 * @author akos.kitta
 */
class SadlIdeActionHandler {

	@Inject
	protected extension UriExtensions;

	@Inject
	protected SadlConsole console;

	@Inject
	protected SadlProjectHelper projectHelper;

	@Inject
	protected SadlInferenceProcessorProvider inferenceProcessorProvider;
	
	protected SadlGraphVisualizerHandler graphVisualizerHandler;
	protected ISadlInferenceProcessor inferenceProcessor;

	/**
	 * NOTE: This is mutable now. We can get rid of this after removing the Eclipse dependency from the graph generation.
	 */
	@Inject
	def void setGraphVisualizerHandler(SadlGraphVisualizerHandler graphVisualizerHandler) {
		this.graphVisualizerHandler = graphVisualizerHandler;
	}

	/**
	 * Prepares the Xtext resource by validating it and acquiring a inference processor for the give resource.
	 * Returns with a reference to the argument Xtext resource.  
	 */
	protected def XtextResource prepareResource(XtextResource it) {
		val validator = resourceServiceProvider.resourceValidator;
		validator.validate(it, CheckMode.FAST_ONLY, CancelIndicator.NullImpl);
		inferenceProcessor = inferenceProcessorProvider.getProcessor(it);
		return it;
	}

	/**
	 * Loads the resource from the resource set with the given FS path. If the resource exists, runs
	 * the validation on it and return with the resource. Otherwise, returns {@code null}. 
	 */
	protected def XtextResource findAndPrepareResource(ResourceSet resourceSet, Path resourcePath) {
		val uri = resourcePath.toUri.toEmfUri;
		val resource = resourceSet.getResource(uri, false);
		if (resource instanceof XtextResource) {
			return resource.prepareResource;
		}
		return null;
	}

	protected def void resultSetToGraph(Path path, ResultSet resultSet, String description, String baseFileName,
		Orientation orientation, Map<String, String> properties) {

		this.graphVisualizerHandler.resultSetToGraph(path, resultSet, description, baseFileName, orientation, properties);
	}

	protected def Path getOwlModelsFolderPath(Path it) {
		val root = projectHelper.getRoot(projectHelper.toUri(it));
		Preconditions.checkNotNull(root, '''Cannot locate contain SADL project for resource: «it»''');
		return Paths.get(root).resolve(ResourceManager.OWLDIR);
	}

	/**
	 * Returns with a map of preferences for the given resource.
	 */
	protected def Map<String, String> getPreferences(XtextResource it) {
		val preferences = newHashMap;
		val preferenceValuesProvider = resourceServiceProvider.get(IPreferenceValuesProvider);
		if (preferenceValuesProvider !== null) {
			val preferenceValues = preferenceValuesProvider.getPreferenceValues(it);
			preferences.putBoolean(preferenceValues, SadlPreferences.SHOW_TIMING_INFORMATION);
			preferences.putBoolean(preferenceValues, SadlPreferences.VALIDATE_BEFORE_TEST);
			preferences.putBoolean(preferenceValues, SadlPreferences.NAMESPACE_IN_QUERY_RESULTS);
		}
		return preferences;
	}

	protected def Map<String, String> putBoolean(Map<String, String> putHere, IPreferenceValues values,
		PreferenceKey key) {

		if (values === null) {
			return putHere;
		}
		putHere.put(key.id, Boolean.parseBoolean(String.valueOf(values.getPreference(key))).toString);
		return putHere;
	}

}
