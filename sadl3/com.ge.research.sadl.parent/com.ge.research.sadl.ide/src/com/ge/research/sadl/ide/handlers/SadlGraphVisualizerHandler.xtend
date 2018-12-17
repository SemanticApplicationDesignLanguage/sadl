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
import com.ge.research.sadl.reasoner.ConfigurationException
import com.ge.research.sadl.reasoner.ResultSet
import com.google.inject.ImplementedBy
import java.io.IOException
import java.nio.file.Path
import java.util.Map

/**
 * Representation of a handler for graph creation and visualization.
 */
@ImplementedBy(Default)
interface SadlGraphVisualizerHandler {

	def void resultSetToGraph(Path path, ResultSet resultSet, String description, String baseFileName,
		Orientation orientation, Map<String, String> properties) throws ConfigurationException, IOException;

	static class Default implements SadlGraphVisualizerHandler {

		/**
		 * Does <b>nothing</b> by default. 
		 */
		override resultSetToGraph(Path path, ResultSet resultSet, String description, String baseFileName,
			Orientation orientation, Map<String, String> properties) throws ConfigurationException, IOException {
			// NOOP
		}

	}

}
