/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.helpers

import com.google.inject.Singleton

/**
 * Singleton service for providing the content for the {@code .project} file. It
 * is required to locate the {@code SADL} project root without a running
 * platform and workspace.
 * 
 * @author akos.kitta
 */
@Singleton
class DotProjectContentProvider {

	/**
	 * Returns with the content of the {@code .project} file for a project with a
	 * given name.
	 * 
	 * @param projectName
	 *            The name of the project to get the {@code .project} file content.
	 * 
	 * @return the {@code .project} file content for a project as a string.
	 */
	def String getContent(String projectName) '''
		<?xml version="1.0" encoding="UTF-8"?>
		<projectDescription>
			<name>«projectName»</name>
			<comment></comment>
			<projects>
			</projects>
			<buildSpec>
				<buildCommand>
					<name>org.eclipse.xtext.ui.shared.xtextBuilder</name>
					<arguments>
					</arguments>
				</buildCommand>
			</buildSpec>
			<natures>
				<nature>org.eclipse.xtext.ui.shared.xtextNature</nature>
			</natures>
		</projectDescription>
	'''

}
