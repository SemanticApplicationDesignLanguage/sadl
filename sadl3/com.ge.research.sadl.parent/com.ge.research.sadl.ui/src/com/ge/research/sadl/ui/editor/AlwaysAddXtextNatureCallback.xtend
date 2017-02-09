/** 
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 * Project: SADL
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 */
package com.ge.research.sadl.ui.editor

import com.google.inject.Inject
import org.eclipse.xtext.builder.nature.ToggleXtextNatureAction
import org.eclipse.xtext.ui.editor.IXtextEditorCallback.NullImpl
import org.eclipse.xtext.ui.editor.XtextEditor

/** 
 * Simple callback that adds the Xtext nature to the project if it does 
 * not have when opening up the {@code SADL} editor.
 * @author akos.kitta
 */
class AlwaysAddXtextNatureCallback extends NullImpl {
	
	@Inject 
	extension ToggleXtextNatureAction;
	
	@Override
	override afterCreatePartControl(XtextEditor editor) {
		val extension project = editor?.resource?.project;
		if (project !== null && accessible && !hidden && !hasNature(project)) {
			project.toggleNature;
		}
	}
	
}
