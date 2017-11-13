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

import java.util.Collection
import org.eclipse.xtext.ui.XtextUIMessages
import org.eclipse.xtext.ui.editor.hover.ProblemAnnotationHover

/**
 * Annotation hover that does not clip the message after 60 characters.
 * 
 * See: https://github.com/crapo/sadlos2/issues/251
 * 
 * @author akos.kitta
 */
class SadlProblemAnnotationHover extends ProblemAnnotationHover {

	override formatInfo(Collection<String> messages) {
		val sb = new StringBuilder();
		if (messages.size > 1) {
			sb.append(XtextUIMessages.AbstractHover_MultipleMarkers);
			val e = messages.iterator();
			while (e.hasNext) {
				sb.append("- ").append(e.next).append("\n")
			}
			sb.deleteCharAt(sb.length - 1);
		} else if (messages.size === 1) {
			sb.append(messages.iterator.next);
		}
		return sb.toString;
	}

}
