/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.validation

import com.ge.research.sadl.sADL.SADLPackage
import org.eclipse.xtext.linking.ILinkingDiagnosticMessageProvider.ILinkingDiagnosticContext
import org.eclipse.xtext.linking.impl.LinkingDiagnosticMessageProvider

class SoftLinkingMessageProvider extends LinkingDiagnosticMessageProvider {

	override getUnresolvedProxyMessage(ILinkingDiagnosticContext context) {
		if (context.reference.EReferenceType === SADLPackage.Literals.SADL_RESOURCE) {
			// treated as declaration. 
			return null
		}
		super.getUnresolvedProxyMessage(context)
	}

}