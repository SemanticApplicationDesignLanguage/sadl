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