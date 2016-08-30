package com.ge.research.sadl.scoping;

import java.util.Collections;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.linking.impl.DefaultLinkingService;
import org.eclipse.xtext.linking.impl.IllegalNodeException;
import org.eclipse.xtext.linking.impl.XtextLinkingDiagnostic;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.scoping.impl.MapBasedScope;

import com.google.inject.Inject;

public class ErrorAddingLinkingService extends DefaultLinkingService {

	public static final String ISSUE_CODE = "ambiguous_import";

	public static final String ERROR = "ERROR";
	public static final String ALTERNATIVES = "ALTERNATIVES";

	private static final Logger logger = Logger.getLogger(ErrorAddingLinkingService.class);
	
	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;
	
	/**
	 * @return the first element returned from the injected {@link IScopeProvider} which matches the text of the passed
	 *         {@link INode node}
	 */
	@Override
	public List<EObject> getLinkedObjects(EObject context, EReference ref, INode node)
			throws IllegalNodeException {
		Resource ctxrsrc = context.eResource();
		final EClass requiredType = ref.getEReferenceType();
		if (requiredType == null)
			return Collections.<EObject> emptyList();

		final String crossRefString = getCrossRefNodeAsString(node);
		if (crossRefString != null && !crossRefString.equals("")) {
			if (logger.isDebugEnabled()) {
				logger.debug("before getLinkedObjects: node: '" + crossRefString + "'");
			}
			final IScope scope = getScope(context, ref);
			QualifiedName qualifiedLinkName =  qualifiedNameConverter.toQualifiedName(crossRefString);
			IEObjectDescription eObjectDescription = scope.getSingleElement(qualifiedLinkName);
			if (logger.isDebugEnabled()) {
				logger.debug("after getLinkedObjects: node: '" + crossRefString + "' result: " + eObjectDescription);
			}
			if (eObjectDescription != null) {
				String errorMessage = eObjectDescription.getUserData(ERROR);
				if (errorMessage != null) {
					createAndAddDiagnostic(context.eResource(), node, errorMessage, eObjectDescription.getUserData(ALTERNATIVES));
				}
				List<EObject> results = Collections.singletonList(eObjectDescription.getEObjectOrProxy());
				return results;
			}
		}
		return Collections.emptyList();
	}
	
//	protected IScope getScope(EObject context, EReference ref) {
//		Resource rsrc = context.eResource();
//		IScope scope = TestScopeProvider.find(rsrc);
//		if (scope == null) { 
//			scope = super.getScope(context, ref);
//			if (scope instanceof MapBasedScope) {
//				TestScopeProvider.attach(rsrc, scope);
////				System.out.println("Attaching scope to '" + rsrc.toString() + "': " + scope.toString());
//			}
//		}
//		return scope;
//	}
	
	protected void createAndAddDiagnostic(Resource resource, INode node, String message, String commaSeparatedAlternatives) {
		resource.getErrors().add(new XtextLinkingDiagnostic(node, message, ISSUE_CODE, commaSeparatedAlternatives));
	}

	
}
