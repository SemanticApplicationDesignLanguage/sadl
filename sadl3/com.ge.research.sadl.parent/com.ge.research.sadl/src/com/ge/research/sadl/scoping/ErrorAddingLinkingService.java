package com.ge.research.sadl.scoping;

import static com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription.AMBIGUOUS_NAME_ALTERNATIVES;
import static com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription.AMBIGUOUS_NAME_ERROR;
import static com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription.AMBIGUOUS_NAME_ISSUE_CODE;

import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

import com.ge.research.sadl.utils.SadlASTUtils;
import com.google.inject.Inject;

public class ErrorAddingLinkingService extends DefaultLinkingService {

	private static final Logger logger = LoggerFactory.getLogger(ErrorAddingLinkingService.class);
	
	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;
	
	/**
	 * @return the first element returned from the injected {@link IScopeProvider} which matches the text of the passed
	 *         {@link INode node}
	 */
	@Override
	public List<EObject> getLinkedObjects(EObject context, EReference ref, INode node)
			throws IllegalNodeException {
		final EClass requiredType = ref.getEReferenceType();
		if (requiredType == null)
			return Collections.<EObject> emptyList();

		final String crossRefString = getCrossRefNodeAsString(node);
		if (crossRefString != null && !crossRefString.equals("")) {
			logger.debug("before getLinkedObjects: node: '" + crossRefString + "'");
			if (SadlASTUtils.isUnit(context)) {
				return Collections.emptyList();
			}
			final IScope scope = getScope(context, ref);
			QualifiedName qualifiedLinkName =  qualifiedNameConverter.toQualifiedName(crossRefString);
			IEObjectDescription eObjectDescription = scope.getSingleElement(qualifiedLinkName);
			logger.debug("after getLinkedObjects: node: '" + crossRefString + "' result: " + eObjectDescription);
			if (eObjectDescription != null) {
				String errorMessage = eObjectDescription.getUserData(AMBIGUOUS_NAME_ERROR);
				if (errorMessage != null && !alternativesAllSame(eObjectDescription)) {
					createAndAddDiagnostic(context.eResource(), node, errorMessage, eObjectDescription.getUserData(AMBIGUOUS_NAME_ALTERNATIVES));
				}
				List<EObject> results = Collections.singletonList(eObjectDescription.getEObjectOrProxy());
				return results;
			}
		}
		return Collections.emptyList();
	}
	
	/**
	 * This tests to see if the alternatives are all the same. If they are then it is not
	 * ambiguous as concepts with the same URI are the same concept. This can happen when 
	 * multiple projects are used, each with their own sadlimplicitmodel.
	 * @param eObjectDescription
	 * @return
	 */
	protected boolean alternativesAllSame(IEObjectDescription eObjectDescription) {
		String alternatives = eObjectDescription.getUserData(AMBIGUOUS_NAME_ALTERNATIVES);
		StringTokenizer st = new StringTokenizer(alternatives, ",");
		String lastToken = null;
		boolean allSame = true;
		while (st.hasMoreTokens()) {
			String thisToken = st.nextToken();
			if (lastToken != null && !lastToken.equals(thisToken)) {
				allSame = false;
				break;
			}
			lastToken = thisToken;
		}
		return allSame;
	}

	protected void createAndAddDiagnostic(Resource resource, INode node, String message, String commaSeparatedAlternatives) {
		resource.getErrors().add(new XtextLinkingDiagnostic(node, message, AMBIGUOUS_NAME_ISSUE_CODE, commaSeparatedAlternatives));
	}

	
}
