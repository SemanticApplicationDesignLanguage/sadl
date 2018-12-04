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
/*
 * generated by Xtext 2.9.0-SNAPSHOT
 */
package com.ge.research.sadl.validation

import com.ge.research.sadl.errorgenerator.generator.SadlErrorMessages
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.ge.research.sadl.resource.UserDataHelper
import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.sADL.SubjHasProp
import com.ge.research.sadl.utils.DependencyTraverserHelper
import com.ge.research.sadl.utils.ImportHelper
import com.ge.research.sadl.utils.SadlModelEquivalence
import com.google.inject.Inject
import java.util.ArrayList
import java.util.List
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.IResourceDescription
import org.eclipse.xtext.resource.IResourceDescriptionsProvider
import org.eclipse.xtext.validation.Check

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.impl.CompositeNodeWithSemanticElement
import com.ge.research.sadl.utils.SadlProjectHelper
import java.net.URI

/**
 * This class contains custom validation rules. 
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class SADLValidator extends AbstractSADLValidator {
	
	public static final String INVALID_MODEL_URI = "INVALID_MODEL_URI"
	public static final String INVALID_IMPORT_URI = "INVALID_IMPORT_URI"
	public static final String INVALID_MODEL_ALIAS = "INVALID_MODEL_ALIAS"
	public static final String MISSING_MODEL_ALIAS = "MISSING_MODEL_ALIAS"
	public static final String INVALID_MODEL_FILENAME = "INVALID_MODEL_FILENAME"
	public static final String UNBOUND_VARIABLE_IN_RULE_HEAD = "UNBOUND_VARIABLE_IN_RULE_HEAD"
	public static final String DUPLICATE_RULE_NAME = "DUPLICATE_RULE_NAME"
	public static final String UNRESOLVED_SADL_RESOURCE = "UNRESOLVED_SADL_RESOURCE"
	public static final String INVALID_COMMA_SEPARATED_ABREVIATED_EXPRESSION = "INVALID_COMMA_SEPARATED_ABREVIATED_EXPRESSION"
	public static final String CYCLIC_DEPENDENCY = "CYCLIC_DEPENDENCY"
	 
	@Inject DeclarationExtensions declarationExtensions
	@Inject IResourceDescriptionsProvider resourceDescriptionsProvider
	@Inject IResourceDescription.Manager resourceDescriptionManager
	@Inject UserDataHelper userDataHelper
	@Inject SadlProjectHelper projectHelper;

		
	protected var List<String> otherNames = new ArrayList
	
//	EStructuralFeature UNRESOLVED_SADL_RESOURCE
	
	// names of other structures, i.e., rules and named queries
	
	new() {
		otherNames.clear
	}

	@Check
	def checkSadlModel(SadlModel model) {
		initializeValidator()	// this class instance appears to be used repeatedly--need to clear this list for this resource usage this time
		val thisUri = model.baseUri
		val errMsg = SadlUtils.validateUri(thisUri);
		if (errMsg !== null) {
			error(errMsg, SADL_MODEL__BASE_URI, INVALID_MODEL_URI);
		}
		val resourceUri = model.eResource.URI
		val simpleFileName = resourceUri.lastSegment
		val thisResourceDescription = resourceDescriptionManager.getResourceDescription(model.eResource)
		val allModels = resourceDescriptionsProvider.getResourceDescriptions(model.eResource.resourceSet).getExportedObjectsByType(SADLPackage.Literals.SADL_MODEL)
		val projectRoot = try { projectHelper.getRoot(new URI(resourceUri.toString)); } catch (Exception e) { null };
		val allModelsPerProject = allModels.filter[
			// Nothing to filter if we cannot locate the SADL project root for the current resource.
			// Assuming a the headless tool-chain where all files are simply located in a folder on the FS.
			if (projectRoot === null) {
				return true;
			}
			val otherProjectRoot = projectHelper.getRoot(new URI(EObjectURI.trimFragment.toString));
			if (otherProjectRoot === null) {
				return false
			}
			return otherProjectRoot.toString == projectRoot.toString;
		];
		for (modelDescription : allModelsPerProject.filter[EObjectURI.trimFragment != thisResourceDescription.URI
			    && (!EObjectURI.isPlatformResource &&  !thisResourceDescription.URI.isPlatformResource 
			    || EObjectURI.segmentCount > 1 && thisResourceDescription.URI.segmentCount > 1
				&& EObjectURI.segment(1) == thisResourceDescription.URI.segment(1))]) {
			if (modelDescription.name.toString == thisUri) {
				val message = "This URI is already used in '" + modelDescription.EObjectURI.trimFragment + "'";
				error(message, SADL_MODEL__BASE_URI, INVALID_MODEL_URI)
			}
			if (model.alias !== null && userDataHelper.getAlias(modelDescription).orNull == model.alias) {
				val message = "The alias '"+model.alias+"' is already used in '" + modelDescription.EObjectURI.trimFragment + "'"
				error(message, SADL_MODEL__ALIAS, INVALID_MODEL_ALIAS)
			}
			if (modelDescription.EObjectURI.trimFragment.lastSegment == simpleFileName) {
				val message = "The simple filename (" + simpleFileName + ") is already used by model '" + modelDescription.EObjectURI.trimFragment + "'; filenames must be unique within a project."
				error(message, SADL_MODEL__BASE_URI, INVALID_MODEL_FILENAME)
			}
		}
		
		var imports = model.imports
		// does an import need any validation?
		if (imports !== null) {
			var itr = imports.iterator
			while (itr.hasNext) {
				var imp = itr.next;
				var importedURI = NodeModelUtils.findNodesForFeature(imp, SADL_IMPORT__IMPORTED_RESOURCE).map[text].join().trimQuotes
				val errorMsg = SadlUtils.validateUri(importedURI);
				if (errorMsg !== null) {
					error(errorMsg, imp, SADL_IMPORT__IMPORTED_RESOURCE);
				}
				if (importedURI == thisUri) {
					error("A model cannot import itself", imp, SADL_IMPORT__IMPORTED_RESOURCE)
				}
			}
		}
	}
	
	def Object[] getEObjectText(EObject po) {
		val r = po.eResource()
		if (r instanceof XtextResource) {
			val root = (r as XtextResource).getParseResult().getRootNode();
	        for(INode node : root.getAsTreeIterable()) {   
	        	if (node instanceof CompositeNodeWithSemanticElement) {
	        		val semElt = (node as CompositeNodeWithSemanticElement).getSemanticElement();
	        		if (semElt.equals(po)) {
	        			// this is the one!
	        			val txt = NodeModelUtils.getTokenText(node);
	        			var Object[] results = newArrayOfSize(3)
	        			results.set(0, node.getTotalLength());
	        			results.set(1, node.getTotalOffset());
       					results.set(2, txt.trim());
       					return results;
	        		}
	        	}
	        }
		}
		return null;
	}
	
	def trimQuotes(String string) {
		if (string.length > 0) {
			string.substring(1, string.length-1)
		}
		else {
			string
		}
	}
	
	@Check
	def checkRuleStatement(RuleStatement rule) {
		// make sure rule name is unique
		val thisName = declarationExtensions.getConcreteName(rule.name);
		
		if (otherNames.contains(thisName)) {
			var errMsg = "The name '" + thisName + "' in this namespace is already used."
			error(errMsg, SADLPackage.Literals.RULE_STATEMENT__NAME, DUPLICATE_RULE_NAME)
		}
		else {
			otherNames.add(thisName)			
		}
		// make sure all variables used in the head are bound in the body
		val itr = EcoreUtil2.getAllContents(rule.thens).filter(Name).toList.iterator
		while (itr.hasNext) {
			var name = itr.next
			if (!name.function && name.name.equals(name)) {
				val bodyitr = EcoreUtil2.getAllContents(rule.ifs).filter(Name).toList.iterator
				var foundInBody = false;
				while (bodyitr.hasNext) {
					var bName = bodyitr.next
					if (bName.name.equals(name)) {
						foundInBody = true
					}
				}
				if (!foundInBody) {
					var errMsg = "Rule conclusion contains variable '" + declarationExtensions.getConcreteName(name.name) + "' which is not bound in the rule premises."
					if (name.eContainer instanceof Name && (name.eContainer as Name).function) {
						warning(errMsg,  RULE_STATEMENT__THENS, UNBOUND_VARIABLE_IN_RULE_HEAD);
					}
					else {
					error(errMsg, RULE_STATEMENT__THENS, UNBOUND_VARIABLE_IN_RULE_HEAD);
					}
				}
			}
		}
	}
	
	@Check
	def checkQueryStatement(QueryStatement query) {
		// make sure rule name is unique
		if (query.name !== null && otherNames.contains(query.name)) {
			var errMsg = "The name '" + query.name + "' in this namespace is already used."
			error(errMsg, RULE_STATEMENT__NAME, DUPLICATE_RULE_NAME)
		}
	}
	
	@Check
	def checkSadlSimpleTypeReference(SadlSimpleTypeReference ref) {
		if (ref.type.name === null) {
			error("Undefined type", SADL_SIMPLE_TYPE_REFERENCE__TYPE, UNRESOLVED_SADL_RESOURCE)
		}
	}

//	@Check
//	def checkResourceName(SadlResource name) {
//		val nm = declarationExtensions.getConcreteName(name)
//		if (nm.startsWith("__")) {
//			error("", SADL_RESOURCE__NAME, <constant>)
//		}
//	}

	@Check
	def checkSadlResource(SadlResource sr) {
		var nm = null as String
		val nm1 = sr.name
		if (nm1 !== null) {
			nm = declarationExtensions.getConcreteName(nm1) 
		}
		if (nm === null) {
			nm = declarationExtensions.getConcreteName(sr)
		}
		if (nm === null) {
			try {
				if (sr instanceof Name) {
					val isFunc = (sr as Name).function
					if (!isFunc) {	
						error("Is this an undeclared variable?", SADL_RESOURCE__NAME, UNRESOLVED_SADL_RESOURCE)
					}
//					else {
//						// this might be a built-in so get the text and check the name
//						val srNode = NodeModelUtils.getNode(sr)
//						var boolean isBuiltin = false
//						if (srNode.hasChildren) {
//							val itr = srNode.children
//							for (c:itr) {
//								val txt = NodeModelUtils.getTokenText(c)
//	//							val b = RequirementsConstants.isFunctionConsideredBuiltin(null, txt)
//	//							if (b) {
//	//								isBuiltin = b
//	//							}
//							}
//						}
//						if (!isBuiltin) {
//							error("Is this an undeclared function?", SADL_RESOURCE__NAME, UNRESOLVED_SADL_RESOURCE)
//						}
//					}
				}
			}
			catch (Throwable t) {
				t.printStackTrace
			}
		}
		else {
			if (nm.contains(':')) {
				error("Invalid declaration of new concept with a QName from another namespace", SADL_RESOURCE__NAME)
			}
		}
	}
	
	@Check
	def checkCommaSeparatedAbreviatedExpression(SubjHasProp it) {
		// normally this would occur as a nested expression inside an "is" or other assignment (as object of a "with" or "has")
		if (eContainer instanceof BinaryOperation && comma) {
			val bop = eContainer as BinaryOperation
			val op = bop.op
			if (op.equals("and") || op.equals("or")) {
				 warning("Is this a declaration that should be nested in parentheses?", it, SUBJ_HAS_PROP__LEFT)	
			}
		}
	}
	
	@Check
	def checkHasDependencyCycle(SadlModel model) {
		val dependencies = ImportHelper.DEPENDENCIES; 
		val eqivalence = SadlModelEquivalence.INSTANCE;
		val cycle = new DependencyTraverserHelper().checkCycle(model, dependencies, eqivalence);
		if (cycle.present) {
			val message = SadlErrorMessages.CIRCULAR_IMPORT.get('''Dependency cycle was detected: «cycle.get.prettyPrint[baseUri]».''');
			error(message, model, SADL_MODEL__IMPORTS, CYCLIC_DEPENDENCY);
		}
	}
	
	/**
	 * This method initializes this instance of this validator class for use on a specified Resource
	 */
	def initializeValidator() {
		otherNames.clear
	}
}
