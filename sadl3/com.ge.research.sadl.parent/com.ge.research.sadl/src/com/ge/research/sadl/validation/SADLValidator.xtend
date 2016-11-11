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

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.resource.ResourceDescriptionStrategy
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.IResourceDescription
import org.eclipse.xtext.resource.IResourceDescriptionsProvider
import org.eclipse.xtext.validation.Check
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.ge.research.sadl.sADL.SadlResource
import java.util.List
import java.util.ArrayList
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import org.eclipse.emf.ecore.EStructuralFeature
import com.ge.research.sadl.sADL.SubjHasProp

/**
 * This class contains custom validation rules. 
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class SADLValidator extends AbstractSADLValidator {
	
	@Inject DeclarationExtensions declarationExtensions
	@Inject IResourceDescriptionsProvider resourceDescriptionsProvider
	@Inject IResourceDescription.Manager resourceDescriptionManager

	public static final String INVALID_MODEL_URI = "INVALID_MODEL_URI"
	public static final String INVALID_IMPORT_URI = "INVALID_IMPORT_URI"
	public static final String INVALID_MODEL_ALIAS = "INVALID_MODEL_ALIAS"
	public static final String MISSING_MODEL_ALIAS = "MISSING_MODEL_ALIAS"
	public static final String INVALID_MODEL_FILENAME = "INVALID_MODEL_FILENAME"
	public static final String UNBOUND_VARIABLE_IN_RULE_HEAD = "UNBOUND_VARIABLE_IN_RULE_HEAD"
	public static final String DUPLICATE_RULE_NAME = "DUPLICATE_RULE_NAME"
	public static final String UNRESOLVED_SADL_RESOURCE = "UNRESOLVED_SADL_RESOURCE"
		
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
		if (errMsg != null) {
			error(errMsg, SADLPackage.Literals.SADL_MODEL__BASE_URI, INVALID_MODEL_URI);
		}
		val thisRsrc = model.eResource
		val emfURI = thisRsrc.URI;
		val simpleFileName = emfURI.lastSegment
		val thisResourceDescription = resourceDescriptionManager.getResourceDescription(model.eResource)
		val allModels = resourceDescriptionsProvider.getResourceDescriptions(model.eResource.resourceSet).getExportedObjectsByType(SADLPackage.Literals.SADL_MODEL)
		for (modelDescription : allModels.filter[EObjectURI.trimFragment != thisResourceDescription.URI
			    && (!EObjectURI.isPlatformResource &&  !thisResourceDescription.URI.isPlatformResource 
			    || EObjectURI.segmentCount > 1 && thisResourceDescription.URI.segmentCount > 1
				&& EObjectURI.segment(1) == thisResourceDescription.URI.segment(1))]) {
			if (modelDescription.name.toString == thisUri) {
				error("This URI is already used in '" + modelDescription.EObjectURI.trimFragment + "'", SADLPackage.Literals.SADL_MODEL__BASE_URI, INVALID_MODEL_URI)
			}
			if (model.alias !== null && modelDescription.getUserData(ResourceDescriptionStrategy.USER_DATA_ALIAS) == model.alias) {
				error("The alias '"+model.alias+"' is already used in '" + modelDescription.EObjectURI.trimFragment + "'", SADLPackage.Literals.SADL_MODEL__ALIAS, INVALID_MODEL_ALIAS)
			}
			if (modelDescription.EObjectURI.trimFragment.lastSegment == simpleFileName) {
				error("The simple filename (" + simpleFileName + ") is already used by model '" + modelDescription.EObjectURI.trimFragment + "'; filenames must be unique within a project.", SADLPackage.Literals.SADL_MODEL__BASE_URI, INVALID_MODEL_FILENAME)
			}
		}
		
		var imports = model.imports
		// does an import need any validation?
		if (imports != null) {
			var itr = imports.iterator
			while (itr.hasNext) {
				var imp = itr.next;
				var importedURI = NodeModelUtils.findNodesForFeature(imp, SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE).map[text].join().trimQuotes
				val errorMsg = SadlUtils.validateUri(importedURI);
				if (errorMsg != null) {
					error(errorMsg, imp, SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE);
				}
				if (importedURI == thisUri) {
					error("A model cannot import itself", imp, SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE)
				}
			}
		}
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
		if (otherNames.contains(rule.name)) {
			var errMsg = "The name '" + rule.name + "' in this namespace is already used."
			error(errMsg, SADLPackage.Literals.RULE_STATEMENT__NAME, DUPLICATE_RULE_NAME)
		}
		otherNames.add(rule.name)
		// make sure all variables used in the head are bound in the bod
		val itr = EcoreUtil2.getAllContents(rule.thens).filter(Name).toList.iterator
		while (itr.hasNext) {
			var name = itr.next
			if (!name.function && name.name.equals(name)) {
				var errMsg = "Rule conclusion contains variable '" + declarationExtensions.getConcreteName(name) + "' which is not bound in the rule premises."
				error(errMsg, SADLPackage.Literals.RULE_STATEMENT__THENS, UNBOUND_VARIABLE_IN_RULE_HEAD);
			}
		}
	}
	
	@Check
	def checkQueryStatement(QueryStatement query) {
		// make sure rule name is unique
		if (query.name != null && otherNames.contains(query.name)) {
			var errMsg = "The name '" + query.name + "' in this namespace is already used."
			error(errMsg, SADLPackage.Literals.RULE_STATEMENT__NAME, DUPLICATE_RULE_NAME)
		}
	}
	
	@Check
	def checkSadlSimpleTypeReference(SadlSimpleTypeReference sstr) {
		val type = sstr.type
		val nm = type.name
		if (nm == null) {
			error("Undefined type", SADLPackage.Literals.SADL_SIMPLE_TYPE_REFERENCE__TYPE, UNRESOLVED_SADL_RESOURCE)
		}
	}

//	@Check
//	def checkResourceName(SadlResource name) {
//		val nm = declarationExtensions.getConcreteName(name)
//		if (nm.startsWith("__")) {
//			error("", SADLPackage.Literals.SADL_RESOURCE__NAME, <constant>)
//		}
//	}

	@Check
	def checkSadlResource(SadlResource sr) {
		var nm = null as String
		var isProxy = sr.eIsProxy
		val nm1 = sr.name
		if (nm1 != null) {
			nm = declarationExtensions.getConcreteName(nm1) 
		}
		else {
			nm = declarationExtensions.getConcreteName(sr)
		}
		if (nm == null) {
			try {
				if (sr instanceof Name) {
					val isFunc = (sr as Name).function
					if (!isFunc) {	
						error("Is this an undeclared variable?", SADLPackage.Literals.SADL_RESOURCE__NAME, UNRESOLVED_SADL_RESOURCE)
					}
					else {
						// this might be a built-in so get the text and check the name
						val srNode = NodeModelUtils.getNode(sr)
						var boolean isBuiltin = false
						if (srNode.hasChildren) {
							val itr = srNode.children
							for (c:itr) {
								val txt = NodeModelUtils.getTokenText(c)
	//							val b = RequirementsConstants.isFunctionConsideredBuiltin(null, txt)
	//							if (b) {
	//								isBuiltin = b
	//							}
							}
						}
						if (!isBuiltin) {
							error("Is this an undeclared function?", SADLPackage.Literals.SADL_RESOURCE__NAME, UNRESOLVED_SADL_RESOURCE)
						}
					}
				}
			}
			catch (Throwable t) {
				t.printStackTrace
			}
		}
	}
	
	/**
	 * This method initializes this instance of this validator class for use on a specified Resource
	 */
	def initializeValidator() {
		otherNames.clear
	}
}
