/************************************************************************
 * Copyright © 2023 - Natural Semantics, LLC. All Rights Reserved.
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
package com.ge.research.sadl.jena.reasoner;

import java.util.List;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.TranslationException;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper;
import com.naturalsemanticsllc.sadl.reasoner.TypedBuiltinFunctionException;

public class SimpleJenaTypedBuiltinFunctionHelper implements ITypedBuiltinFunctionHelper {

	private ITranslator translator = null;
	
	
	@Override
	public Node[] validateArgumentTypes(BuiltinElement be, Object model, List<Node> args, List<Node> argTypes)
			throws TypedBuiltinFunctionException, ConfigurationException, TranslationException {
		if (be.getFuncUri() != null && be.getFuncUri().equals("http://sadl.org/builtinfunctions#combineUnits")) {
			for (Node argType : argTypes) {
				if (!argType.getURI().equals(XSD.xstring.getURI())) {
					throw new TypedBuiltinFunctionException("combineUnits only takes string arguments");
				}
			}
			Node[] retTypes = new Node[1];
			retTypes[0] = new NamedNode(XSD.xstring.getURI(), NodeType.DataTypeNode);
			return retTypes;
		}	
		ITranslator trans = getTranslator();
		if (trans != null && model instanceof OntModel) {
			if (be.getExternalUri() == null) {
				if (be.getFuncUri() == null) {
					String biUri = SadlConstants.SADL_BUILTIN_FUNCTIONS_URI + "#" + trans.builtinTypeToString(be);
					be.setFuncUri(biUri);
				}
				try {
					addExternalUri(be, (OntModel) model);
				}
				catch (TypedBuiltinFunctionException e) {
					// is this a implied operator?
				}
			}
			Node[] retTypes = trans.validateArgumentTypes(be, (OntModel)model, args, argTypes);
			if (retTypes != null) {
				return retTypes;
			}
		}
		return null;
	}

	private void addExternalUri(BuiltinElement be, OntModel m) throws TypedBuiltinFunctionException {
		Individual subject = m.getIndividual(be.getFuncUri());
		if (subject != null) {
			StmtIterator sitr = m.listStatements(subject, m.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI), (RDFNode)null);
			if (sitr.hasNext()) {
				String euri = sitr.nextStatement().getObject().toString();
				be.setExternalUri(euri);
			}
		}
		else {
			throw new TypedBuiltinFunctionException(be.getFuncUri() + " not found in model.");
		}
	}

	@Override
	public ITranslator getTranslator() {
		return translator;
	}

	@Override
	public void setTranslator(ITranslator translator) {
		this.translator = translator;
	}

	@Override
	public String combineUnits(String operator, String arg1Units, String arg2Units) throws TypedBuiltinFunctionException {
		ITranslator theTranslator = getTranslator();
		if (theTranslator instanceof JenaTranslatorPlugin) {
			BuiltinType builtinType = ((JenaTranslatorPlugin)theTranslator).reasonerBuiltinNameToBuiltinType(operator);
			if (builtinType != null) {
				UnittedQuantityBuiltinHandlingType biUqType = ((JenaTranslatorPlugin)theTranslator).getUnittedQuantityBuiltinHandlingTypeOfCommonBuiltins(builtinType);
				if (biUqType != null) {
					if (biUqType.equals(UnittedQuantityBuiltinHandlingType.SameUnitsRequired)) {
						if (arg1Units != null && arg2Units != null && arg1Units.equals(arg2Units)) {
							return arg1Units;
						}
						else {
							throw new TypedBuiltinFunctionException("Units (" + arg1Units + ", " + arg2Units + ") for operation '" + operator + "' must be the same.");
						}
					}
					else if (biUqType.equals(UnittedQuantityBuiltinHandlingType.DifferentUnitsAllowedOrLeftOnly)) {
						if (arg1Units != null) {
							if (arg2Units == null) {
								return arg1Units;
							}
							else {
								if (operator.equals(BuiltinType.Multiply.toString())) {
									return arg1Units + "*" + arg2Units;
								}
								else if (operator.equals(BuiltinType.Divide.toString())) {
									return arg1Units + "/" + arg2Units;
								}
								else {
									return arg1Units + operator + arg2Units;
								}
							}
						}
						else {
							throw new TypedBuiltinFunctionException("Units for operation '" + operator + "' must have at least the first argument units not null.");
						}
					}
					else {
						throw new TypedBuiltinFunctionException("Unhandled UnittedQuantityBuiltinHandlingType: " + biUqType.toString());
					}
				}
			}
		}
		return arg1Units + operator + arg2Units;
	}

	@Override
	public Object[] performUnitConversions(Object operator, Number arg1Value, Object arg1Units, Number arg2Value,
			Object arg2Units) throws TypedBuiltinFunctionException {
		throw new TypedBuiltinFunctionException("This handler does not support unit conversion");
	}

	@Override
	public UnittedQuantityBuiltinHandlingType getUnittedQuantityBuiltinHandlingTypeOfBuiltinFromTranslator(
			String builtinUri) {
		return getTranslator().getUnittedQuantityBuiltinHandlingTypeOfBuiltin(builtinUri);
	}

}
