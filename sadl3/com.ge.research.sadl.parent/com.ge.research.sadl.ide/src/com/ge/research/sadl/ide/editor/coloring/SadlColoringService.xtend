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
package com.ge.research.sadl.ide.editor.coloring

import com.ge.research.sadl.model.CircularDefinitionException
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SadlIsInverseOf
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlPropertyCondition
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.google.common.collect.ImmutableList
import com.google.inject.Inject
import java.util.Collections
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.lsp4j.ColoringInformation
import org.eclipse.lsp4j.Range
import org.eclipse.xtext.ide.server.Document
import org.eclipse.xtext.ide.server.coloring.IColoringService
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource

import static com.ge.research.sadl.ide.editor.coloring.SadlColoringStyle.*
import static com.ge.research.sadl.sADL.SADLPackage.Literals.*
import org.eclipse.emf.ecore.util.EcoreUtil

/**
 * Generic highlighting and coloring service for the {@code SADL} language.
 * 
 * @author akos.kitta
 * 
 * @see IColoringService
 */
class SadlColoringService implements IColoringService {

	@Inject
	extension DeclarationExtensions;

	@Override
	override getColoring(XtextResource resource, Document doc) {

		if (resource === null) {
			return emptyList;
		}

		val SadlModel model = resource.contents.head as SadlModel;
		if (model === null) {
			return emptyList;
		}
		val builder = ImmutableList.<ColoringInformation>builder;

		// Imported resource URIs.
		builder.add(doc.createInfos(model.imports, SADL_IMPORT__IMPORTED_RESOURCE, URI_ID));

		// Aliases (if any).
		if (model.alias !== null) {
			builder.add(doc.createInfos(model, SADL_MODEL__ALIAS, URI_ID));
		}

		// Make sure, the URI has different color, just like imported resources and alias.
		if (model.baseUri !== null) {
			builder.add(doc.createInfos(model, SADL_MODEL__BASE_URI, URI_ID));
		}

		model.eAllContents.forEach [
			val namedElement = if(it instanceof Name) name else it;
			switch (namedElement) {
				Name: {
					val id = if (namedElement.eContainer instanceof QueryStatement) {
							DEFAULT_ID;
						} else if (namedElement.function) {
							FUNCTION_NAME_ID;
						} else {
							VARIABLE_ID;
						};
					builder.add(doc.createInfos(it, SADL_RESOURCE__NAME, id));
				}
				SadlResource: {
					val id = if (namedElement instanceof Name && (namedElement as Name).function) {
							FUNCTION_NAME_ID;
						} else {
							getId(namedElement);
						};
					val nodes = NodeModelUtils.findNodesForFeature(it, SADL_RESOURCE__NAME);
					if (!nodes.nullOrEmpty) {
						val head = nodes.head;
						val last = nodes.last;
						val start = head.offset;
						val end = last.offset + last.length - start;
						builder.add(doc.createInfos(start, end, id));
					}
				}
				SadlPropertyCondition: {
					val id = getId(namedElement.property);
					builder.add(doc.createInfos(it, SADL_PROPERTY_CONDITION__PROPERTY, id));
				}
				SadlPropertyInitializer: {
					val id = getId(namedElement.property);
					builder.add(doc.createInfos(it, SADL_PROPERTY_INITIALIZER__PROPERTY, id));
				}
				SadlSimpleTypeReference: {
					val id = getId(namedElement.type);
					builder.add(doc.createInfos(it, SADL_SIMPLE_TYPE_REFERENCE__TYPE, id));
				}
				SadlIsInverseOf: {
					val id = getId(namedElement.otherProperty);
					builder.add(doc.createInfos(it, SADL_IS_INVERSE_OF__OTHER_PROPERTY, id));
				}
				default: {
					// Ignored case.
				}
			}
		];
		return builder.build;
	}

	private def createInfos(Document doc, EObject object, EStructuralFeature feature, Integer id) {
		return doc.createInfos(Collections.singleton(object), feature, id);
	}

	private def createInfos(Document doc, Iterable<? extends EObject> objects, EStructuralFeature feature, Integer id) {
		return objects.map[NodeModelUtils.findNodesForFeature(it, feature)].flatten.map [
			doc.createInfos(offset, length, id);
		];
	}

	private def createInfos(Document doc, int offset, int length, Integer id) {
		val range = new Range(doc.getPosition(offset), doc.getPosition(offset + length));
		return new ColoringInformation(range, Collections.singletonList(id));
	}

	private def getId(SadlResource it) {
		val type = try {
			ontConceptType;
		} catch (CircularDefinitionException e) {
			val uri = EcoreUtil.getURI(it);
			val message = '''Error while getting ontology concept type for SADL resource: «it». [URI: «uri»]''';
			new IllegalStateException(message, e).printStackTrace;
			e.definitionType;
		}
		switch (type) {
			case CLASS_PROPERTY: {
				return OBJECT_PROPERTY_ID;
			}
			case DATATYPE_PROPERTY: {
				return DATA_PROPERTY_ID;
			}
			case ANNOTATION_PROPERTY: {
				return ANNOTATION_PROPERTY_ID;
			}
			case RDF_PROPERTY: {
				return RDF_PROPERTY_ID;
			}
			case INSTANCE: {
				return INSTANCE_ID;
			}
			case CLASS: {
				return CLASS_ID;
			}
			case CLASS_LIST: {
				return CLASS_ID;
			}
			case DATATYPE: {
				return RDF_DATATYPE_ID;
			}
			case DATATYPE_LIST: {
				return CLASS_ID;
			}
			case FUNCTION_DEFN: {
				return FUNCTION_NAME_ID;
			}
			default: {
				return VARIABLE_ID;
			}
		}
	}

}
