/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.markers

import com.google.common.base.Preconditions
import java.util.List
import org.eclipse.xtend.lib.annotations.Data

/**
 * Representation of a SADL marker.
 * 
 * @author akos.kitta
 */
@Data
class SadlMarker {

	/**
	 * Copies the marker argument and returns with a new instance after setting the missing model URI.
	 * Throws an exception if the model URI is neither empty nor {@code null}.
	 */
	static def copyWithModelUri(SadlMarker it, String newModelUri) {
		Preconditions.checkArgument(!isModelUriSet, '''Expected empty or null model URI. Was: '«modelUri»'.''');
		return new SadlMarker(newModelUri, message, astNodeName, severity, references as List<SadlMarkerRef>);
	}

	/**
	 * Returns with the URI of the model where this marker belongs to. 
	 */
	val String modelUri;

	/**
	 * Returns with the human readable message of this marker.
	 * Eventually, this is the description of the marker.
	 */
	val String message;

	/**
	 * The unique name of the AST element this marker attached to in the resource.
	 */
	val String astNodeName;

	/**
	 * The severity of the marker.
	 */
	val SadlMarkerSeverity severity;
	
	/**
	 * Zero to any references for references.
	 */
	val List<SadlMarkerRef> references;

	/**
	 * Returns with {@code true} if the model URI is neither {@code null} nor empty string.
	 */
	def boolean isModelUriSet() {
		return modelUri !== null && !modelUri.empty;
	}
	
	/**
	 * Resolves the reference by altering the internal state of the current marker.
	 */
	def resolveRef(SadlMarkerRef it, String resolvedReferenceId) {
		val index = references.indexOf(it);
		Preconditions.checkState(index >= 0, '''Reference «it» does not exist on marker: «this»''');
		val unresolvedRef = references.get(index);
		Preconditions.checkState(!unresolvedRef.resolved, '''Expected unresolved SADL marker reference. Ref: «unresolvedRef»''');
		val resolvedRef = new SadlMarkerRef(resolvedReferenceId, unresolvedRef.type, true);
		references.set(index, resolvedRef);
		return this;
	}

}

@Data
class SadlMarkerRef {
	
	/**
	 * Unresolved SADL marker reference.
	 */
	new(String referenceId, SadlMarkerRefType type) {
		this(referenceId, type, false);
	}
	
	new(String referenceId, SadlMarkerRefType type, boolean resolved) {
		this.referencedId = referenceId;
		this.type = type;
		this.resolved = resolved;
	}

	/**
	 * The identifier of the reference model element or file. If not yet {@link SadlMarkerRef#isResolved() resolved}, 
	 * then it is {@code "http://sadl.org/File.sadl#modelName"} if the type if model element, and {@code "/path/to/file"}.
	 */
	val String referencedId;

	/**
	 * File of model element. The reference ID has to be resolved based on this.
	 */
	val SadlMarkerRefType type;

	/**
	 * {@code true} if the {@link SadlMarkerRef#getReferencedId() referenceId} has been resolved and substituted. Otherwise, {@code false}. 
	 */
	val boolean resolved;
	
}

enum SadlMarkerRefType {

	/**
	 * If the SADL marker references a model element. Such as a SADL resource {@code AResource} from a {@code http://sadl.org/A.sadl}.
	 * So it will be {@code "http://sadl.org/A.sadl#AResource}.
	 */
	ModelElement,

	/**
	 * If the marker references any arbitrary file from the project. The path must be a project root relative path with forward slashes.
	 * This should be robust and the followings should be the same {@code "/folder/file"}, {@code "folder/file"} and {@code "folder/file/"}.
	 */
	File

}
