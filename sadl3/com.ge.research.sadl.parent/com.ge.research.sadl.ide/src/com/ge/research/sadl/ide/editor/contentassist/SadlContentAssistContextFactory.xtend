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
package com.ge.research.sadl.ide.editor.contentassist

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableList.Builder
import com.google.common.collect.Iterables
import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.antlr.ContentAssistContextFactory

/**
 * Factory that always considers and creates a CA context with the last complete node.
 * 
 * <p>
 * This is for handling such cases:
 * <pre>
 * Bar is a class.
 * MyBar is a Bar.
 * Foo is a class described by p1 with values of type Bar. 
 * MyFoo is a Foo with p1 M<|>
 * </pre>
 * 
 * <p>
 * We have already passed the property initializer assignment but still want to propose it with prefixes.
 * 
 * @author akos.kitta
 */
class SadlContentAssistContextFactory extends ContentAssistContextFactory {

	@Inject
	Provider<org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext.Builder> builderProvider;

	override protected ContentAssistContext[] doCreateContexts(int offset) {
		val contexts = super.doCreateContexts(offset);
		if (datatypeNode != lastCompleteNode || completionOffset === lastCompleteNode.offset) {
			initializeFromViewerAndResource(offset);
			handleLastCompleteNodeIsAtEndOfDatatypeNode();
			val Builder<ContentAssistContext> builder = ImmutableList.builder();
			builder.add(contextBuilders.map[apply]);
			builder.add(contexts);
			return Iterables.toArray(builder.build(), ContentAssistContext).adjustContextsWithoutPrefix;
		}
		return contexts.adjustContextsWithoutPrefix;
	}

	/**
	 * If there is a single, common non-empty prefix, all context will have the same prefix.
	 */
	private def ContentAssistContext[] adjustContextsWithoutPrefix(ContentAssistContext[] contexts) {
		val commonNonEmptyPrefixes = contexts.map[prefix].filter[!nullOrEmpty].toSet;
		if (commonNonEmptyPrefixes.length === 1) {
			val adjustedContexts = newArrayList;
			for (var i = 0; i < contexts.length; i++) {
				val it = contexts.get(i);
				if (prefix.nullOrEmpty) {
					val builder = builderProvider.get;
					builder.setPrefix(commonNonEmptyPrefixes.head)
					builder.setSelectedText(selectedText)
					builder.setRootModel(rootModel)
					builder.setRootNode(rootNode)
					builder.setCurrentModel(currentModel)
					builder.setPreviousModel(previousModel)
					builder.setCurrentNode(currentNode)
					builder.setLastCompleteNode(lastCompleteNode)
					builder.setOffset(offset)
					builder.setReplaceRegion(replaceRegion)
					builder.setResource(resource)
					for (grammarElement : firstSetGrammarElements) {
						builder.accept(grammarElement)
					}
					adjustedContexts.add(builder.apply)
				} else {
					adjustedContexts.add(it);
				}
			}
			return adjustedContexts;
		}
		return contexts;
	}

}
