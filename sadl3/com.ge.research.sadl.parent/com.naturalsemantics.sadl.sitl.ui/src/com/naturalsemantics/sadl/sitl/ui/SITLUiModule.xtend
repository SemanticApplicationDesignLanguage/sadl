/************************************************************************
 * 
 * Project: SADL
 * Copyright 2007-2022 - General Electric Company, All Rights Reserved
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
 * SADL Extension for SADL Import Template Language (SITL)
 * Copyright 2022 - Natural Semantics, LLC, All Rights Reserved
 */
package com.naturalsemantics.sadl.sitl.ui;

import com.ge.research.sadl.refactoring.RefactoringHelper
import com.ge.research.sadl.ui.contentassist.SadlReferenceProposalCreator
import com.ge.research.sadl.ui.outline.NoopOutlineRefreshJob
import com.ge.research.sadl.ui.refactoring.EclipseRefactoringHelper
import com.ge.research.sadl.ui.refactoring.SadlReferenceUpdater
import com.ge.research.sadl.ui.refactoring.SadlRenameContextFactory
import com.ge.research.sadl.ui.refactoring.SadlRenameRefactoringController
import com.ge.research.sadl.ui.refactoring.SadlRenameRefactoringExecuter
import com.ge.research.sadl.ui.refactoring.SadlResourceRenameStrategy
import com.google.inject.Provider
import com.naturalsemantics.sadl.sitl.ui.syntaxcoloring.SitlHighlightingConfiguration
import com.naturalsemantics.sadl.sitl.ui.syntaxcoloring.SitlSemanticHighlightingCalculator
import com.naturalsemantics.sadl.sitl.ui.syntaxcoloring.SitlTokenToAttributeIdMapper
import org.eclipse.ui.plugin.AbstractUIPlugin
import org.eclipse.xtext.ide.editor.syntaxcoloring.AbstractAntlrTokenToAttributeIdMapper
import org.eclipse.xtext.ide.editor.syntaxcoloring.ISemanticHighlightingCalculator
import org.eclipse.xtext.ui.editor.contentassist.AbstractJavaBasedContentProposalProvider.ReferenceProposalCreator
import org.eclipse.xtext.ui.editor.outline.impl.OutlineRefreshJob
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfiguration
import org.eclipse.xtext.ui.refactoring.IRenameStrategy
import org.eclipse.xtext.ui.refactoring.ui.IRenameContextFactory
import org.eclipse.xtext.ui.refactoring.ui.RenameRefactoringController
import org.eclipse.xtext.ui.refactoring.ui.RenameRefactoringExecuter

/**
 * Use this class to register components to be used within the Eclipse IDE.
 */
class SITLUiModule extends AbstractSITLUiModule {

	new(AbstractUIPlugin plugin) {
		super(plugin);
	}


	def Class<? extends IHighlightingConfiguration> bindILexicalHighlightingConfiguration() {
		return SitlHighlightingConfiguration
	}

	def Class<? extends ISemanticHighlightingCalculator> bindISemanticHighlightingCalculator() {
		return SitlSemanticHighlightingCalculator
	}

	def Class<? extends AbstractAntlrTokenToAttributeIdMapper> bindTokenToAttributeIdMapper() {
		return SitlTokenToAttributeIdMapper
	}

//	def void configurePreferenceInitializer(Binder binder) {
//		binder.bind(IPreferenceStoreInitializer).annotatedWith(Names.named("SitlPreferenceInitializer")).to(
//			SitlPreferencesInitializer)
//	}

//	def Class<? extends LanguageRootPreferencePage> bindLanguageRootPreferencePage() {
//		return SitlRootPreferencePage
//	}

	def Class<? extends ReferenceProposalCreator> bindReferenceProposalCreator() {
		return SadlReferenceProposalCreator
	}

//	def Class<? extends IOntologyContextProvider> bindIOntologyContextProvider() {
//		return SitlOntologyContextProvider
//	}

//	def Class<? extends IdeContentProposalProvider> bindIdeContentProposalProvider() {
//		return SitlIdeContentProposalProvider
//	}

//	def Class<? extends IdeCrossrefProposalProvider> bindIdeCrossrefProposalProvider() {
//		return SitlIdeCrossrefProposalProvider
//	}

//	@SingletonBinding(eager=true)
//	def Class<? extends ISitlAnswerProviders> bindISitlEditorStateManager() {
//		return SitlAnswerProviders
//	}

//	def Class<? extends IFoldingStructureProvider> bindIFoldingStructureProvider() {
//		return SitlFoldingStructureProvider;
//	}

//	def Class<? extends DefaultFoldingStructureProvider> bindDefaultFoldingStructureProvider() {
//		return SitlFoldingStructureProvider;
//	}

//	def Class<? extends IFoldingRegionProvider> bindIFoldingRegionProvider() {
//		return SitlFoldingFoldingRegionProvider;
//	}

//	def Class<? extends DefaultFoldingRegionProvider> bindDefaultFoldingRegionProvider() {
//		return SitlFoldingFoldingRegionProvider;
//	}

	override Class<? extends IRenameStrategy> bindIRenameStrategy() {
		return SadlResourceRenameStrategy;
	}

	override bindIReferenceUpdater() {
		return SadlReferenceUpdater;
	}

	def Class<? extends IRenameContextFactory> bindIRenameContextFactory() {
		return SadlRenameContextFactory;
	}

	def Provider<? extends RefactoringHelper> provideRefactoringHelper() {
		return [
			EclipseRefactoringHelper.INSTANCE
		];
	}

	def Provider<? extends EclipseRefactoringHelper> provideEclipseRefactoringHelper() {
		return [
			EclipseRefactoringHelper.INSTANCE
		];
	}

	def Class<? extends RenameRefactoringController> bindRenameRefactoringController() {
		return SadlRenameRefactoringController;
	}

	def Class<? extends OutlineRefreshJob> bindOutlineRefreshJob() {
		return NoopOutlineRefreshJob;
	}

	def Class<? extends RenameRefactoringExecuter> bindRenameRefactoringExecuter() {
		return SadlRenameRefactoringExecuter;
	}

}
