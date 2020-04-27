package com.ge.research.sadl.ui.refactoring

import org.eclipse.xtext.util.ITextRegion

import static org.eclipse.xtext.util.ITextRegionWithLineInformation.EMPTY_REGION

class RefactoringExtensions {

	def boolean isNullOrEmpty(ITextRegion region) {
		return region === null || EMPTY_REGION == region
	}

}
