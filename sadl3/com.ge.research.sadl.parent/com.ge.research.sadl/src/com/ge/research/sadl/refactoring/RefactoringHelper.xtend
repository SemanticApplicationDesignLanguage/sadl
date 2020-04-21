package com.ge.research.sadl.refactoring

import com.google.inject.ImplementedBy

/**
 * Service to check whether refactoring whether is in progress or not.
 * In the headless case, it is always {@code false}. Eclipse UI can rebind the implementation.
 * @see https://github.com/GEGlobalResearch/DARPA-ASKE-TA1/issues/104#issuecomment-616746652
 */
@ImplementedBy(Default)
interface RefactoringHelper {

	def boolean isInProgress()

	static class Default implements RefactoringHelper {
		
		override isInProgress() {
			return false
		}

	}

}
