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
package com.ge.research.sadl.scoping;

import org.eclipse.xtext.linking.impl.ImportedNamesAdapter;
import org.eclipse.xtext.scoping.IScope;

/**
 * Scope wrapper/imported names adapter that avoids logging
 * {@code "getElements should be called with a QualifiedName during linking"}
 * when retrieving {@link IScope#getAllElements() all elements} from a scope in
 * the scope provider.
 * 
 * <p>
 * See:
 * <ul>
 * <li><a href=
 * "https://github.com/crapo/sadlos2/issues/228">https://github.com/crapo/sadlos2/issues/228</a></li>
 * <li><a href=
 * "https://gitter.im/eclipse/xtext?at=59a7c74066c1c7c477fd74dd">https://gitter.im/eclipse/xtext?at=59a7c74066c1c7c477fd74dd</a></li>
 * </ul>
 * 
 * @author akos.kitta
 */
public class SilencedImportedNamesAdapter extends ImportedNamesAdapter {

	@Override
	public IScope wrap(IScope scope) {
		return new SilentWrappingScope(scope);
	}

	protected class SilentWrappingScope extends WrappingScope {

		SilentWrappingScope(IScope scope) {
			super(scope);
		}

		@Override
		protected void handleNoNameQuery() {
			// NOOP
		}

	}

}
