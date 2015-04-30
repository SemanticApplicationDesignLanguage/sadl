/************************************************************************
 * Copyright © 2007-2014 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.resource;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import org.eclipse.xtext.resource.DerivedStateAwareResource;

/**
 * Appends an artificial whitespace when reading a file to work around the problem that
 * a dot at EOF is not detected as End Of Statement.
 */
public class SadlResource extends DerivedStateAwareResource {
	class SadlInputStream extends InputStream {
		private InputStream delegate;
		private boolean eof;

		public SadlInputStream (InputStream delegate) {
			this.delegate = delegate;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int read() throws IOException {
			if (eof)
				return -1;
			int value = delegate.read();
			if (value!=-1) {
				return value;
			} else {
				eof = true;
				return Character.valueOf(' ').charValue();
			}
		}

	}
	@Override
	protected void doLoad(InputStream inputStream, Map<?, ?> options)
			throws IOException {
		SadlInputStream is = new SadlInputStream(inputStream);
		super.doLoad(is, options);
	}
}
