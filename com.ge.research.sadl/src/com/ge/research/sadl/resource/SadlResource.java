package com.ge.research.sadl.resource;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import org.eclipse.xtext.linking.lazy.LazyLinkingResource;

/**
 * Appends an artificial whitespace when reading a file to work around the problem that
 * a dot at EOF is not detected as End Of Statement.
 */
public class SadlResource extends LazyLinkingResource {
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
