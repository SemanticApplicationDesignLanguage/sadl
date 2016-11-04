/**
 * Copyright (c) 2016 TypeFox GmbH (http://typefox.io)
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 */
package io.typefox.monaco.services

import com.google.common.base.StandardSystemProperty
import com.google.common.base.Supplier
import com.google.common.base.Suppliers
import com.google.inject.Singleton
import java.nio.file.Files
import java.nio.file.Paths

/**
 * Singleton service class for initializing the root folder for the language server.
 * Also ensures that the resource with the built-in, native arithmetics exists in the root folder.
 * 
 * @author akos.kitta
 */
@Singleton
class RootPathProviderService implements Supplier<String> {
	
	private Supplier<String> delegate = Suppliers.memoize([
		val tempRoot = Paths.get(StandardSystemProperty.JAVA_IO_TMPDIR.value);
		val temp = Files.createTempDirectory(tempRoot, 'sadlRoot');
		return temp.toFile.absolutePath;
	]);

	@Override
	override get() {
		return delegate.get();
	}
	
}