/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.external

import com.ge.research.sadl.external.ExternalEmfResourcePredicate
import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.common.base.Preconditions
import com.google.inject.Inject
import com.google.inject.Singleton
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.BiPredicate
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.util.UriExtensions

/**
 * External EMF resource that expects file URIs and uses the file-system.
 */
@Singleton
class SadlIdeExternalEmfResourcePredicate extends ExternalEmfResourcePredicate.Default {

	@Inject
	extension UriExtensions;

	@Inject
	SadlProjectHelper projectHelper;
	
	override apply(URI it) {
		Preconditions.checkArgument(file, '''Implementation error. Expected a `file` URI. Was: «it»''');
		if (!super.apply(it)) {
			return false;
		}
		val fileUri = new java.net.URI(it.toString);
		val root = projectHelper.getRoot(fileUri);
		if (root === null) {
			return false;
		}
		return Files.find(Paths.get(root), maxDepth, matcher)
			.map[Paths.get(it.toString().replaceFirst('[.][^.]+$', ''))]
			.filter[Files.isDirectory(it)]
			.map[toFile.toURI.toEmfUri]
			.anyMatch[folderUri | it.toString.startsWith(folderUri.toString)];
	}

	protected def int getMaxDepth() {
		return 20;
	}

	protected def BiPredicate<Path, BasicFileAttributes> getMatcher() {
		return [ path, attribute |
			attribute.regularFile && path.fileName.toString.endsWith(EXTERNAL_EXTENSION)
		];
	}

}
