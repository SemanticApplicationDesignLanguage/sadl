/************************************************************************
 * Copyright 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.processing

import com.google.inject.ImplementedBy
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.util.ServiceLoader
import org.eclipse.core.runtime.Platform

import static com.ge.research.sadl.processing.ISadlImplicitModelFragmentProvider.*
import static com.google.common.base.Preconditions.*
import static java.lang.System.lineSeparator

import static extension com.google.common.base.Strings.repeat
import static extension org.eclipse.xtext.util.Files.writeStringIntoFile

/**
 * Representation of an implicit model content provider for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
@ImplementedBy(ISadlImplicitModelContentProvider.Default)
interface ISadlImplicitModelContentProvider {

	/**
	 * Returns with the content of the implicit model that has to be created on demand.
	 */
	def String getContent();

	/**
	 * Writes the SADL implicit model {@link #getContent() content} into the file
	 * under the given resource path. If a resource with the given path exists, then
	 * the existing file will be deleted and a new will be created instead.
	 * 
	 * @param desiredPath
	 *            the path to the desired implicit model resource.
	 * @param setReadOnly
	 *            when {@code true} then the underlying code tries to set write
	 *            protection on the file by calling
	 *            {@link java.io.File#setReadOnly()} on the file.
	 * @return the argument.
	 */
	def Path createImplicitModel(Path desiredPath, boolean setReadOnly) {
		checkNotNull(desiredPath, 'desiredPath');
		val file = desiredPath.toFile;
		if (file.exists) {
			Files.delete(desiredPath);
		}

		val parent = desiredPath.parent;
		checkNotNull(parent, '''Missing parent folder for «desiredPath».''');
		if (!parent.toFile.exists) {
			Files.createDirectories(parent);
		}

		if (!file.exists) {
			Files.createFile(desiredPath).toString.writeStringIntoFile(content);
		}

		if (setReadOnly) {
			file.setReadOnly;
		}

		return desiredPath;
	}

	/**
	 * Sugar for {@link #createImplicitModel(Path, boolean)}.
	 */
	def File createImplicitModel(File desiredFile, boolean setReadOnly) {
		return createImplicitModel(checkNotNull(desiredFile, 'desiredFile').toPath, setReadOnly).toFile;
	}

	/**
	 * The default SADL implicit model content provider. 
	 */
	static class Default implements ISadlImplicitModelContentProvider {

		/**
		 * The base content of the SADL implicit model.
		 */
		public static val DEFAULT_CONTENT = '''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			
			impliedProperty is a type of annotation.
			expandedProperty is a type of annotation.
			UnittedQuantity is a class,
				described by ^value with values of type decimal,
				described by unit with values of type string.
		''';

		@Override
		override getContent() {
			return fragmentProviders.fold(new StringBuilder(DEFAULT_CONTENT), [ sb, provider |
				sb.append(lineSeparator.repeat(2)).append(provider.fragmentToAppend)
			]).toString;
		}

		private def Iterable<ISadlImplicitModelFragmentProvider> getFragmentProviders() {
			return if (Platform.running) {
				Platform.extensionRegistry.getConfigurationElementsFor(EXTENSION_POINT_ID).map [
					createExecutableExtension(EXECUTABLE_ATTRIBUTE_ID) as ISadlImplicitModelFragmentProvider
				];
			} else
				{
					ServiceLoader.load(ISadlImplicitModelFragmentProvider).iterator.toIterable;
				}.map[afterCreation; it];
		}

	}

}
