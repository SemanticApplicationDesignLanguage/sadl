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
package com.ge.research.sadl.utils

import com.google.common.base.Preconditions
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.List
import javax.xml.parsers.DocumentBuilderFactory

/**
 * Helper class for validating and locating SADL projects on the file system.
 * This class does not expect a running Eclipse platform.
 */
class SadlIdeProjectHelper implements SadlProjectHelper {

	/**
	 * {@code true} if the URI represents a SADL project root. Otherwise, {@code false}.
	 * A project is considered to be a SADL project if it contains the {@code OwlModels} folder. 
	 * 
	 * A location is considered to be a SADL project root if one of the followings is {@code true}:
	 * <p>
	 * <ul>
	 * <li>The location contains the {@code OwlModels} folder.</li>
	 * <li>The location contains the {@code .project} file.</li>
	 * <li>The location contains the {@code .sadl_project} file.</li>
	 * </ul>
	 * </p>
	 */
	override isRoot(URI uri) {
		uri.checkUri
		val path = Paths.get(uri);
		if (path === null || !Files.exists(path)) {
			return false;
		}
		return path.resolve(ResourceManager.OWLDIR).toFile.directory || path.resolve('.project').toFile.file;
	}

	/**
	 * Returns with the container SADL project for the nested location. If the nested location is not contained in a SADL project, this
	 * method returns with {@code null}.
	 * 
	 * This method does not magic, but recursively traverses up the file hierarchy and checks whether the parent of the nested location is a SADL project
	 * root or not.
	 */
	override getRoot(URI nested) {
		nested.checkUri;
		if (nested.isRoot) {
			return Paths.get(nested).toFile.toURI;
		}
		val path = Paths.get(nested);
		val parent = path.parent;
		if (parent === null || parent == path) {
			return null;
		}
		return getRoot(parent.toFile.toURI);
	}

	override toPath(URI uri) {
		uri.checkUri;
		return Paths.get(uri);
	}

	/**
	 * Checks the URI and returns with the argument.
	 */
	protected def URI checkUri(URI uri) {
		Preconditions.checkNotNull(uri, 'uri');
		Preconditions.checkArgument('file'.equalsIgnoreCase(uri.scheme), '''Expected a `file` URI. Got instead: «uri».''')
		return uri;
	}

	override getReferencedProjectURIs(URI uri) {
		val rootUri = uri.getRoot
		val rootPath = rootUri.toPath
		val dotProjectPath = rootPath.resolve(DOT_PROJECT)
		val projectNames = new ReferencedProjectsParser().parse(dotProjectPath)
		projectNames.map[rootPath.resolve(it)].map[toFile].filter[exists && directory && canRead].map[toURI].toList
	}

}

class ReferencedProjectsParser {

	def List<String> parse(Path path) {
		var InputStream is = null
		try {
			is = path.toUri.toURL.openStream
			return parse(is)
		} finally {
			if (is !== null) {
				is.close
			}
		}
	}

	def List<String> parse(CharSequence content) {
		return parse(new ByteArrayInputStream(content.toString().getBytes(StandardCharsets.UTF_8)))
	}

	def List<String> parse(InputStream is) {
		try {
			val projectNames = newLinkedHashSet;
			val factory = DocumentBuilderFactory.newInstance()
			val builder = factory.newDocumentBuilder()
			val doc = builder.parse(is)
			doc.getDocumentElement().normalize()
			val projectNode = doc.getElementsByTagName('project')
			for (var i = 0; i < projectNode.length; i++) {
				val project = projectNode.item(i)
				val children = project.childNodes
				for (var j = 0; j < children.length; j++) {
					val projectName = children.item(j).nodeValue
					if (!projectName.nullOrEmpty) {
						projectNames.add(projectName)
					}
				}
			}
			return projectNames.toList
		} catch (Exception e) {
			e.printStackTrace
			return newArrayList
		}
	}
}
