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
package com.ge.research.sadl.external

import com.google.common.base.Optional
import com.google.inject.Singleton
import java.io.FileInputStream
import java.io.InputStream
import java.nio.file.Path
import javax.xml.parsers.DocumentBuilderFactory
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.util.StringInputStream
import org.eclipse.xtext.util.internal.Log
import org.w3c.dom.Attr
import org.w3c.dom.Node

/**
 * Helper that tries to read the {@code base} URO of the ontology model, if the content
 * is given as an XML.
 * 
 * <p>
 * This is just a fallback logic if the {@code base} cannot be extracted from the 
 * {@code xmlns="http://prefix/Something"} construct. It tries to locate the
 * {@code xml:base="http://prefix/Something"} construct. 
 */
@Log
@Singleton
class XMLHelper {

	static val BASE = 'xml:base';

	static val IAcceptor<Node> ACCEPTOR = [ node |
		if (node !== null && node.attributes !== null) {
			(0 .. node.attributes.length).forEach [ index |
				val attribiute = node.attributes.item(index);
				if (attribiute instanceof Attr) {
					if (BASE == attribiute.nodeName) {
						val value = (attribiute.value ?: '').trim;
						val base = if(value.endsWith('#')) value.substring(0, value.length - 1) else value;
						throw new BaseUriFoundException(base);
					}
				}
			];
		}
	];

	def dispatch Optional<String> tryReadBaseUri(InputStream is) {
		try {
			val factory = DocumentBuilderFactory.newInstance();
			val builder = factory.newDocumentBuilder();
			val doc = builder.parse(is);
			visitNode(doc, 0, ACCEPTOR);
			return Optional.absent;
		} catch (BaseUriFoundException e) {
			return Optional.of(e.baseUri);
		} catch (Exception e) {
			e.printStackTrace
			// For instance when the input is not an XML at all.
			return Optional.absent;
		}
	}

	def dispatch Optional<String> tryReadBaseUri(String string) {
		return new StringInputStream(string).tryReadBaseUri;
	}

	def dispatch Optional<String> tryReadBaseUri(Path path) {
		var FileInputStream fis = null;
		try {
			fis = new FileInputStream(path.toFile);
			return fis.tryReadBaseUri;
		} catch (Exception e) {
			LOG.error('''Error when trying to locate base URL in file: «path»''', e);
			return Optional.absent;
		} finally {
			if (fis !== null) {
				fis.close;
			}
		}
	}

	def private void visitNode(Node node, int level, extension IAcceptor<Node> acceptor) {
		accept(node);
		val children = node.childNodes;
		(0 .. children.length).forEach [ index |
			val childNode = children.item(index);
			visitNode(childNode, level + 1, acceptor);
		]
	}

	@Data
	private static final class BaseUriFoundException extends RuntimeException {
		val String baseUri;
	}

}