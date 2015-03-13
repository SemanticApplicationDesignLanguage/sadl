package com.ge.research.sadl.resource;

import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.EObjectDescription;

import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.Model;
import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;

/**
 * For instances of {@link Model} a comma-separated String of imported URIs are stored
 * in the user data map with key {@link #IMPORT_KEY}.
 * @author thoms
 */
public class SadlEObjectDescription extends EObjectDescription {
	private static final Joiner JOINER = Joiner.on(',');
	public static final String IMPORT_KEY = "import";
	private static final Function<Import,String> IMPORT_TO_URI = new Function<Import,String>() {
		@Override
		public String apply(Import input) {
			return input.getImportURI();
		}
	};

	public SadlEObjectDescription(QualifiedName qualifiedName, EObject element,
			Map<String, String> userData) {
		super(qualifiedName, element, userData);
		Resource resource = element.eResource();
		handleImport(userData, resource, element);
	}

	private void handleImport(Map<String, String> userData, Resource resource,
			EObject eObject) {
		if (eObject instanceof Model) {
			Iterable<String> importedURIs = Iterables.transform(((Model)eObject).getImports(), IMPORT_TO_URI);
			userData.put(IMPORT_KEY, JOINER.join(importedURIs));
		}
	}
}
