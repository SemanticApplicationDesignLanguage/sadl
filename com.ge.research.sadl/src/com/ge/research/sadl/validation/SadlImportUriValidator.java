package com.ge.research.sadl.validation;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ImportUriValidator;

import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ModelName;

public class SadlImportUriValidator extends ImportUriValidator {

	@Check(value=CheckType.FAST)
	public void checkImportUriIsValid(EObject object) {
		String importURI = getResolver().resolve(object);
//		if ((object instanceof Model) || (object instanceof ModelName) || (object instanceof Import)) {
		if (object instanceof Import) {
			if (importURI == null) {
				error("Imported resource could not be found.", getResolver().getAttribute(object));
			}
		}
		else {
			super.checkImportUriIsValid(object);
		}
		
	}

}
