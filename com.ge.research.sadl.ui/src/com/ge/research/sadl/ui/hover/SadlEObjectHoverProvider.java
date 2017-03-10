package com.ge.research.sadl.ui.hover;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.ui.editor.hover.html.DefaultEObjectHoverProvider;

public class SadlEObjectHoverProvider extends DefaultEObjectHoverProvider {
 
	@Override
    protected String getFirstLine(EObject o) {
//		System.out.println("EObject of hover: " + o.toString());
        return super.getFirstLine(o);
    }

}
