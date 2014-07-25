/************************************************************************
 * Copyright © 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/22 20:32:50 $
 ***********************************************************************/

package com.ge.research.sadl.ui.contentassist;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.eclipse.jface.text.templates.SimpleTemplateVariableResolver;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.xtext.ui.editor.templates.XtextTemplateContextType;

import com.google.inject.Inject;

/**
 * Adds our own custom resolver to <code>XtextTemplateContextType's</code> 
 * list of preconfigured <code>TemplateVariableResolver's</code>.
  */
public class SadlTemplateContextType extends XtextTemplateContextType {

	static class XmlDateResolver extends SimpleTemplateVariableResolver {
		public static final String NAME="xmldate";
		
		public XmlDateResolver() {
			super(NAME, "current date in XML format");
		}
		
		protected String resolve(TemplateContext context) {
			Calendar currentDate = Calendar.getInstance(); 
			SimpleDateFormat formatter=  new SimpleDateFormat("yyyy-MM-dd");
			String dateNow = formatter.format(currentDate.getTime());
			return dateNow;
		}
		
		protected boolean isUnambiguous(TemplateContext context) {
			return false;
		}
	}
	
	static class XmlDateTimeResolver extends SimpleTemplateVariableResolver {
		public static final String NAME="xmldatetime";
		
		public XmlDateTimeResolver() {
			super(NAME, "current datetime in XML format");
		}
		
		protected String resolve(TemplateContext context) {
			Calendar currentDate = Calendar.getInstance(); 
			SimpleDateFormat formatter=  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			String dateNow = formatter.format(currentDate.getTime());
			return dateNow;
		}
		
		protected boolean isUnambiguous(TemplateContext context) {
			return false;
		}
	}
	
	static class XmlTimeResolver extends SimpleTemplateVariableResolver {
		public static final String NAME="xmltime";
		
		public XmlTimeResolver() {
			super(NAME, "current time in XML format");
		}
		
		protected String resolve(TemplateContext context) {
			Calendar currentDate = Calendar.getInstance(); 
			SimpleDateFormat formatter=  new SimpleDateFormat("HH:mm:ss");
			String dateNow = formatter.format(currentDate.getTime());
			return dateNow;
		}
		
		protected boolean isUnambiguous(TemplateContext context) {
			return false;
		}
	}

	@Override
	protected void addDefaultTemplateVariables() {
		super.addDefaultTemplateVariables();
		addResolver(new XmlDateResolver());
		addResolver(new XmlDateTimeResolver());
		addResolver(new XmlTimeResolver());
	}
	
    @Inject
    public void setSadlResourceNameResolver(SadlResourceNameTemplateVariableResolver resolver) {
        addResolver(resolver);
    }

}
