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
 * $Revision: 1.4 $ Last modified on   $Date: 2014/06/12 14:49:43 $
 ***********************************************************************/

package com.ge.research.sadl.ui.properties;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Enumeration;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.xtext.ui.editor.preferences.LanguageRootPreferencePage;
import org.eclipse.xtext.ui.editor.preferences.fields.LabelFieldEditor;
import org.pojava.datetime.DateTimeConfig;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.google.inject.Inject;

/**
 * Builds the SADL root preference page. 
 */
public class SadlRootPreferencePage extends LanguageRootPreferencePage  {
	
    @Inject
    private SadlModelManager visitor;

    @Override
    protected void createFieldEditors() {
        addField(new LabelFieldEditor("General SADL Settings", getFieldEditorParent()));
        addField(new StringFieldEditor("baseUri", "Base URI", getFieldEditorParent()));
//        addField(new RadioGroupFieldEditor("OWL_Format", "Saved OWL model format :", 6, 
        addField(new RadioGroupFieldEditor("OWL_Format", "Saved OWL model format :", 5, 
        		new String[][] {
        		{ConfigurationManager.RDF_XML_ABBREV_FORMAT, ConfigurationManager.RDF_XML_ABBREV_FORMAT}, 
        		{ConfigurationManager.RDF_XML_FORMAT, ConfigurationManager.RDF_XML_FORMAT}, 
        		{ConfigurationManager.N3_FORMAT, ConfigurationManager.N3_FORMAT}, 
        		{ConfigurationManager.N_TRIPLE_FORMAT, ConfigurationManager.N_TRIPLE_FORMAT}, 
        		{ConfigurationManager.JENA_TDB, ConfigurationManager.JENA_TDB},
//        		{ConfigurationManager.OWL_Func_SWRL, ConfigurationManager.OWL_Func_SWRL}
        		}, 
        		getFieldEditorParent()));
        addField(new RadioGroupFieldEditor("importBy", "Show import model list as:", 2, 
        		new String[][] {{"Model Namespaces", "ns"}, {"SADL File Names", "fn"}}, getFieldEditorParent()));
        addField(new BooleanFieldEditor("prefixesOnlyAsNeeded", "Show prefixes for imported concepts only when needed for disambiguation", getFieldEditorParent()));
        addField(new BooleanFieldEditor("validateBeforeTest", "Validate before Testing", getFieldEditorParent()));
        addField(new BooleanFieldEditor("namespacesInQueryResults", "Show Namespaces in Query Results", getFieldEditorParent()));
        addField(new BooleanFieldEditor("showTimingInformation", "Show Timing Informaton (Build, Reasoning)", getFieldEditorParent()));
        addField(new RadioGroupFieldEditor(IConfigurationManager.dmyOrder, "Interpret Date 10/11/2012 as:", 2, 
        		new String[][] {{"MM/DD/YYYY", IConfigurationManager.dmyOrderMDY}, 
        						{"DD/MM/YYYY", IConfigurationManager.dmyOrderDMY}}, getFieldEditorParent()));
        addField(new BooleanFieldEditor("deepValidationOff", "Disable Deep Validation of Model", getFieldEditorParent()));
        addField(new StringFieldEditor("graphvizpath", "GraphViz bin folder", getFieldEditorParent()));
    }

	@Override
	public boolean performOk() {
		boolean retVal = super.performOk();
		if (retVal && isPropertyPage()) {
			// the changes apply only to the current project
			IPreferencesService service = Platform.getPreferencesService();
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
			if (visitor.getCurrentResource() != null) {
				try {
					String curResource = ResourceManager.getOwlModelsFolder(visitor.getCurrentResource().getURI());
					IConfigurationManager cmgr = visitor.getConfigurationMgr(curResource.toString());
					if (cmgr != null) {
						cmgr.getModelGetter().setFormat(format);
					}
					String dmyOrder = service.getString("com.ge.research.sadl.Sadl", 
							IConfigurationManager.dmyOrder, IConfigurationManager.dmyOrderMDY, null);
					if (dmyOrder.equals(IConfigurationManager.dmyOrderDMY)) {
						DateTimeConfig.getGlobalDefault().setDmyOrder(true);
					}
					else {
						DateTimeConfig.getGlobalDefault().setDmyOrder(false);
					}
					if (cmgr != null && cmgr instanceof ConfigurationManagerForEditing) {
						// TODO
						// put date format in configuration
						String[] itemContent = new String[1];
						itemContent[0] = IConfigurationManager.DateFormat;
						NameValuePair nvp = new NameValuePair(IConfigurationManager.dmyOrder, dmyOrder);
						ConfigurationItem ci = new ConfigurationItem(itemContent);
						ci.addNameValuePair(nvp);
						((ConfigurationManagerForEditing)cmgr).addConfiguration(ci);
						((ConfigurationManagerForIDE)cmgr).saveConfiguration();
					}
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		else {
			// the changes apply to all projects
			IPreferencesService service = Platform.getPreferencesService();
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
			String dmyOrder = service.getString("com.ge.research.sadl.Sadl", "dmyOrder", "mdy", null);
			String[] itemContent = new String[1];
			itemContent[0] = IConfigurationManager.DateFormat;
			NameValuePair nvp = new NameValuePair(IConfigurationManager.dmyOrder, dmyOrder);
			ConfigurationItem ci = new ConfigurationItem(itemContent);
			ci.addNameValuePair(nvp);
			Enumeration<ConfigurationManagerForIDE> cmgrs = visitor.getConfigurationManagers();
			while (cmgrs.hasMoreElements()) {
				ConfigurationManagerForIDE cmgr = cmgrs.nextElement();
				cmgr.getModelGetter().setFormat(format);
				if (dmyOrder.equals(IConfigurationManager.dmyOrderDMY)) {
					DateTimeConfig.getGlobalDefault().setDmyOrder(true);
				}
				else {
					DateTimeConfig.getGlobalDefault().setDmyOrder(false);
				}
				if (cmgr != null && cmgr instanceof ConfigurationManagerForIDE) {
					// put date format in configuration
					try {
						((ConfigurationManagerForIDE)cmgr).addConfiguration(ci);
						((ConfigurationManagerForIDE)cmgr).saveConfiguration();
					} catch (ConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		}
		return retVal;
	}
	
}
