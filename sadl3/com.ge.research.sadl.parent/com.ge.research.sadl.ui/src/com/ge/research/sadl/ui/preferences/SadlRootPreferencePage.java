/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.preferences;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.xtext.ui.editor.preferences.LanguageRootPreferencePage;
import org.eclipse.xtext.ui.editor.preferences.fields.LabelFieldEditor;

import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManager;

public class SadlRootPreferencePage extends LanguageRootPreferencePage {
	
	@SuppressWarnings("restriction")
	@Override
    protected void createFieldEditors() {
        addField(new LabelFieldEditor("General SADL Settings", getFieldEditorParent()));
        addField(new StringFieldEditor(SadlPreferences.SADL_BASE_URI.getId(), "Base URI", getFieldEditorParent()));
        addField(new RadioGroupFieldEditor(SadlPreferences.OWL_MODEL_FORMAT.getId(), "Saved OWL model format :", 5, 
        		new String[][] {
        		{SadlPreferences.RDF_XML_ABBREV_FORMAT.getId(), SadlPreferences.RDF_XML_ABBREV_FORMAT.getId()}, 
        		{SadlPreferences.RDF_XML_FORMAT.getId(), SadlPreferences.RDF_XML_FORMAT.getId()}, 
        		{SadlPreferences.N3_FORMAT.getId(), SadlPreferences.N3_FORMAT.getId()}, 
        		{SadlPreferences.N_TRIPLE_FORMAT.getId(), SadlPreferences.N_TRIPLE_FORMAT.getId()}, 
        		{SadlPreferences.JENA_TDB.getId(), SadlPreferences.JENA_TDB.getId()},
        		}, 
        		getFieldEditorParent()));
        addField(new RadioGroupFieldEditor("importBy", "Show import model list as:", 2, 
        		new String[][] {{"Model Namespaces", SadlPreferences.MODEL_NAMESPACES.getId()}, {"SADL File Names", SadlPreferences.SADL_FILE_NAMES.getId()}}, getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.PREFIXES_ONLY_AS_NEEDED.getId(), "Show prefixes for imported concepts only when needed for disambiguation", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.VALIDATE_BEFORE_TEST.getId(), "Validate before Testing", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.TEST_WITH_KSERVER.getId(), "Test/Query with Knowledge Server", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId(), "Show Namespaces in Query Results", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.SHOW_TIMING_INFORMATION.getId(), "Show Timing Informaton (Build, Reasoning)", getFieldEditorParent()));
        addField(new RadioGroupFieldEditor("dmyOrder", "Interpret Date 10/11/2012 as:", 2, 
        		new String[][] {{"MM/DD/YYYY", SadlPreferences.DMY_ORDER_MDY.getId()}, 
        						{"DD/MM/YYYY", SadlPreferences.DMY_ORDER_DMY.getId()}}, getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.DEEP_VALIDATION_OFF.getId(), "Disable Deep Validation of Model", getFieldEditorParent()));
        addField(new StringFieldEditor(SadlPreferences.GRAPH_RENDERER_CLASS.getId(), "Graph renderer package and class", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.getId(), "Check for ambiguous names", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.DISABLE_TYPE_CHECKING.getId(), "Disable type checking of model", getFieldEditorParent()));
	    addField(new BooleanFieldEditor(SadlPreferences.IGNORE_UNITTEDQUANTITIES.getId(), "Ignore Unitted Quantities (treat as numeric only) during translation", getFieldEditorParent()));
	    addField(new BooleanFieldEditor(SadlPreferences.USE_IMPLIED_PROPERTIES_IN_TRANSLATION.getId(), "Include implied properties in translation", getFieldEditorParent()));
	    addField(new BooleanFieldEditor(SadlPreferences.ENABLE_METRICS_COLLECTION.getId(), "Enable metrics collection during project build", getFieldEditorParent()));
		addField(new FileFieldEditor(SadlPreferences.METRICS_QUERY_FILENAME.getId(), "File containing metric queries: ", getFieldEditorParent()));
    }
	
	@Override
	public boolean performOk() {
		boolean retVal = super.performOk();
		
		if (retVal && isPropertyPage()) {
			// the changes apply only to the current project
			IPreferencesService service = Platform.getPreferencesService();
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
//			if (visitor.getCurrentResource() != null) {
//				try {
//					String curResource = ResourceManager.getOwlModelsFolder(visitor.getCurrentResource().getURI());
//					IConfigurationManager cmgr = visitor.getConfigurationMgr(curResource.toString());
//					if (cmgr != null) {
//						cmgr.getModelGetter().setFormat(format);
//					}
//					String dmyOrder = service.getString("com.ge.research.sadl.Sadl", 
//							IConfigurationManager.dmyOrder, IConfigurationManager.dmyOrderMDY, null);
//					if (dmyOrder.equals(IConfigurationManager.dmyOrderDMY)) {
//						DateTimeConfig.getGlobalDefault().setDmyOrder(true);
//					}
//					else {
//						DateTimeConfig.getGlobalDefault().setDmyOrder(false);
//					}
//					if (cmgr != null && cmgr instanceof ConfigurationManagerForEditing) {
//						// TODO
//						// put date format in configuration
//						String[] itemContent = new String[1];
//						itemContent[0] = IConfigurationManager.DateFormat;
//						ConfigurationItem ci = new ConfigurationItem(itemContent);
//						NameValuePair nvp = ci.new NameValuePair(IConfigurationManager.dmyOrder, dmyOrder);
//						ci.addNameValuePair(nvp);
//						((ConfigurationManagerForEditing)cmgr).addConfiguration(ci);
//						((IConfigurationManagerForIDE)cmgr).saveConfiguration();
//					}
//				} catch (ConfigurationException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} catch (URISyntaxException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} catch (IOException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				}
//			}
		}
		else {
			// the changes apply to all projects
			IPreferencesService service = Platform.getPreferencesService();
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
			String dmyOrder = service.getString("com.ge.research.sadl.Sadl", "dmyOrder", "mdy", null);
			String[] itemContent = new String[1];
			itemContent[0] = IConfigurationManager.DateFormat;
			ConfigurationItem ci = new ConfigurationItem(itemContent);
			NameValuePair nvp = ci.new NameValuePair(IConfigurationManager.dmyOrder, dmyOrder);
			ci.addNameValuePair(nvp);
//			Enumeration<IConfigurationManagerForIDE> cmgrs = visitor.getConfigurationManagers();
//			while (cmgrs.hasMoreElements()) {
//				IConfigurationManagerForIDE cmgr = cmgrs.nextElement();
//				cmgr.getModelGetter().setFormat(format);
//				if (dmyOrder.equals(IConfigurationManager.dmyOrderDMY)) {
//					DateTimeConfig.getGlobalDefault().setDmyOrder(true);
//				}
//				else {
//					DateTimeConfig.getGlobalDefault().setDmyOrder(false);
//				}
//				if (cmgr != null && cmgr instanceof IConfigurationManagerForIDE) {
//					// put date format in configuration
//					try {
//						((IConfigurationManagerForIDE)cmgr).addConfiguration(ci);
//						((IConfigurationManagerForIDE)cmgr).saveConfiguration();
//					} catch (ConfigurationException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					}
//				}
//			}
		}
		return retVal;
	}

}
