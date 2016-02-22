package com.ge.research.sadl.ui.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.xtext.ui.editor.preferences.LanguageRootPreferencePage;
import org.eclipse.xtext.ui.editor.preferences.fields.LabelFieldEditor;

import com.ge.research.sadl.preferences.SadlPreferences;

public class SadlRootPreferencePage extends LanguageRootPreferencePage {
	@SuppressWarnings("restriction")
	@Override
    protected void createFieldEditors() {
        addField(new LabelFieldEditor("General SADL Settings", getFieldEditorParent()));
        addField(new StringFieldEditor(SadlPreferences.SADL_BASE_URI.getId(), "Base URI", getFieldEditorParent()));
        addField(new RadioGroupFieldEditor("OWL_Format", "Saved OWL model format :", 5, 
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
        addField(new BooleanFieldEditor(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId(), "Show Namespaces in Query Results", getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.SHOW_TIMING_INFORMATION.getId(), "Show Timing Informaton (Build, Reasoning)", getFieldEditorParent()));
        addField(new RadioGroupFieldEditor("dmyOrder", "Interpret Date 10/11/2012 as:", 2, 
        		new String[][] {{"MM/DD/YYYY", SadlPreferences.DMY_ORDER_MDY.getId()}, 
        						{"DD/MM/YYYY", SadlPreferences.DMY_ORDER_DMY.getId()}}, getFieldEditorParent()));
        addField(new BooleanFieldEditor(SadlPreferences.DEEP_VALIDATION_OFF.getId(), "Disable Deep Validation of Model", getFieldEditorParent()));
        addField(new StringFieldEditor(SadlPreferences.GRAPH_VIZ_PATH.getId(), "GraphViz bin folder", getFieldEditorParent()));
    }

}
